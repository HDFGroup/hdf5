!
! This file contains Fortran90 interfaces for H5P functions.
!
     MODULE H5P

       USE H5FORTRAN_TYPES
       USE H5FORTRAN_FLAGS

       INTERFACE h5pset_fill_value_f
         MODULE PROCEDURE h5pset_fill_value_integer
         MODULE PROCEDURE h5pset_fill_value_real
! Comment if on T3E
         MODULE PROCEDURE h5pset_fill_value_double
! End comment if on T3E
         MODULE PROCEDURE h5pset_fill_value_char
       END INTERFACE
     
       INTERFACE h5pget_fill_value_f
         MODULE PROCEDURE h5pget_fill_value_integer
         MODULE PROCEDURE h5pget_fill_value_real
! Comment if on T3E
         MODULE PROCEDURE h5pget_fill_value_double
! End comment if on T3E
         MODULE PROCEDURE h5pget_fill_value_char
       END INTERFACE

     CONTAINS

          SUBROUTINE h5pcreate_f(classtype, prp_id, hdferr) 
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: classtype  ! The type of the property list 
                                              ! to be created. Possible values
                                              ! are: 
                                              !  H5P_FILE_CREATE_F (0)
                                              !  H5P_FILE_ACCESS_F (1)
                                              !  H5P_DATASET_CREATE_F (2)
                                              !  H5P_DATASET_XFER_F (3)
                                              !  H5P_MOUNT_F (4)
            INTEGER(HID_T), INTENT(OUT) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5pcreate_c
            hdferr = h5pcreate_c(classtype, prp_id) 
          END SUBROUTINE h5pcreate_f


          SUBROUTINE h5pset_preserve_f(prp_id, flag, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) ::  flag ! TRUE/FALSE flag to set the dataset
                                         ! transfer property for partila writing/reading
                                         ! compound datatype 
            INTEGER, INTENT(OUT) :: hdferr    ! Error code
            INTEGER, EXTERNAL :: h5pset_preserve_c
            hdferr = h5pset_preserve_c(prp_id, flag) 
          END SUBROUTINE h5pset_preserve_f

          SUBROUTINE h5pget_preserve_f(prp_id, flag, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) ::  flag ! TRUE/FALSE flag. Shows status of the dataset's
                                         ! transfer property for partial writing/reading
                                         ! compound datatype 
            INTEGER, INTENT(OUT) :: hdferr    ! Error code
            INTEGER, EXTERNAL :: h5pget_preserve_c
            hdferr = h5pget_preserve_c(prp_id, flag) 
          END SUBROUTINE h5pget_preserve_f

          SUBROUTINE h5pget_class_f(prp_id, classtype, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: classtype  ! The type of the property list 
                                              ! to be created. Possible values
                                              ! are: 
                                              !  H5P_NO_CLASS (-1) 
                                              !  H5P_FILE_CREATE_F (0)
                                              !  H5P_FILE_ACCESS_F (1)
                                              !  H5PE_DATASET_CREATE_F (2)
                                              !  H5P_DATASET_XFER_F (3)
                                              !  H5P_MOUNT_F (4)
            INTEGER, INTENT(OUT) :: hdferr    ! Error code
            INTEGER, EXTERNAL :: h5pget_class_c
            hdferr = h5pget_class_c(prp_id, classtype) 
          END SUBROUTINE h5pget_class_f


          SUBROUTINE h5pcopy_f(prp_id, new_prp_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(OUT) :: new_prp_id 
                                                ! Identifier  of property list
                                                ! copy  
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER, EXTERNAL :: h5pcopy_c
            hdferr = h5pcopy_c(prp_id, new_prp_id)
          END SUBROUTINE h5pcopy_f


          SUBROUTINE h5pclose_f(prp_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5pclose_c
            hdferr = h5pclose_c(prp_id)
          END SUBROUTINE h5pclose_f


          SUBROUTINE h5pset_chunk_f(prp_id, ndims, dims, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: ndims    ! Number of chunk dimensions
            INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(IN) :: dims    
                                            ! Array containing sizes of
                                            ! chunk dimensions
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_chunk_c
            hdferr =  h5pset_chunk_c(prp_id, ndims, dims)
          END SUBROUTINE h5pset_chunk_f


          SUBROUTINE h5pget_chunk_f(prp_id, ndims, dims, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: ndims    ! Number of chunk dimensions to
                                            ! to return
            INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(OUT) :: dims    
                                            ! Array containing sizes of
                                            ! chunk dimensions
            INTEGER, INTENT(OUT) :: hdferr  ! Error code; number of
                                            ! chunk dimensions on success,
                                            ! -1 on failure
            INTEGER, EXTERNAL :: h5pget_chunk_c
            hdferr =  h5pget_chunk_c(prp_id, ndims, dims)
          END SUBROUTINE h5pget_chunk_f


          SUBROUTINE h5pset_deflate_f(prp_id, level, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: level        ! Compression level 
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER, EXTERNAL :: h5pset_deflate_c
            hdferr = h5pset_deflate_c(prp_id, level)
          END SUBROUTINE h5pset_deflate_f


          SUBROUTINE h5pset_fill_value_integer(prp_id, type_id, fillvalue, &
                                               hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype 
                                                  ! (in memory)
            INTEGER, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_fill_value_c
            hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pset_fill_value_integer


          SUBROUTINE h5pget_fill_value_integer(prp_id, type_id, fillvalue, &
                                               hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype
                                                  ! (in memory) 
            INTEGER, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_fill_value_c
            hdferr = h5pget_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pget_fill_value_integer


          SUBROUTINE h5pset_fill_value_real(prp_id, type_id, fillvalue, &
                                               hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype 
                                                  ! (in memory)
            REAL, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_fill_value_c
            hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pset_fill_value_real


          SUBROUTINE h5pget_fill_value_real(prp_id, type_id, fillvalue, &
                                               hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype
                                                  ! (in memory) 
            REAL, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_fill_value_c
            hdferr = h5pget_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pget_fill_value_real


          SUBROUTINE h5pset_fill_value_double(prp_id, type_id, fillvalue, &
                                               hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype 
                                                  ! (in memory)
            DOUBLE PRECISION, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_fill_value_c
            hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pset_fill_value_double


          SUBROUTINE h5pget_fill_value_double(prp_id, type_id, fillvalue, &
                                               hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype
                                                  ! (in memory) 
            DOUBLE PRECISION, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_fill_value_c
            hdferr = h5pget_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pget_fill_value_double


          SUBROUTINE h5pset_fill_value_char(prp_id, type_id, fillvalue, &
                                               hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype 
                                                  ! (in memory)
            CHARACTER, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_fill_valuec_c
            hdferr = h5pset_fill_valuec_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pset_fill_value_char


          SUBROUTINE h5pget_fill_value_char(prp_id, type_id, fillvalue, &
                                               hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype
                                                  ! (in memory) 
            CHARACTER, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_fill_valuec_c
            hdferr = h5pget_fill_valuec_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pget_fill_value_char

          SUBROUTINE h5pget_version_f(prp_id, boot, freelist, &
                                    stab, shhdr, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, DIMENSION(:), INTENT(OUT) :: boot  !array to put boot
                                                        !block version number
            INTEGER, DIMENSION(:), INTENT(OUT) :: freelist  !array to put global
                                                        !freelist version number
     
            INTEGER, DIMENSION(:), INTENT(OUT) :: stab  !array to put symbol
                                                        !table version number
            INTEGER, DIMENSION(:), INTENT(OUT) :: shhdr !array to put shared
                                                        !object header version number
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_version_c
            hdferr = h5pget_version_c(prp_id, boot, freelist, stab, shhdr)
          END SUBROUTINE h5pget_version_f
 
          SUBROUTINE h5pset_userblock_f (prp_id, size, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(IN) :: size !Size of the user-block in bytes 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_userblock_c
            hdferr = h5pset_userblock_c(prp_id, size)
          END SUBROUTINE h5pset_userblock_f


          SUBROUTINE h5pget_userblock_f(prp_id, block_size, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), DIMENSION(:), INTENT(OUT) ::  block_size !Size of the 
                                                               !user-block in bytes 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_userblock_c
            INTEGER :: len
            hdferr = h5pget_userblock_c(prp_id,  block_size)
          END SUBROUTINE h5pget_userblock_f

          SUBROUTINE h5pset_sizes_f (prp_id, sizeof_addr, sizeof_size, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(SIZE_T), INTENT(IN) :: sizeof_addr !Size of an object 
                                                       !offset in bytes 
            INTEGER(SIZE_T), INTENT(IN) :: sizeof_size !Size of an object 
                                                       !length in bytes 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_sizes_c
            hdferr = h5pset_sizes_c(prp_id, sizeof_addr, sizeof_size)
          END SUBROUTINE h5pset_sizes_f


          SUBROUTINE h5pget_sizes_f(prp_id, sizeof_addr, sizeof_size, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(SIZE_T), DIMENSION(:), INTENT(OUT) :: sizeof_addr !Size of an object
                                                                      !offset in bytes 
            INTEGER(SIZE_T), DIMENSION(:), INTENT(OUT) :: sizeof_size !Size of an object
                                                                      !length in bytes 
  
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_sizes_c
            hdferr = h5pget_sizes_c(prp_id, sizeof_addr, sizeof_size)
          END SUBROUTINE h5pget_sizes_f

          SUBROUTINE h5pset_sym_k_f (prp_id, ik, lk, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: ik ! Symbol table tree rank 
            INTEGER, INTENT(IN) :: lk ! Symbol table node size 
                                                       
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_sym_k_c
            hdferr = h5pset_sym_k_c(prp_id, ik, lk)
          END SUBROUTINE h5pset_sym_k_f


          SUBROUTINE h5pget_sym_k_f(prp_id, ik, lk, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: ik !Symbol table tree rank
            INTEGER, INTENT(OUT) :: lk !Symbol table node size
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_sym_k_c
            hdferr = h5pget_sym_k_c(prp_id, ik, lk)
          END SUBROUTINE h5pget_sym_k_f

          SUBROUTINE h5pset_istore_k_f (prp_id, ik, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: ik ! 1/2 rank of chunked storage B-tree
                                                       
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_istore_k_c
            hdferr = h5pset_istore_k_c(prp_id, ik)
          END SUBROUTINE h5pset_istore_k_f


          SUBROUTINE h5pget_istore_k_f(prp_id, ik, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: ik !1/2 rank of chunked storage B-tree
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_istore_k_c
            hdferr = h5pget_istore_k_c(prp_id, ik)
          END SUBROUTINE h5pget_istore_k_f

          SUBROUTINE h5pget_driver_f(prp_id, driver, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: driver !low-level file driver identifier
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_driver_c
            hdferr = h5pget_driver_c(prp_id, driver)
          END SUBROUTINE h5pget_driver_f

          SUBROUTINE h5pset_stdio_f (prp_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_stdio_c
            hdferr = h5pset_stdio_c(prp_id)
          END SUBROUTINE h5pset_stdio_f

          SUBROUTINE h5pget_stdio_f (prp_id, io, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: io   ! value indicates that the file 
                                         !access property list is set to 
                                         !the stdio driver
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_stdio_c
            hdferr = h5pget_stdio_c(prp_id, io)
          END SUBROUTINE h5pget_stdio_f

          SUBROUTINE h5pset_sec2_f (prp_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_sec2_c
            hdferr = h5pset_sec2_c(prp_id)
          END SUBROUTINE h5pset_sec2_f

          SUBROUTINE h5pget_sec2_f (prp_id, sec2, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: sec2   ! value indicates whether the file 
                                           !driver uses the functions declared
                                           !in the unistd.h file
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_sec2_c
            hdferr = h5pget_sec2_c(prp_id, sec2)
          END SUBROUTINE h5pget_sec2_f

          SUBROUTINE h5pset_alignment_f(prp_id, threshold,  alignment, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(IN) :: threshold ! Threshold value
            INTEGER(HSIZE_T), INTENT(IN) :: alignment ! alignment value
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_alignment_c
            hdferr = h5pset_alignment_c(prp_id, threshold, alignment)
          END SUBROUTINE h5pset_alignment_f

          SUBROUTINE h5pget_alignment_f(prp_id, threshold,  alignment, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(OUT) :: threshold ! Threshold value
            INTEGER(HSIZE_T), INTENT(OUT) :: alignment ! alignment value
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_alignment_c
            hdferr = h5pget_alignment_c(prp_id, threshold, alignment)
          END SUBROUTINE h5pget_alignment_f

          SUBROUTINE h5pset_core_f(prp_id, increment, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(SIZE_T), INTENT(IN) :: increment ! File block size in bytes.
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_core_c
            hdferr = h5pset_core_c(prp_id, increment)
          END SUBROUTINE h5pset_core_f

          SUBROUTINE h5pget_core_f(prp_id, increment, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(SIZE_T), INTENT(OUT) :: increment ! File block size in bytes.
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_core_c
            hdferr = h5pget_core_c(prp_id, increment)
          END SUBROUTINE h5pget_core_f

          SUBROUTINE h5pset_family_f(prp_id, memb_size, memb_plist , hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(IN) :: memb_size ! Logical size, in bytes,
                                                      !of each family member
            INTEGER(HID_T), INTENT(IN) :: memb_plist !Identifier of the file 
                                                     !access property list for 
                                                     !each member of the family
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_family_c
            hdferr = h5pset_family_c(prp_id, memb_size, memb_plist)
          END SUBROUTINE h5pset_family_f


          SUBROUTINE h5pget_family_f(prp_id, memb_size, memb_plist , hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(OUT) :: memb_size ! Logical size, in bytes,
                                                      !of each family member
            INTEGER(HID_T), INTENT(OUT) :: memb_plist !Identifier of the file 
                                                     !access property list for 
                                                     !each member of the family
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_family_c
            hdferr = h5pget_family_c(prp_id, memb_size, memb_plist)
          END SUBROUTINE h5pget_family_f

          SUBROUTINE h5pset_cache_f(prp_id, mdc_nelmts,rdcc_nelmts, rdcc_nbytes, rdcc_w0, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: mdc_nelmts  !Number of elements (objects)
                                                        ! in the meta data cache
            INTEGER, INTENT(IN) :: rdcc_nelmts  !Number of elements (objects)
                                                        ! in the meta data cache
            INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes !Total size of the raw data 
                                                      !chunk cache, in bytes 
            REAL, INTENT(IN) :: rdcc_w0 !Preemption policy
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pset_cache_c
            hdferr = h5pset_cache_c(prp_id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0 )
          END SUBROUTINE h5pset_cache_f

          SUBROUTINE h5pget_cache_f(prp_id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: mdc_nelmts  !Number of elements (objects)
                                                        ! in the meta data cache
            INTEGER, INTENT(OUT) :: rdcc_nelmts  !Number of elements (objects)
                                                        ! in the meta data cache
            INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes !Total size of the raw data 
                                                      !chunk cache, in bytes 
            REAL, INTENT(OUT) :: rdcc_w0 !Preemption policy
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pget_cache_c
            hdferr = h5pget_cache_c(prp_id, mdc_nelmts,rdcc_nelmts, rdcc_nbytes, rdcc_w0 )
          END SUBROUTINE h5pget_cache_f

          SUBROUTINE h5pset_split_f(prp_id, meta_ext, meta_plist, raw_ext, raw_plist, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            CHARACTER(LEN=*), INTENT(IN) :: meta_ext  !Name of the extension for
                                                      !the metafile filename
            INTEGER(HID_T), INTENT(IN) :: meta_plist  ! Identifier of the meta file
                                                      ! access property list
            CHARACTER(LEN=*), INTENT(IN) :: raw_ext  !Name extension for the raw file filename
            INTEGER(HID_T), INTENT(IN) :: raw_plist  !Identifier of the raw file 
                                                     !access property list
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER :: meta_len, raw_len;

            INTEGER, EXTERNAL :: h5pset_split_c
            meta_len = LEN(meta_ext)
            raw_len = LEN(raw_ext)
            hdferr = h5pset_split_c(prp_id, meta_len, meta_ext, meta_plist, raw_len, raw_ext, raw_plist )
          END SUBROUTINE h5pset_split_f

          SUBROUTINE h5pget_split_f(prp_id, meta_ext_size, meta_ext, meta_plist,raw_ext_size,&
                                     raw_ext, raw_plist, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER(SIZE_T), INTENT(IN) :: meta_ext_size ! Number of characters of the meta
                                                         ! file extension to be copied to the
                                                         ! meta_ext buffer
      
            CHARACTER(LEN=*), INTENT(OUT) :: meta_ext  !Name of the extension for
                                                      !the metafile filename
            INTEGER(HID_T), INTENT(OUT) :: meta_plist  ! Identifier of the meta file
                                                      ! access property list
            INTEGER(SIZE_T), INTENT(IN) :: raw_ext_size ! Number of characters of the raw
                                                         ! file extension to be copied to the
                                                         ! raw_ext buffer
            CHARACTER(LEN=*), INTENT(OUT) :: raw_ext  !Name extension for the raw file filename
            INTEGER(HID_T), INTENT(OUT) :: raw_plist  !Identifier of the raw file 
                                                     !access property list
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pget_split_c
            hdferr = h5pget_split_c(prp_id, meta_ext_size, meta_ext, meta_plist, &
                                    raw_ext_size, raw_ext, raw_plist )
          END SUBROUTINE h5pget_split_f

 
          SUBROUTINE h5pset_gc_references_f (prp_id, gc_reference, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: gc_reference !the flag for garbage collecting
                                                ! references for the file
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_gc_references_c
            hdferr = h5pset_gc_references_c(prp_id, gc_reference)
          END SUBROUTINE h5pset_gc_references_f

          SUBROUTINE h5pget_gc_references_f (prp_id, gc_reference, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: gc_reference !the flag for garbage collecting
                                                ! references for the file
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_gc_references_c
            hdferr = h5pget_gc_references_c(prp_id, gc_reference)
          END SUBROUTINE h5pget_gc_references_f

          SUBROUTINE h5pset_layout_f (prp_id, layout, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: layout !Type of storage layout for raw data
                                          !possible values are:
                                          !H5D_COMPACT_F(0)
                                          !H5D_CONTIGUOUS_F(1)
                                          !H5D_CHUNKED_F(2)
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pset_layout_c
            hdferr = h5pset_layout_c(prp_id, layout)
          END SUBROUTINE h5pset_layout_f

          SUBROUTINE h5pget_layout_f (prp_id, layout, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: layout !Type of storage layout for raw data
                                          !possible values are:
                                          !H5D_COMPACT_F(0)
                                          !H5D_CONTIGUOUS_F(1)
                                          !H5D_CHUNKED_F(2)
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_layout_c
            hdferr = h5pget_layout_c(prp_id, layout)
          END SUBROUTINE h5pget_layout_f

          SUBROUTINE h5pset_filter_f(prp_id, filter, flags, cd_nelmts, cd_values,  hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: filter  !Filter to be added to the pipeline.
            INTEGER, INTENT(IN) :: flags  !Bit vector specifying certain general
                                          !properties of the filter.
            INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts  !Number of elements in cd_values.
            INTEGER, DIMENSION(*), INTENT(IN) :: cd_values  !Auxiliary data for the filter.

            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pset_filter_c
            hdferr = h5pset_filter_c(prp_id, filter, flags, cd_nelmts, cd_values )
          END SUBROUTINE h5pset_filter_f

          SUBROUTINE h5pget_nfilters_f (prp_id, nfilters, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: nfilters !the number of filters in the pipeline
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_nfilters_c
            hdferr = h5pget_nfilters_c(prp_id, nfilters)
          END SUBROUTINE h5pget_nfilters_f

          SUBROUTINE h5pget_filter_f(prp_id, filter_number, flags, cd_nelmts, cd_values, namelen, name, filter_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: filter_number  !Sequence number within the filter
                                                  !pipeline of the filter for which 
                                                  !information is sought
            INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values  !Auxiliary data for the filter.
            INTEGER, INTENT(OUT) :: flags  !Bit vector specifying certain general
                                          !properties of the filter.
            INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts  !Number of elements in cd_values.
            INTEGER(SIZE_T), INTENT(IN) :: namelen !Anticipated number of characters in name.
            CHARACTER(LEN=*), INTENT(OUT) :: name !Name of the filter
            INTEGER, INTENT(OUT) :: filter_id ! filter identification number  

            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pget_filter_c
            hdferr = h5pget_filter_c(prp_id, filter_number, flags, cd_nelmts,  & 
                                     cd_values, namelen, name, filter_id )
          END SUBROUTINE h5pget_filter_f

          SUBROUTINE h5pset_external_f(prp_id, name, offset,bytes, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            CHARACTER(LEN=*), INTENT(IN) :: name !Name of an external file
            INTEGER, INTENT(IN) :: offset !Offset, in bytes, from the beginning 
                                          !of the file to the location in the file 
                                          !where the data starts.
            INTEGER(HSIZE_T), INTENT(IN) :: bytes ! Number of bytes reserved in the 
                                                 !file for the data
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pset_external_c
            INTEGER :: namelen
            namelen = LEN(name)

            hdferr = h5pset_external_c(prp_id, name,namelen, offset, bytes)
          END SUBROUTINE h5pset_external_f

          SUBROUTINE h5pget_external_count_f (prp_id, count, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: count !number of external files for the 
                                          !specified dataset
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5pget_external_count_c
            hdferr = h5pget_external_count_c(prp_id, count)
          END SUBROUTINE h5pget_external_count_f


          SUBROUTINE h5pget_external_f(prp_id, idx, name_size, name, offset,bytes, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(IN) :: idx !External file index.
            INTEGER(SIZE_T), INTENT(IN) :: name_size !Maximum length of name array 
            CHARACTER(LEN=*), INTENT(OUT) :: name !Name of an external file
            INTEGER, INTENT(OUT) :: offset !Offset, in bytes, from the beginning 
                                          !of the file to the location in the file 
                                          !where the data starts.
            INTEGER(HSIZE_T), INTENT(OUT) :: bytes ! Number of bytes reserved in the 
                                                 !file for the data
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pget_external_c

            hdferr = h5pget_external_c(prp_id, idx, name_size, name, offset, bytes)
          END SUBROUTINE h5pget_external_f

          SUBROUTINE h5pset_hyper_cache_f(prp_id, cache, limit, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(IN) :: cache !
            INTEGER, INTENT(IN) :: limit ! Maximum size of the hyperslab block to 
                                         !cache. 0 (zero) indicates no limit.
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pset_hyper_cache_c
            hdferr = h5pset_hyper_cache_c(prp_id, cache, limit)
          END SUBROUTINE h5pset_hyper_cache_f

          SUBROUTINE h5pget_hyper_cache_f(prp_id, cache, limit, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(OUT) :: cache !
            INTEGER, INTENT(OUT) :: limit ! Maximum size of the hyperslab block to 
                                         !cache. 0 (zero) indicates no limit.
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pget_hyper_cache_c
            hdferr = h5pget_hyper_cache_c(prp_id, cache, limit)
          END SUBROUTINE h5pget_hyper_cache_f

          SUBROUTINE h5pset_btree_ratios_f(prp_id, left, middle, right, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            REAL, INTENT(IN) :: left !The B-tree split ratio for left-most nodes.
            REAL, INTENT(IN) :: middle !The B-tree split ratio for all other nodes 
            REAL, INTENT(IN) :: right !The B-tree split ratio for right-most 
                                      !nodes and lone nodes. 

            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pset_btree_ratios_c
            hdferr = h5pset_btree_ratios_c(prp_id, left, middle, right)
          END SUBROUTINE h5pset_btree_ratios_f

          SUBROUTINE h5pget_btree_ratios_f(prp_id, left, middle, right, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            REAL, INTENT(OUT) :: left !The B-tree split ratio for left-most nodes.
            REAL, INTENT(OUT) :: middle !The B-tree split ratio for all other nodes 
            REAL, INTENT(OUT) :: right !The B-tree split ratio for right-most 
                                      !nodes and lone nodes. 

            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pget_btree_ratios_c
            hdferr = h5pget_btree_ratios_c(prp_id, left, middle, right)
          END SUBROUTINE h5pget_btree_ratios_f

     END MODULE H5P
