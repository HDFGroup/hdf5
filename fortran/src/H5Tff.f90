!
! This file contains FORTRAN90 interfaces for H5T functions
!
      MODULE H5T

        USE H5FORTRAN_TYPES 
        USE H5FORTRAN_FLAGS 
      
      CONTAINS

          SUBROUTINE h5topen_f(loc_id, name, type_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id  ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name  
                                  ! Datatype name within file or group
            INTEGER(HID_T), INTENT(OUT) :: type_id  ! Datatype identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER :: namelen          ! Name length 
            INTEGER, EXTERNAL :: h5topen_c
            namelen = LEN(name)
            hdferr = h5topen_c(loc_id, name, namelen, type_id)
          END SUBROUTINE h5topen_f

          SUBROUTINE h5tcommit_f(loc_id, name, type_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id  ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name  
                                  ! Datatype name within file or group
            INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER :: namelen          ! Name length 
            INTEGER, EXTERNAL :: h5tcommit_c
            namelen = LEN(name)
            hdferr = h5tcommit_c(loc_id, name, namelen, type_id)
          END SUBROUTINE h5tcommit_f


          SUBROUTINE h5tcopy_f(type_id, new_type_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(HID_T), INTENT(OUT) :: new_type_id 
                                 ! Identifier of datatype's copy 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tcopy_c
            hdferr = h5tcopy_c(type_id, new_type_id)
          END SUBROUTINE h5tcopy_f

          SUBROUTINE h5tequal_f(type1_id, type2_id, flag, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type1_id ! Datatype identifier 
            INTEGER(HID_T), INTENT(IN) :: type2_id ! Datatype identifier 
            LOGICAL, INTENT(OUT) :: flag ! TRUE/FALSE flag to indicate if two
                                         ! datatypes are equal
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: c_flag
            INTEGER, EXTERNAL :: h5tequal_c
            flag = .FALSE.
            hdferr = h5tequal_c(type1_id, type2_id, c_flag)
            if(c_flag .gt. 0) flag = .TRUE.
          END SUBROUTINE h5tequal_f


          SUBROUTINE h5tclose_f(type_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL ::  h5tclose_c
            hdferr = h5tclose_c(type_id)
          END SUBROUTINE h5tclose_f


          SUBROUTINE h5tget_class_f(type_id, class, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: class 
                           ! Datatype class, possible values are:
                                          ! H5T_NO_CLASS_F (-1)
                                          ! H5T_INTEGER_F  (0)
                                          ! H5T_FLOAT_F (1)
                                          ! H5T_TIME_F  (2)
                                          ! H5T_STRING_F (3)
                                          ! H5T_BITFIELD_F (4)
                                          ! H5T_OPAQUE_F (5)
                                          ! H5T_COMPOUND_F (6)
                                          ! H5T_REFERENCE_F (7)
                                          ! H5T_ENUM_F (8)
          INTEGER, INTENT(OUT) :: hdferr        ! Error code
          INTEGER, EXTERNAL :: h5tget_class_c
          hdferr = h5tget_class_c(type_id, class)
          END SUBROUTINE h5tget_class_f


          SUBROUTINE h5tget_size_f(type_id, size, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(SIZE_T), INTENT(OUT) :: size ! Datatype size
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_size_c
            hdferr = h5tget_size_c(type_id, size)
          END SUBROUTINE h5tget_size_f


          SUBROUTINE h5tset_size_f(type_id, size, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(SIZE_T), INTENT(IN) :: size ! Datatype size
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_size_c
            hdferr = h5tset_size_c(type_id, size)
          END SUBROUTINE h5tset_size_f


          SUBROUTINE h5tget_order_f(type_id, order, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: order 
                              ! Datatype byte order, bossible values are:
                                          ! H5T_ORDER_LE_F
                                          ! H5T_ORDER_BE_F
                                          ! H5T_ORDER_VAX_F
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_order_c
            hdferr = h5tget_order_c(type_id, order)
          END SUBROUTINE h5tget_order_f


          SUBROUTINE h5tset_order_f(type_id, order, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: order ! Datatype byte order, bossible values
                                          ! are:
                                          ! H5T_ORDER_LE_F 
                                          ! H5T_ORDER_BE_F
                                          ! H5T_ORDER_VAX_F 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_order_c
            hdferr = h5tset_order_c(type_id, order)
          END SUBROUTINE h5tset_order_f


          SUBROUTINE h5tget_precision_f(type_id, precision, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(SIZE_T), INTENT(OUT) :: precision ! Datatype precision
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_precision_c
            hdferr = h5tget_precision_c(type_id, precision)
          END SUBROUTINE h5tget_precision_f

          SUBROUTINE h5tset_precision_f(type_id, precision, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(SIZE_T), INTENT(IN) :: precision ! Datatype precision
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_precision_c
            hdferr = h5tset_precision_c(type_id, precision)
          END SUBROUTINE h5tset_precision_f

          SUBROUTINE h5tget_offset_f(type_id, offset, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(SIZE_T), INTENT(OUT) :: offset ! Datatype bit offset of the
                                           ! first significant bit
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_offset_c
            hdferr = h5tget_offset_c(type_id, offset)
          END SUBROUTINE h5tget_offset_f

          SUBROUTINE h5tset_offset_f(type_id, offset, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(SIZE_T), INTENT(IN) :: offset ! Datatype bit offset of the
                                           ! first significant bit
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_offset_c
            hdferr = h5tset_offset_c(type_id, offset)
          END SUBROUTINE h5tset_offset_f

          SUBROUTINE h5tget_pad_f(type_id, lsbpad, msbpad, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: lsbpad ! padding type of the 
                                           ! least significant bit
            INTEGER, INTENT(OUT) :: msbpad ! padding type of the 
                                           ! most significant bit
                                           ! Possible values of padding type are:
                                           ! H5T__PAD_ZERO_F = 0
                                           ! H5T__PAD_ONE_F = 1
                                           ! H5T__PAD_BACKGROUND_F = 2
                                           ! H5T_PAD_ERROR_F      = -1
                                           ! H5T_PAD_NPAD_F      = 3

            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_pad_c
            hdferr = h5tget_pad_c(type_id, lsbpad, msbpad)
          END SUBROUTINE h5tget_pad_f

          SUBROUTINE h5tset_pad_f(type_id, lsbpad, msbpad, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: lsbpad ! padding type of the 
                                           ! least significant bit
            INTEGER, INTENT(IN) :: msbpad ! padding type of the 
                                           ! most significant bit
                                           ! Possible values of padding type are:
                                           ! H5T_PAD_ZERO_F = 0
                                           ! H5T_PAD_ONE_F = 1
                                           ! H5T_PAD_BACKGROUND_F = 2
                                           ! H5T_PAD_ERROR_F      = -1
                                           ! H5T_PAD_NPAD_F      = 3
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_pad_c
            hdferr = h5tset_pad_c(type_id, lsbpad, msbpad)
          END SUBROUTINE h5tset_pad_f

          SUBROUTINE h5tget_sign_f(type_id, sign, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: sign ! sign type for an integer type
                                         !possible values are:
                                         !Unsigned integer type H5T_SGN_NONE_F = 0
                                         !Two's complement signed integer type
                                         !H5T_SGN_2_F = 1
                                         !or error value: H5T_SGN_ERROR_F=-1 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_sign_c
            hdferr = h5tget_sign_c(type_id, sign)
          END SUBROUTINE h5tget_sign_f

          SUBROUTINE h5tset_sign_f(type_id, sign, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: sign !sign type for an integer type 
                                         !possible values are:
                                         !Unsigned integer type H5T_SGN_NONE_F = 0
                                         !Two's complement signed integer type
                                         !H5T_SGN_2_F = 1
                                         !or error value: H5T_SGN_ERROR_F=-1 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_sign_c
            hdferr = h5tset_sign_c(type_id, sign)
          END SUBROUTINE h5tset_sign_f

          SUBROUTINE h5tget_fields_f(type_id, epos, esize, mpos, msize, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: epos   ! exponent bit-position 
            INTEGER, INTENT(OUT) :: esize  ! size of exponent in bits
            INTEGER, INTENT(OUT) :: mpos   ! mantissa bit-position 
            INTEGER, INTENT(OUT) :: msize  ! size of mantissa in bits
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

            INTEGER, EXTERNAL :: h5tget_fields_c
            hdferr = h5tget_fields_c(type_id, epos, esize, mpos, msize, hdferr)
          END SUBROUTINE h5tget_fields_f

          SUBROUTINE h5tset_fields_f(type_id, epos, esize, mpos, msize, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: epos   ! exponent bit-position 
            INTEGER, INTENT(IN) :: esize  ! size of exponent in bits
            INTEGER, INTENT(IN) :: mpos   ! mantissa bit-position 
            INTEGER, INTENT(IN) :: msize  ! size of mantissa in bits
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

            INTEGER, EXTERNAL :: h5tset_fields_c
            hdferr = h5tset_fields_c(type_id, epos, esize, mpos, msize)
          END SUBROUTINE h5tset_fields_f

          SUBROUTINE h5tget_ebias_f(type_id, ebias, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(SIZE_T), INTENT(OUT) :: ebias ! Datatype exponent bias of a floating-point type
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_ebias_c
            hdferr = h5tget_ebias_c(type_id, ebias)
          END SUBROUTINE h5tget_ebias_f


          SUBROUTINE h5tset_ebias_f(type_id, ebias, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER(SIZE_T), INTENT(IN) :: ebias !Datatype exponent bias of a floating-point type
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_ebias_c
            hdferr = h5tset_ebias_c(type_id, ebias)
          END SUBROUTINE h5tset_ebias_f

          SUBROUTINE h5tget_norm_f(type_id, norm, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: norm !mantissa normalization of a floating-point datatype
                                         !Valid normalization types are:
                                         !H5T_NORM_IMPLIED_F(0),MSB of mantissa is not 
                                         !stored, always 1,  H5T_NORM_MSBSET_F(1), MSB of 
                                         !mantissa is always 1, H5T_NORM_NONE_F(2)
                                         !Mantissa is not normalize
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_norm_c
            hdferr = h5tget_norm_c(type_id, norm)
          END SUBROUTINE h5tget_norm_f


          SUBROUTINE h5tset_norm_f(type_id, norm, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: norm !mantissa normalization of a floating-point datatype
                                         !Valid normalization types are:
                                         !H5T_NORM_IMPLIED_F(0),MSB of mantissa is not 
                                         !stored, always 1,  H5T_NORM_MSBSET_F(1), MSB of 
                                         !mantissa is always 1, H5T_NORM_NONE_F(2)
                                         !Mantissa is not normalize
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_norm_c
            hdferr = h5tset_norm_c(type_id, norm)
          END SUBROUTINE h5tset_norm_f

          SUBROUTINE h5tget_inpad_f(type_id, padtype, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: padtype ! padding type for unused bits 
                                            ! in floating-point datatypes.
                                            ! Possible values of padding type are:
                                            ! H5T__PAD_ZERO_F = 0
                                            ! H5T__PAD_ONE_F = 1
                                            ! H5T__PAD_BACKGROUND_F = 2

            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_inpad_c
            hdferr = h5tget_inpad_c(type_id, padtype)
          END SUBROUTINE h5tget_inpad_f

          SUBROUTINE h5tset_inpad_f(type_id, padtype, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: padtype ! padding type for unused bits 
                                           ! in floating-point datatypes.
                                           ! Possible values of padding type are:
                                           ! H5T__PAD_ZERO_F = 0
                                           ! H5T__PAD_ONE_F = 1
                                           ! H5T__PAD_BACKGROUND_F = 2
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_inpad_c
            hdferr = h5tset_inpad_c(type_id, padtype)
          END SUBROUTINE h5tset_inpad_f

          SUBROUTINE h5tget_cset_f(type_id, cset, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: cset ! character set type of a string datatype 
                                            ! Possible values of padding type are:
                                            !H5T_CSET_ASCII_F = 0
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_cset_c
            hdferr = h5tget_cset_c(type_id, cset)
          END SUBROUTINE h5tget_cset_f

          SUBROUTINE h5tset_cset_f(type_id, cset, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: cset !character set type of a string datatype  
                                           !Possible values of padding type are:
                                           !H5T_CSET_ASCII_F = 0
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_cset_c
            hdferr = h5tset_cset_c(type_id, cset)
          END SUBROUTINE h5tset_cset_f

          SUBROUTINE h5tget_strpad_f(type_id, strpad, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: strpad ! string padding method for a string datatype 
                                           ! Possible values of padding type are:
                                           !Pad with zeros (as C does): H5T_STR_NULL_F(0), 
                                           !Pad with spaces (as FORTRAN does): 
                                           !H5T_STR_SPACE_F(1)
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_strpad_c
            hdferr = h5tget_strpad_c(type_id, strpad)
          END SUBROUTINE h5tget_strpad_f

          SUBROUTINE h5tset_strpad_f(type_id, strpad, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: strpad ! string padding method for a string datatype 
                                          ! Possible values of padding type are:
                                          !Pad with zeros (as C does): H5T_STR_NULL_F(0), 
                                          !Pad with spaces (as FORTRAN does): 
                                          !H5T_STR_SPACE_F(1)
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tset_strpad_c
            hdferr = h5tset_strpad_c(type_id, strpad)
          END SUBROUTINE h5tset_strpad_f


          SUBROUTINE h5tget_nmembers_f(type_id, num_members, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(OUT) :: num_members !number of fields in a compound datatype 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_nmembers_c
            hdferr = h5tget_nmembers_c(type_id, num_members)
          END SUBROUTINE h5tget_nmembers_f

          SUBROUTINE h5tget_member_name_f(type_id,index, member_name,  namelen, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: index !Field index (0-based) of the field name to retrieve 
            CHARACTER(LEN=*), INTENT(OUT) :: member_name !name of a field of
                                                         !a compound datatype 
            INTEGER, INTENT(OUT) :: namelen ! Length the name 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_member_name_c
            hdferr = h5tget_member_name_c(type_id, index, member_name, namelen)
          END SUBROUTINE h5tget_member_name_f

          SUBROUTINE h5tget_member_offset_f(type_id, member_no, offset, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: member_no !Number of the field  
                                                       !whose offset is requested
            INTEGER(SIZE_T), INTENT(OUT) :: offset !byte offset of the the beginning of the field
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_member_offset_c
            hdferr = h5tget_member_offset_c(type_id, member_no, offset )
          END SUBROUTINE h5tget_member_offset_f

!          SUBROUTINE h5tget_member_dims_f(type_id, field_idx,dims, field_dims, perm, hdferr) 
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
!            INTEGER, INTENT(IN) :: field_idx !Field index (0-based) of 
!                                             !field_dims, perm)
!            INTEGER, INTENT(OUT) :: dims     !number of dimensions of the field
!
!            INTEGER(SIZE_T),DIMENSION(*), INTENT(OUT) ::  field_dims !buffer to store the 
!                                                                      !dimensions of the field
!            INTEGER, DIMENSION(*), INTENT(OUT)  ::  perm  !buffer to store the 
!                                                                   !permutation vector of the field
!            INTEGER, INTENT(OUT) :: hdferr        ! Error code
!            INTEGER, EXTERNAL :: h5tget_member_dims_c
!            hdferr = h5tget_member_dims_c(type_id, field_idx, dims, field_dims, perm)
!
!          END SUBROUTINE h5tget_member_dims_f

          SUBROUTINE h5tget_array_dims_f(type_id, dims, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Array datatype identifier 
            INTEGER(HSIZE_T),DIMENSION(*), INTENT(OUT) ::  dims !buffer to store array datatype
                                                                ! dimensions 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_array_dims_c
            hdferr = h5tget_array_dims_c(type_id, dims)

          END SUBROUTINE h5tget_array_dims_f

          SUBROUTINE h5tget_array_ndims_f(type_id, ndims, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Array datatype identifier 
            INTEGER, INTENT(OUT) ::  ndims ! number of array dimensions
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_array_ndims_c
            hdferr = h5tget_array_ndims_c(type_id, ndims)

          END SUBROUTINE h5tget_array_ndims_f

          SUBROUTINE h5tget_super_f(type_id, base_type_id, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! datatype identifier 
            INTEGER(HID_T), INTENT(OUT) :: base_type_id ! identifier of the datatype
                                           ! from which datatype (type_id) was derived
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_super_c
            hdferr = h5tget_super_c(type_id, base_type_id)

          END SUBROUTINE h5tget_super_f

          SUBROUTINE h5tget_member_type_f(type_id,  field_idx, datatype, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: field_idx !Field index (0-based) of the field type to retrieve
            INTEGER(HID_T), INTENT(OUT) :: datatype !identifier of a copy of 
                                                    !the datatype of the field 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_member_type_c
            hdferr = h5tget_member_type_c(type_id, field_idx , datatype)
          END SUBROUTINE h5tget_member_type_f


          SUBROUTINE h5tcreate_f(class, size, type_id, hdferr) 
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: class ! Datatype class cna be one of
                                         ! H5T_COMPOUND_F (6)
                                         ! H5T_ENUM_F     (8)
                                         ! H5T_OPAQUE_F   (9)
            INTEGER(SIZE_T), INTENT(IN) :: size ! Size of the datatype
            INTEGER(HID_T), INTENT(OUT) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tcreate_c

           hdferr = h5tcreate_c(class, size, type_id)
          END SUBROUTINE h5tcreate_f

          SUBROUTINE h5tinsert_f(type_id,  name, offset, field_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name !Name of the field to insert
            INTEGER(SIZE_T), INTENT(IN) :: offset !Offset in memory structure of the field to insert
            INTEGER(HID_T), INTENT(IN) :: field_id !datatype identifier of the new member

            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: namelen
            INTEGER, EXTERNAL :: h5tinsert_c
            namelen = LEN(name)
            hdferr = h5tinsert_c(type_id, name, namelen, offset, field_id )
          END SUBROUTINE h5tinsert_f

          SUBROUTINE h5tpack_f(type_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tpack_c
            hdferr = h5tpack_c(type_id) 
          END SUBROUTINE h5tpack_f

!          SUBROUTINE h5tinsert_array_f(parent_id,name,offset, ndims, dims, member_id, hdferr, perm) 
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: parent_id ! identifier of the parent compound datatype
!            CHARACTER(LEN=*), INTENT(IN) :: name !Name of the new member
!            INTEGER(SIZE_T), INTENT(IN) :: offset !Offset to start of new member 
!                                                   !within compound datatype
!            INTEGER, INTENT(IN) ::  ndims !Dimensionality of new member. 
!                                          !Valid values are 0 (zero) through 4 (four)
!            INTEGER(SIZE_T), DIMENSION(*), INTENT(IN) :: dims !Size of new member array
!            INTEGER(HID_T), INTENT(IN) :: member_id ! identifier of the datatype of the new member
!            INTEGER, INTENT(OUT) :: hdferr        ! Error code
!
!            INTEGER, DIMENSION(*), OPTIONAL, INTENT(IN) :: perm 
!                                                               !Pointer to buffer to store 
!                                                               !the permutation vector of the field
!            INTEGER :: namelen, sizeofperm
!            INTEGER, EXTERNAL :: h5tinsert_array_c,  h5tinsert_array_c2
!            namelen = LEN(name)
!            if (present(perm)) then
!              hdferr = h5tinsert_array_c(parent_id, name, namelen, offset, ndims,dims, member_id, perm)
!            else
!              hdferr = h5tinsert_array_c2(parent_id, name, namelen, offset, ndims,dims, member_id)  
!            end if           
!           
!         END SUBROUTINE h5tinsert_array_f
         
          SUBROUTINE h5tarray_create_f(base_id, rank, dims, type_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: base_id ! identifier of array base datatype
            INTEGER, INTENT(IN) ::  rank ! Rank of the array
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims !Sizes of each array dimension
            INTEGER(HID_T), INTENT(OUT) :: type_id ! identifier of the array datatype 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

            INTEGER, EXTERNAL :: h5tarray_create_c
              hdferr = h5tarray_create_c(base_id, rank, dims, type_id)
           
         END SUBROUTINE h5tarray_create_f

          SUBROUTINE h5tenum_create_f(parent_id, new_type_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: parent_id  ! Datatype identifier for
                                                     ! the  base datatype
            INTEGER(HID_T), INTENT(OUT) :: new_type_id 
                                                     !datatype identifier for the
                                                     ! new enumeration datatype    
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tenum_create_c
            hdferr = h5tenum_create_c(parent_id, new_type_id)
          END SUBROUTINE h5tenum_create_f
            
          SUBROUTINE h5tenum_insert_f(type_id,  name, value, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name  !Name of  the new member
            INTEGER, INTENT(IN) :: value !value of the new member
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: namelen
            INTEGER, EXTERNAL :: h5tenum_insert_c
            namelen = LEN(name)
            hdferr = h5tenum_insert_c(type_id, name, namelen, value)
          END SUBROUTINE h5tenum_insert_f

          SUBROUTINE h5tenum_nameof_f(type_id,  name, namelen, value, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            CHARACTER(LEN=*), INTENT(OUT) :: name  !Name of the  enumeration datatype.
            INTEGER(SIZE_T), INTENT(IN) :: namelen !length of the name
            INTEGER, INTENT(IN) :: value !value of the  enumeration datatype.
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tenum_nameof_c
            hdferr = h5tenum_nameof_c(type_id, value, name, namelen)
          END SUBROUTINE h5tenum_nameof_f
            
          SUBROUTINE h5tenum_valueof_f(type_id,  name, value, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name  !Name of the  enumeration datatype.
            INTEGER, INTENT(OUT) :: value !value of the  enumeration datatype.
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: namelen
            INTEGER, EXTERNAL :: h5tenum_valueof_c
            namelen = LEN(name)
            hdferr = h5tenum_valueof_c(type_id, name, namelen,  value)
          END SUBROUTINE h5tenum_valueof_f

          SUBROUTINE h5tget_member_value_f(type_id,  member_no, value, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            INTEGER, INTENT(IN) :: member_no !Number of the enumeration datatype member
            INTEGER, INTENT(OUT) :: value !value of the  enumeration datatype.
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_member_value_c
            hdferr = h5tget_member_value_c(type_id, member_no, value)
          END SUBROUTINE h5tget_member_value_f

          SUBROUTINE h5tset_tag_f(type_id, tag, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            CHARACTER(LEN=*), INTENT(IN) :: tag !Unique ASCII string with which 
                                                !the opaque datatype is to be tagged 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: namelen
            INTEGER, EXTERNAL :: h5tset_tag_c
            namelen = LEN(tag)
            hdferr = h5tset_tag_c(type_id, tag, namelen)
          END SUBROUTINE h5tset_tag_f

          SUBROUTINE h5tget_tag_f(type_id, tag,taglen, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier 
            CHARACTER(LEN=*), INTENT(OUT) :: tag !Unique ASCII string with which 
                                                !the opaque datatype is to be tagged
            INTEGER, INTENT(OUT) :: taglen !length of tag 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5tget_tag_c
            hdferr = h5tget_tag_c(type_id, tag, taglen)
          END SUBROUTINE h5tget_tag_f

      END MODULE H5T
