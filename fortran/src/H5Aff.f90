!
! This file contains Fortran90 interfaces for H5A functions.
!
      MODULE H5A

        USE H5GLOBAL
          
          INTERFACE h5awrite_f

            MODULE PROCEDURE h5awrite_integer_scalar 
            MODULE PROCEDURE h5awrite_integer_1 
            MODULE PROCEDURE h5awrite_integer_2 
            MODULE PROCEDURE h5awrite_integer_3 
            MODULE PROCEDURE h5awrite_integer_4 
            MODULE PROCEDURE h5awrite_integer_5 
            MODULE PROCEDURE h5awrite_integer_6 
            MODULE PROCEDURE h5awrite_integer_7 
            MODULE PROCEDURE h5awrite_char_scalar 
            MODULE PROCEDURE h5awrite_char_1 
            MODULE PROCEDURE h5awrite_char_2 
            MODULE PROCEDURE h5awrite_char_3 
            MODULE PROCEDURE h5awrite_char_4 
            MODULE PROCEDURE h5awrite_char_5 
            MODULE PROCEDURE h5awrite_char_6 
            MODULE PROCEDURE h5awrite_char_7 
            MODULE PROCEDURE h5awrite_real_scalar
            MODULE PROCEDURE h5awrite_real_1
            MODULE PROCEDURE h5awrite_real_2
            MODULE PROCEDURE h5awrite_real_3
            MODULE PROCEDURE h5awrite_real_4
            MODULE PROCEDURE h5awrite_real_5
            MODULE PROCEDURE h5awrite_real_6
            MODULE PROCEDURE h5awrite_real_7
! Comment if on T3E
            MODULE PROCEDURE h5awrite_double_scalar
            MODULE PROCEDURE h5awrite_double_1
            MODULE PROCEDURE h5awrite_double_2
            MODULE PROCEDURE h5awrite_double_3
            MODULE PROCEDURE h5awrite_double_4
            MODULE PROCEDURE h5awrite_double_5
            MODULE PROCEDURE h5awrite_double_6
            MODULE PROCEDURE h5awrite_double_7
! End commnet if on T3E

          END INTERFACE

          INTERFACE h5aread_f

            MODULE PROCEDURE h5aread_integer_scalar
            MODULE PROCEDURE h5aread_integer_1 
            MODULE PROCEDURE h5aread_integer_2 
            MODULE PROCEDURE h5aread_integer_3 
            MODULE PROCEDURE h5aread_integer_4 
            MODULE PROCEDURE h5aread_integer_5 
            MODULE PROCEDURE h5aread_integer_6 
            MODULE PROCEDURE h5aread_integer_7 
            MODULE PROCEDURE h5aread_char_scalar 
            MODULE PROCEDURE h5aread_char_1 
            MODULE PROCEDURE h5aread_char_2 
            MODULE PROCEDURE h5aread_char_3 
            MODULE PROCEDURE h5aread_char_4 
            MODULE PROCEDURE h5aread_char_5 
            MODULE PROCEDURE h5aread_char_6 
            MODULE PROCEDURE h5aread_char_7 
            MODULE PROCEDURE h5aread_real_scalar
            MODULE PROCEDURE h5aread_real_1
            MODULE PROCEDURE h5aread_real_2
            MODULE PROCEDURE h5aread_real_3
            MODULE PROCEDURE h5aread_real_4
            MODULE PROCEDURE h5aread_real_5
            MODULE PROCEDURE h5aread_real_6
            MODULE PROCEDURE h5aread_real_7
! Comment if on T3E
            MODULE PROCEDURE h5aread_double_scalar
            MODULE PROCEDURE h5aread_double_1
            MODULE PROCEDURE h5aread_double_2
            MODULE PROCEDURE h5aread_double_3
            MODULE PROCEDURE h5aread_double_4
            MODULE PROCEDURE h5aread_double_5
            MODULE PROCEDURE h5aread_double_6
            MODULE PROCEDURE h5aread_double_7
! End commnet if on T3E
          END INTERFACE

      CONTAINS
          SUBROUTINE h5acreate_f(obj_id, name, type_id, space_id, attr_id, &
                                 hdferr, creation_prp) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
            INTEGER(HID_T), INTENT(IN) :: type_id   
                                           ! Attribute datatype identifier 
            INTEGER(HID_T), INTENT(IN) :: space_id  
                                           ! Attribute dataspace identifier
            INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: creation_prp
                                                ! Attribute creation property 
                                                ! list identifier 
            INTEGER :: creation_prp_default 
            INTEGER :: namelen
            INTEGER, EXTERNAL :: h5acreate_c
            creation_prp_default = H5P_DEFAULT_F
            namelen = LEN(NAME)
            if (present(creation_prp)) creation_prp_default = creation_prp
            hdferr = h5acreate_c(obj_id, name, namelen, type_id, space_id, &
                                 creation_prp_default, attr_id)
          END SUBROUTINE h5acreate_f



          SUBROUTINE h5aopen_name_f(obj_id, name, attr_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
            INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER :: namelen
            INTEGER, EXTERNAL :: h5aopen_name_c
            namelen = LEN(name)
            hdferr = h5aopen_name_c(obj_id, name, namelen, attr_id)
          END SUBROUTINE h5aopen_name_f



          SUBROUTINE h5aopen_idx_f(obj_id, index, attr_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier 
            INTEGER, INTENT(IN) :: index            ! Attribute index 
            INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aopen_idx_c
            hdferr = h5aopen_idx_c(obj_id, index, attr_id)
          END SUBROUTINE h5aopen_idx_f



          SUBROUTINE h5awrite_integer_scalar(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, INTENT(IN) :: buf              ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_integer_scalar

          SUBROUTINE h5awrite_integer_1(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_integer_1


          SUBROUTINE h5awrite_integer_2(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_integer_2


          SUBROUTINE h5awrite_integer_3(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_integer_3


          SUBROUTINE h5awrite_integer_4(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_integer_4


          SUBROUTINE h5awrite_integer_5(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_integer_5


          SUBROUTINE h5awrite_integer_6(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_integer_6


          SUBROUTINE h5awrite_integer_7(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_integer_7


          SUBROUTINE h5awrite_real_scalar(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, INTENT(IN) :: buf                 ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_real_scalar

          SUBROUTINE h5awrite_real_1(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_real_1


          SUBROUTINE h5awrite_real_2(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_real_2


          SUBROUTINE h5awrite_real_3(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_real_3


          SUBROUTINE h5awrite_real_4(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_real_4


          SUBROUTINE h5awrite_real_5(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_real_5


          SUBROUTINE h5awrite_real_6(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_real_6


          SUBROUTINE h5awrite_real_7(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_real_7


          SUBROUTINE h5awrite_double_scalar(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, INTENT(IN) :: buf     ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_double_scalar

          SUBROUTINE h5awrite_double_1(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_double_1


          SUBROUTINE h5awrite_double_2(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_double_2


          SUBROUTINE h5awrite_double_3(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_double_3


          SUBROUTINE h5awrite_double_4(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_double_4


          SUBROUTINE h5awrite_double_5(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_double_5


          SUBROUTINE h5awrite_double_6(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_double_6


          SUBROUTINE h5awrite_double_7(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awrite_c
            hdferr = h5awrite_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_double_7

          SUBROUTINE h5awrite_char_scalar(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*),INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awritec_c
            hdferr = h5awritec_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_char_scalar

          SUBROUTINE h5awrite_char_1(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(*), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awritec_c
            hdferr = h5awritec_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_char_1


          SUBROUTINE h5awrite_char_2(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awritec_c
            hdferr = h5awritec_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_char_2


          SUBROUTINE h5awrite_char_3(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awritec_c
            hdferr = h5awritec_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_char_3


          SUBROUTINE h5awrite_char_4(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awritec_c
            hdferr = h5awritec_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_char_4


          SUBROUTINE h5awrite_char_5(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awritec_c
            hdferr = h5awritec_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_char_5


          SUBROUTINE h5awrite_char_6(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awritec_c
            hdferr = h5awritec_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_char_6


          SUBROUTINE h5awrite_char_7(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:,:,:,:,:), INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5awritec_c
            hdferr = h5awritec_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5awrite_char_7


          SUBROUTINE h5aread_integer_scalar(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, INTENT(OUT) :: buf             ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_integer_scalar

          SUBROUTINE h5aread_integer_1(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_integer_1


          SUBROUTINE h5aread_integer_2(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_integer_2


          SUBROUTINE h5aread_integer_3(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_integer_3


          SUBROUTINE h5aread_integer_4(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_integer_4


          SUBROUTINE h5aread_integer_5(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_integer_5


          SUBROUTINE h5aread_integer_6(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_integer_6


          SUBROUTINE h5aread_integer_7(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER, DIMENSION(:,:,:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_integer_7


          SUBROUTINE h5aread_real_scalar(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, INTENT(OUT) :: buf                ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_real_scalar

          SUBROUTINE h5aread_real_1(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_real_1


          SUBROUTINE h5aread_real_2(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_real_2


          SUBROUTINE h5aread_real_3(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_real_3


          SUBROUTINE h5aread_real_4(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_real_4


          SUBROUTINE h5aread_real_5(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_real_5


          SUBROUTINE h5aread_real_6(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_real_6


          SUBROUTINE h5aread_real_7(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            REAL, DIMENSION(:,:,:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_real_7


          SUBROUTINE h5aread_double_scalar(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, INTENT(OUT) :: buf    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_double_scalar

          SUBROUTINE h5aread_double_1(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_double_1


          SUBROUTINE h5aread_double_2(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_double_2


          SUBROUTINE h5aread_double_3(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_double_3


          SUBROUTINE h5aread_double_4(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_double_4


          SUBROUTINE h5aread_double_5(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_double_5


          SUBROUTINE h5aread_double_6(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_double_6


          SUBROUTINE h5aread_double_7(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            DOUBLE PRECISION, DIMENSION(:,:,:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5aread_c
            hdferr = h5aread_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_double_7

          SUBROUTINE h5aread_char_scalar(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5areadc_c
            hdferr = h5areadc_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_char_scalar


          SUBROUTINE h5aread_char_1(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(*), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5areadc_c
            hdferr = h5areadc_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_char_1


          SUBROUTINE h5aread_char_2(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5areadc_c
            hdferr = h5areadc_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_char_2


          SUBROUTINE h5aread_char_3(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5areadc_c
            hdferr = h5areadc_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_char_3


          SUBROUTINE h5aread_char_4(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5areadc_c
            hdferr = h5areadc_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_char_4


          SUBROUTINE h5aread_char_5(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5areadc_c
            hdferr = h5areadc_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_char_5


          SUBROUTINE h5aread_char_6(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5areadc_c
            hdferr = h5areadc_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_char_6


          SUBROUTINE h5aread_char_7(attr_id, memtype_id,  buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            CHARACTER(LEN=*), DIMENSION(:,:,:,:,:,:,:), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5areadc_c
            hdferr = h5areadc_c(attr_id, memtype_id,  buf)
          END SUBROUTINE h5aread_char_7


          SUBROUTINE h5aget_space_f(attr_id, space_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier 
            INTEGER(HID_T), INTENT(OUT) :: space_id 
                                            ! Attribute dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL:: h5aget_space_c
            hdferr = h5aget_space_c(attr_id, space_id)
          END SUBROUTINE h5aget_space_f


          SUBROUTINE h5aget_type_f(attr_id, type_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier 
            INTEGER(HID_T), INTENT(OUT) :: type_id 
                                              ! Attribute datatype identifier
            INTEGER, INTENT(OUT) :: hdferr    ! Error code
            INTEGER, EXTERNAL :: h5aget_type_c
            hdferr = h5aget_type_c(attr_id, type_id)
          END SUBROUTINE h5aget_type_f



          SUBROUTINE h5aget_name_f(attr_id, size, buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier 
            INTEGER(SIZE_T), INTENT(IN) :: size            ! Buffer size 
            CHARACTER(LEN=*), INTENT(INOUT) :: buf   
                                               ! Buffer to hold attribute name
            INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                                   ! name length is successful,
                                                   ! -1 if fail
            INTEGER, EXTERNAL :: h5aget_name_c
            hdferr = h5aget_name_c(attr_id, size, buf)
          END SUBROUTINE h5aget_name_f



          SUBROUTINE h5aget_num_attrs_f(obj_id, attr_num, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id  ! Object identifier 
            INTEGER, INTENT(OUT) :: attr_num      ! Number of attributes of the
                                                  ! object
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER, EXTERNAL :: h5aget_num_attrs_c
            hdferr = h5aget_num_attrs_c(obj_id, attr_num)
          END SUBROUTINE h5aget_num_attrs_f


          SUBROUTINE h5adelete_f(obj_id, name, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER :: namelen
            INTEGER, EXTERNAL ::  h5adelete_c
            namelen = LEN(name)
            hdferr = h5adelete_c(obj_id, name, namelen)
          END SUBROUTINE h5adelete_f


          SUBROUTINE h5aclose_f(attr_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code:
            INTEGER, EXTERNAL :: h5aclose_c
            hdferr = h5aclose_c(attr_id)
          END SUBROUTINE h5aclose_f

   END MODULE H5A
