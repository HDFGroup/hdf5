!
! This file contains FORTRAN90 interfaces for H5I functions
!
      MODULE H5I

        USE H5FORTRAN_TYPES 
        USE H5FORTRAN_FLAGS 
      
      CONTAINS

          SUBROUTINE h5iget_type_f(obj_id, type, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id  !Object identifier 
            INTEGER, INTENT(OUT) :: type !type of an object. 
                                         !possible values are:
                                         !H5I_FILE_F(1)
                                         !H5I_GROUP_F(2)
                                         !H5I_DATATYPE_F(3)
                                         !H5I_DATASPACE_F(4)
                                         !H5I_DATASET_F(5)
                                         !H5I_ATTR_F(6)
                                         !H5I_BADID_F(-1)
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5iget_type_c
            hdferr = h5iget_type_c(obj_id, type)
          END SUBROUTINE h5iget_type_f

      END MODULE H5I
            

                                     


