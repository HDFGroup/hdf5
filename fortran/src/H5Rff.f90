!
! This file contains Fortran90 interfaces for H5R functions.
! 
      MODULE H5R
        USE H5FORTRAN_TYPES 
        USE H5FORTRAN_FLAGS

        TYPE hobj_ref_t_f
             !INTEGER(KIND=4) ref(2)   could cause trouble on Crays
             CHARACTER ref(8)
        END TYPE 

        TYPE hdset_reg_ref_t_f
             !INTEGER(KIND=4) reg_ref(3)  could cause troubles on Crays
             CHARACTER ref(12)
        END TYPE 

          INTERFACE h5rcreate_f

            MODULE PROCEDURE h5rcreate_object_f
            MODULE PROCEDURE h5rcreate_region_f 

          END INTERFACE 
          
          INTERFACE h5rdereference_f

            MODULE PROCEDURE h5rdereference_object_f
            MODULE PROCEDURE h5rdereference_region_f 

          END INTERFACE 
          
          INTERFACE h5rget_region_f

            MODULE PROCEDURE h5rget_region_region_f 

          END INTERFACE 
          
          INTERFACE h5rget_object_type_f

            MODULE PROCEDURE h5rget_object_type_obj_f

          END INTERFACE 
          

        CONTAINS
          
  
          SUBROUTINE h5rcreate_object_f(loc_id, name, ref, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! Location identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the object at location specified
                                                   ! by loc_id identifier 
            TYPE(hobj_ref_t_f), INTENT(OUT) :: ref   ! Object reference 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: namelen                     ! Name length
            INTEGER, EXTERNAL :: h5rcreate_object_c
            namelen = LEN(name)
            hdferr = h5rcreate_object_c(ref, loc_id, name, namelen )

          END SUBROUTINE h5rcreate_object_f
          
          SUBROUTINE h5rcreate_region_f(loc_id, name, space_id, ref, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! Location identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset at location specified
                                                   ! by loc_id identifier 
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataset's dataspace identifier 
            TYPE(hdset_reg_ref_t_f), INTENT(OUT) :: ref ! Dataset region reference 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: namelen                     ! Name length
            INTEGER, EXTERNAL :: h5rcreate_region_c
            namelen = LEN(name)
            hdferr = h5rcreate_region_c(ref, loc_id, name, namelen, space_id )

          END SUBROUTINE h5rcreate_region_f
          
          SUBROUTINE h5rdereference_object_f(dset_id, ref, obj_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier 
            TYPE(hobj_ref_t_f), INTENT(IN) :: ref   ! Object reference 
            INTEGER(HID_T), INTENT(OUT) :: obj_id   ! Object identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: ref_type     ! Reference type 
            INTEGER, EXTERNAL :: h5rdereference_object_c
            ref_type = H5R_OBJECT_F
            hdferr = h5rdereference_object_c(dset_id, ref, obj_id )

          END SUBROUTINE h5rdereference_object_f
          
          SUBROUTINE h5rdereference_region_f(dset_id, ref, obj_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier 
            TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref   ! Object reference 
            INTEGER(HID_T), INTENT(OUT) :: obj_id   ! Object identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code 

            INTEGER :: ref_type      ! Reference type 
            INTEGER, EXTERNAL :: h5rdereference_region_c
            ref_type = H5R_DATASET_REGION_F
            hdferr = h5rdereference_region_c(dset_id, ref, obj_id )

          END SUBROUTINE h5rdereference_region_f
          
          
          SUBROUTINE h5rget_region_region_f(dset_id, ref, space_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier 
            TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref   ! Dataset region reference 
            INTEGER(HID_T), INTENT(OUT) :: space_id   ! Space identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code 

            INTEGER, EXTERNAL :: h5rget_region_region_c
            hdferr = h5rget_region_region_c(dset_id, ref, space_id )

          END SUBROUTINE h5rget_region_region_f

          SUBROUTINE h5rget_object_type_obj_f(dset_id, ref, obj_type, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier 
            TYPE(hobj_ref_t_f), INTENT(IN) :: ref   ! Object reference 
            INTEGER, INTENT(OUT) :: obj_type   ! Object type  
                                               !  H5G_UNKNOWN_F     (-1)
                                               !  H5G_LINK_F         0
                                               !  H5G_GROUP_F        1
                                               !  H5G_DATASET_F      2
                                               !  H5G_TYPE_F         3

            INTEGER, INTENT(OUT) :: hdferr          ! Error code 

            INTEGER, EXTERNAL :: h5rget_object_type_obj_c
            hdferr = h5rget_object_type_obj_c(dset_id, ref, obj_type )

          END SUBROUTINE h5rget_object_type_obj_f

      END MODULE H5R
