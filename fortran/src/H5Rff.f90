!
! This file contains Fortran90 interfaces for H5R functions.
! 
      MODULE H5R
        USE H5GLOBAL

! If you change the value of these parameters, do not forget to change corresponding
! values in the H5f90.h file. 
        INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2 
        INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3 

        TYPE hobj_ref_t_f
             INTEGER ref(REF_OBJ_BUF_LEN)  
        END TYPE 

        TYPE hdset_reg_ref_t_f
             INTEGER ref(REF_REG_BUF_LEN) 
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
            INTEGER :: ref_f(REF_OBJ_BUF_LEN)          ! Local buffer to pass reference
            INTEGER, EXTERNAL :: h5rcreate_object_c
            namelen = LEN(name)
            ref_f = 0
            hdferr = h5rcreate_object_c(ref_f, loc_id, name, namelen )
            ref%ref = ref_f
        
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
            INTEGER :: ref_f(REF_REG_BUF_LEN)          ! Local buffer to pass reference
            INTEGER, EXTERNAL :: h5rcreate_region_c
            namelen = LEN(name)
            ref_f = 0
            hdferr = h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id )
            ref%ref = ref_f

          END SUBROUTINE h5rcreate_region_f
          
          SUBROUTINE h5rdereference_object_f(dset_id, ref, obj_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier 
            TYPE(hobj_ref_t_f), INTENT(IN) :: ref   ! Object reference 
            INTEGER(HID_T), INTENT(OUT) :: obj_id   ! Object identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: ref_type     ! Reference type 
            INTEGER :: ref_f(REF_OBJ_BUF_LEN)          ! Local buffer to pass reference
            INTEGER, EXTERNAL :: h5rdereference_object_c
            ref_f = ref%ref
            hdferr = h5rdereference_object_c(dset_id, ref_f, obj_id )

          END SUBROUTINE h5rdereference_object_f
          
          SUBROUTINE h5rdereference_region_f(dset_id, ref, obj_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier 
            TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref   ! Object reference 
            INTEGER(HID_T), INTENT(OUT) :: obj_id   ! Object identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code 

            INTEGER :: ref_type      ! Reference type 
            INTEGER :: ref_f(REF_REG_BUF_LEN)          ! Local buffer to pass reference
            INTEGER, EXTERNAL :: h5rdereference_region_c
            ref_type = H5R_DATASET_REGION_F
            ref_f = ref%ref
            hdferr = h5rdereference_region_c(dset_id, ref_f, obj_id )

          END SUBROUTINE h5rdereference_region_f
          
          
          SUBROUTINE h5rget_region_region_f(dset_id, ref, space_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier 
            TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref   ! Dataset region reference 
            INTEGER(HID_T), INTENT(OUT) :: space_id   ! Space identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code 
            INTEGER :: ref_f(REF_REG_BUF_LEN)          ! Local buffer to pass reference

            INTEGER, EXTERNAL :: h5rget_region_region_c
            ref_f = ref%ref
            hdferr = h5rget_region_region_c(dset_id, ref_f, space_id )

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
            INTEGER :: ref_f(REF_OBJ_BUF_LEN)          ! Local buffer to pass reference

            INTEGER, EXTERNAL :: h5rget_object_type_obj_c
            ref_f = ref%ref
            hdferr = h5rget_object_type_obj_c(dset_id, ref_f, obj_type )

          END SUBROUTINE h5rget_object_type_obj_f

      END MODULE H5R
