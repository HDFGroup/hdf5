
    SUBROUTINE filters_test(cleanup, total_error)

!   This subroutine tests following functionalities: h5zfilter_avail_f, h5zunregister_f

   USE HDF5 ! This module contains all necessary modules 

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error 
     LOGICAL :: status, status1
     INTEGER(HID_T)    :: crtpr_id, xfer_id
     INTEGER           :: error
     INTEGER(HSIZE_T)  :: ch_dims(2)
     INTEGER           :: RANK = 2
     INTEGER           :: dlevel = 6
     INTEGER           :: edc_flag

     ch_dims(1) = 10
     ch_dims(2) = 3
!
! Deflate filter
!
     CALL h5zfilter_avail_f(H5Z_FILTER_DEFLATE_F, status, error)
              CALL check("h5zfilter_avail_f", error, total_error)
     if(status) then
        CALL h5pcreate_f(H5P_DATASET_CREATE_F, crtpr_id, error)
              CALL check("h5pcreate_f", error, total_error)
        CALL h5pset_chunk_f(crtpr_id, RANK, ch_dims, error)
              CALL check("h5pset_chunk_f",error, total_error)
        CALL h5pset_deflate_f(crtpr_id, dlevel, error)
              CALL check("h5pset_deflate_f", error, total_error) 
        CALL h5pclose_f(crtpr_id,error)
              CALL check("h5pclose_f", error, total_error)
     endif
        
!
! Shuffle filter
!
     CALL h5zfilter_avail_f(H5Z_FILTER_SHUFFLE_F, status, error)
              CALL check("h5zfilter_avail_f", error, total_error)
     if(status) then
        CALL h5pcreate_f(H5P_DATASET_CREATE_F, crtpr_id, error)
              CALL check("h5pcreate_f", error, total_error)
        CALL h5pset_chunk_f(crtpr_id, RANK, ch_dims, error)
              CALL check("h5pset_chunk_f",error, total_error)
        CALL h5pset_shuffle_f(crtpr_id, error)
              CALL check("h5pset_shuffle_f", error, total_error) 
        CALL h5pclose_f(crtpr_id,error)
              CALL check("h5pclose_f", error, total_error)
     endif
        
!
! Checksum filter
!
     CALL h5zfilter_avail_f(H5Z_FILTER_FLETCHER32_F, status, error)
              CALL check("h5zfilter_avail_f", error, total_error)
     if(status) then
        CALL h5pcreate_f(H5P_DATASET_CREATE_F, crtpr_id, error)
              CALL check("h5pcreate_f", error, total_error)
        CALL h5pset_chunk_f(crtpr_id, RANK, ch_dims, error)
              CALL check("h5pset_chunk_f",error, total_error)
        CALL h5pset_fletcher32_f(crtpr_id, error)
              CALL check("h5pset_fletcher32_f", error, total_error) 
        CALL h5pclose_f(crtpr_id,error)
              CALL check("h5pclose_f", error, total_error)
        CALL h5pcreate_f(H5P_DATASET_XFER_F, xfer_id, error)
              CALL check("h5pcreate_f", error, total_error)
        CALL h5pset_edc_check_f( xfer_id, H5Z_DISABLE_EDC_F, error)
              CALL check("h5pset_edc_check_f", error, total_error)
        CALL h5pget_edc_check_f( xfer_id, edc_flag, error)
              CALL check("h5pget_edc_check_f", error, total_error)
        if (edc_flag .ne. H5Z_DISABLE_EDC_F) then
              write(*,*) "EDC status is wrong"
              total_error = total_error + 1
        endif
        CALL h5pclose_f(xfer_id, error)
              CALL check("h5pclose_f", error, total_error)

     endif

     RETURN
     END SUBROUTINE filters_test
