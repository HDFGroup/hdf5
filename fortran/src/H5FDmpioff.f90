!
! This file contains Fortran90 interfaces for H5P functions needed by || MPI programs.
!
     MODULE H5FDMPIO
         USE H5FORTRAN_TYPES
         USE H5FORTRAN_FLAGS
         CONTAINS
         SUBROUTINE h5pset_fapl_mpio_f(prp_id, comm, info, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(IN) :: comm ! MPI communicator to be used for file open
                                        ! as defined in MPI_FILE_OPEN of MPI-2
            INTEGER, INTENT(IN) :: info ! MPI info object to be used for file open
                                        ! as defined in MPI_FILE_OPEN of MPI-2
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pset_fapl_mpio_c
            hdferr = h5pset_fapl_mpio_c(prp_id, comm, info)
          END SUBROUTINE h5pset_fapl_mpio_f

          SUBROUTINE h5pget_fapl_mpio_f(prp_id, comm, info, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(OUT) :: comm ! buffer to return communicator 
            INTEGER, INTENT(IN) :: info ! buffer to return info object 
                                        ! as defined in MPI_FILE_OPEN of MPI-2
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pget_fapl_mpio_c
            hdferr = h5pget_fapl_mpio_c(prp_id, comm, info)
          END SUBROUTINE h5pget_fapl_mpio_f

         SUBROUTINE h5pset_dxpl_mpio_f(prp_id, data_xfer_mode, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(IN) :: data_xfer_mode ! Data transfer mode. Possible values are:
                                                  ! H5FD_MPIO_INDEPENDENT_F (0)
                                                  ! H5FD_MPIO_COLLECTIVE_F  (1)
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pset_dxpl_mpio_c 
            hdferr = h5pset_dxpl_mpio_c(prp_id, data_xfer_mode)
          END SUBROUTINE h5pset_dxpl_mpio_f

         SUBROUTINE h5pget_dxpl_mpio_f(prp_id, data_xfer_mode, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(OUT) :: data_xfer_mode ! Data transfer mode. Possible values are:
                                                  ! H5FD_MPIO_INDEPENDENT_F (0)
                                                  ! H5FD_MPIO_COLLECTIVE_F  (1)
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER, EXTERNAL :: h5pget_dxpl_mpio_c
            hdferr = h5pget_dxpl_mpio_c(prp_id, data_xfer_mode)
          END SUBROUTINE h5pget_dxpl_mpio_f

    END MODULE H5FDMPIO
