#include "H5f90.h"
#include <mpi.h> 

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_mpio_c
 * Purpose:     Call H5Pset_fapl_mpio to set mode for parallel I/O and the user 
 *              supplied communicator and info object 
 * Inputs:      prp_id - property list identifier
 *              comm   - MPI communicator 
 *              info   - MPI info object 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal 
 *              Thursday, October 26, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_mpio_c(hid_t_f *prp_id, int_f* comm, int_f* info)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     MPI_Comm c_comm;
     MPI_Info c_info; 
     c_comm = (MPI_Comm) *comm; 
     c_info = (MPI_Info) *info;

     /*
      * Call H5Pset_mpi function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_fapl_mpio(c_prp_id, c_comm, c_info);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_mpio_c
 * Purpose:     Call H5Pget_fapl_mpio to retrieve communicator and info object 
 * Inputs:      prp_id - property list identifier
 *              comm   - buffer to return MPI communicator 
 *              info   - buffer to return MPI info object 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal 
 *              Thursday, October 26, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_mpio_c(hid_t_f *prp_id, int_f* comm, int_f* info)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     MPI_Comm c_comm;
     MPI_Info c_info; 

     /*
      * Call H5Pget_mpi function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_mpio(c_prp_id, &c_comm, &c_info);
     if (ret < 0) return ret_value;
     *comm = (int_f) c_comm;
     *info = (int_f) c_info;
     ret_value = 0;
     return ret_value;
} 
/*----------------------------------------------------------------------------
 * Name:        h5pset_dxpl_mpio_c
 * Purpose:     Call H5Pset_dxpl_mpio to set transfer mode of the dataset 
 *              trasfer property list 
 * Inputs:      prp_id - property list identifier
 *              data_xfer_mode - transfer mode
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal 
 *              Thursday, October 26, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_dxpl_mpio_c(hid_t_f *prp_id, int_f* data_xfer_mode)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     H5FD_mpio_xfer_t c_data_xfer_mode;
/*
     int CASE;

     CASE = *data_xfer_mode;
     switch (CASE) {

        case H5FD_MPIO_INDEPENDENT_F:
             c_data_xfer_mode = H5FD_MPIO_INDEPENDENT;
             break;

        case H5FD_MPIO_COLLECTIVE_F:
             c_data_xfer_mode = H5FD_MPIO_COLLECTIVE;
             break;
        default:
          return ret_value;
      }
*/
     c_data_xfer_mode = (H5FD_mpio_xfer_t)*data_xfer_mode;
     /*
      * Call H5Pset_dxpl_mpio function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_dxpl_mpio(c_prp_id, c_data_xfer_mode);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pget_dxpl_mpio_c
 * Purpose:     Call H5Pget_dxpl_mpio to get transfer mode of the dataset 
 *              trasfer property list 
 * Inputs:      prp_id - property list identifier
 *              data_xfer_mode  - buffer to retrieve transfer mode
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal 
 *              Thursday, June 15, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_dxpl_mpio_c(hid_t_f *prp_id, int_f* data_xfer_mode)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     H5FD_mpio_xfer_t c_data_xfer_mode;
     int CASE;

     /*
      * Call H5Pget_xfer function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_dxpl_mpio(c_prp_id, &c_data_xfer_mode);
     if (ret < 0) return ret_value;
     *data_xfer_mode = (int_f)c_data_xfer_mode; 
/*
     CASE = c_data_xfer_mode;
     switch (CASE) {

        case H5FD_MPIO_INDEPENDENT:
             *data_xfer_mode = H5FD_MPIO_INDEPENDENT_F;
             break;

        case H5FD_MPIO_COLLECTIVE:
             *data_xfer_mode = H5FD_MPIO_COLLECTIVE_F;
             break;

        default:
          return ret_value;
      }
*/
     ret_value = 0;
     return ret_value;
} 
