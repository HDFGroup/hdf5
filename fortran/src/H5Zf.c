#include "H5f90.h"

/*----------------------------------------------------------------------------
 * Name:        h5zunregister_c
 * Purpose:     Call H5Zunregister to unregister filter
 * Inputs:      filter identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, March 12, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5zunregister_c (int_f *filter)
{
     int ret_value = -1;
     herr_t status;
     H5Z_filter_t c_filter;

     /*
      * Call H5Zunregister function.
      */
     c_filter = (H5Z_filter_t)*filter;
     printf(" filter # %d \n", (int)c_filter);
     status = H5Zunregister(c_filter);
     printf("From C zunregister %d \n", status);
     if (status < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}
/*----------------------------------------------------------------------------
 * Name:        h5zfiletr_avail_c
 * Purpose:     Call H5Zfilter_avail to find if filter is available
 * Inputs:      filter - filter identifier
 * Outputs:     flag - status flag
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, March 12, 2003
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5zfilter_avail_c ( int_f *filter , int_f *flag )
{
  int ret_value = 0;
  H5Z_filter_t c_filter;
  htri_t status;

  c_filter = (H5Z_filter_t)*filter;
  status = H5Zfilter_avail(c_filter);
  *flag = (int_f)status;
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}
