#include "H5f90.h"

/*----------------------------------------------------------------------------
 * Name:        h5iget_type_c
 * Purpose:     Call H5Iget_type to get the type of an object 
 * Inputs:      obj_id - object identifier 
 * Outputs:     type - object type 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Thursday, March 24, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5iget_type_c (hid_t_f *obj_id, int_f *type)
{
     int ret_value = -1;
     hid_t c_obj_id;
     H5I_type_t c_type;

     /*
      * Call H5Iget_type function.
      */
     c_obj_id = *obj_id;
     c_type = H5Iget_type(c_obj_id);
     if (c_type == H5I_BADID) return ret_value;
     *type = (int_f)c_type;
     ret_value = 0;
     return ret_value;
}
