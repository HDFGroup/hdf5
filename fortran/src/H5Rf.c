/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This files contains C stubs for H5R Fortran APIs */

#include "H5f90.h"

/*----------------------------------------------------------------------------
 * Name:        h5rcreate_object_c
 * Purpose:     Call H5Rcreate to create a reference to an object
 * Inputs:      loc_id - file or group identifier
 *              name - name of the dataset
 *              namelen - name length
 * Outputs:     ref  - reference to the object
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5rcreate_object_c (haddr_t_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen)
{
     int ret_value = -1;
     hid_t c_loc_id;
     int ret_value_c;
     char *c_name;
     int c_namelen;
     hobj_ref_t ref_c;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Rcreate function.
      */
     c_loc_id = *loc_id;
     ret_value_c = H5Rcreate(&ref_c, c_loc_id, c_name, H5R_OBJECT, -1);

     HDfree(c_name);
     if (ret_value_c >= 0)  {
         *ref=(haddr_t_f)ref_c;
         ret_value = 0;
     }

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5rcreate_region_c
 * Purpose:     Call H5Rcreate to create a reference to dataset region
 *              region
 * Inputs:      loc_id - file or group identifier
 *              name - name of the dataset
 *              namelen - name length
 *              space_id - dataset space identifier
 * Outputs:     ref  - reference to the dataset region
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5rcreate_region_c (int_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *space_id)
{
     int ret_value = -1;
     hid_t c_loc_id;
     hid_t c_space_id;
     int ret_value_c;
     char *c_name;
     int c_namelen;
     hdset_reg_ref_t ref_c;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Rcreate function.
      */
     c_loc_id = *loc_id;
     c_space_id = *space_id;
     ret_value_c = H5Rcreate(&ref_c, c_loc_id, c_name, H5R_DATASET_REGION, c_space_id);

     HDfree(c_name);
     if (ret_value_c >= 0) {
         HDmemcpy (ref, &ref_c, H5R_DSET_REG_REF_BUF_SIZE);
         ret_value = 0;
     }
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5rdereference_region_c
 * Purpose:     Call H5Rdereference to dereference to dataset region
 * Inputs:      dset_id - dataset identifier
 *              ref - reference to the dataset region
 * Outputs:     obj_id - dereferenced dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5rdereference_region_c (hid_t_f *dset_id, int_f *ref, hid_t_f *obj_id)
{
     int ret_value = -1;
     hid_t c_dset_id;
     hdset_reg_ref_t ref_c;
     hid_t c_obj_id;

     HDmemcpy (&ref_c, ref, H5R_DSET_REG_REF_BUF_SIZE);

     /*
      * Call H5Rdereference function.
      */
     c_dset_id = *dset_id;
     c_obj_id = H5Rdereference(c_dset_id, H5R_DATASET_REGION, &ref_c);
     if(c_obj_id < 0) return ret_value;
     *obj_id = (hid_t_f)c_obj_id;
     ret_value = 0;
     return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5rdereference_object_c
 * Purpose:     Call H5Rdereference to dereference an object
 * Inputs:      dset_id - dataset identifier
 *              ref - reference to an object
 * Outputs:     obj_id - dereferenced  object identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5rdereference_object_c (hid_t_f *dset_id, haddr_t_f *ref, hid_t_f *obj_id)
{
     int ret_value = -1;
     hid_t c_dset_id;
     hid_t c_obj_id;
     hobj_ref_t ref_c;

     ref_c=(hobj_ref_t)*ref;

     /*
      * Call H5Rdereference function.
      */
     c_dset_id = *dset_id;
     c_obj_id = H5Rdereference(c_dset_id, H5R_OBJECT, &ref_c);
     if(c_obj_id < 0) return ret_value;
     *obj_id = (hid_t_f)c_obj_id;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5rget_region_region_object_c
 * Purpose:     Call H5Rget_region to dereference dataspace region
 * Inputs:      dset_id - dataset identifier
 *              ref - reference to the dataset region
 * Outputs:     space_id - dereferenced  dataset dataspace identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5rget_region_region_c (hid_t_f *dset_id, int_f *ref, hid_t_f *space_id)
{
     int ret_value = -1;
     hid_t c_dset_id;
     hid_t c_space_id;
     hdset_reg_ref_t ref_c;

     HDmemcpy (&ref_c, ref, H5R_DSET_REG_REF_BUF_SIZE);

     /*
      * Call H5Rget_region function.
      */
     c_dset_id = *dset_id;
     c_space_id = H5Rget_region(c_dset_id, H5R_DATASET_REGION, &ref_c);
     if(c_space_id < 0) return ret_value;
     *space_id = (hid_t_f)c_space_id;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5rget_object_type_obj_c
 * Purpose:     Call H5Rget_object_type to retrieve the type of the object reference points
 *              to
 * Inputs:      dset_id - dataset identifier
 *              ref - reference to the dataset region
 * Outputs:     obj_type - type of dereferenced object
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, December 1, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5rget_object_type_obj_c (hid_t_f *dset_id, haddr_t_f *ref, int_f *obj_type)
{
     int ret_value = -1;
     hid_t c_dset_id;
     int c_obj_type;
     hobj_ref_t ref_c;

     ref_c=*ref;

     /*
      * Call H5Rget_object_type function.
      */
     c_dset_id = *dset_id;
     c_obj_type = H5Rget_obj_type(c_dset_id, H5R_OBJECT, &ref_c);
     if(c_obj_type < 0) return ret_value;
     *obj_type = (int_f)c_obj_type;
     ret_value = 0;
     return ret_value;
}
