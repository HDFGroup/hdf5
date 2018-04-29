/****h* H5Rf/H5Rf
 * PURPOSE
 *  This file contains C stubs for H5R Fortran APIs
 *
 * COPYRIGHT
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 ******
*/

#include "H5f90.h"
#include "H5Eprivate.h"

/****if* H5Rf/h5rcreate_region_c
 * NAME
 *  h5rcreate_region_c
 * PURPOSE
 *  Call H5Rcreate to create a reference to dataset region
 *  region
 * INPUTS
 *  loc_id - file or group identifier
 *  name - name of the dataset
 *  namelen - name length
 *  space_id - dataset space identifier
 * OUTPUTS
 *  ref  - reference to the dataset region
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Wednesday, December 1, 1999
 * HISTORY
 *
 * SOURCE
*/
int_f
h5rcreate_region_c(int_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *space_id)
/******/
{
     char *c_name = NULL;
     hdset_reg_ref_t ref_c;
     int_f ret_value = 0;

     /*
      * Convert FORTRAN name to C name
      */
     if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
         HGOTO_DONE(FAIL)

     /*
      * Call H5Rcreate function.
      */
     if(H5Rcreate(&ref_c, (hid_t)*loc_id, c_name, H5R_DATASET_REGION, (hid_t)*space_id) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the reference created */
     HDmemcpy(ref, &ref_c, H5R_DSET_REG_REF_BUF_SIZE);

done:
     if(c_name)
         HDfree(c_name);
     return ret_value;
} /* end h5rcreate_region_c() */

/****if* H5Rf/h5rcreate_ptr_c
 * NAME
 *  h5rcreate_ptr_c
 * PURPOSE
 *  Call H5Rcreate to create a reference to dataset region
 * INPUTS
 *  loc_id    - file or group identifier
 *  name      - name of the dataset
 *  namelen   - name length
 *  space_id  - dataset space identifier
 * OUTPUTS
 *  ref       - reference to the dataset region
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  June 20, 2008
 *
 * SOURCE
*/
int_f
h5rcreate_ptr_c (void *ref, hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *ref_type, hid_t_f *space_id)
/******/
{
     int ret_value = -1;
     char *c_name;

     /*
      * Convert FORTRAN name to C name
      */
     c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Rcreate function.
      */
     if(H5Rcreate(ref, (hid_t)*loc_id, c_name, (H5R_type_t)*ref_type, (hid_t)*space_id) >= 0)
       ret_value = 0;

     HDfree(c_name);
     return ret_value;
}

/****if* H5Rf/h5rdereference_ptr_c
 * NAME
 *  h5rdereference_ptr_c
 * PURPOSE
 *  Call H5Rdereference
 * INPUTS
 *  obj_id  - Valid identifier for the file containing the
 *  referenced object or any object in that file.
 *  ref_typ - The reference type of ref.
 *  ref     - Object reference
 * OUTPUTS
 *  ref_obj_id - Identifier of referenced object
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  June 20, 2008
 * HISTORY
 *
 * SOURCE
*/
int_f
h5rdereference_ptr_c (hid_t_f *obj_id, int_f *ref_type, void *ref, hid_t_f *ref_obj_id)
/******/
{
  int ret_value = -1;
  hid_t c_ref_obj_id;

  /*
   * Call H5Rdereference function.
   */
  c_ref_obj_id = H5Rdereference2((hid_t)*obj_id, H5P_DEFAULT, (H5R_type_t)*ref_type, ref);
  if(c_ref_obj_id < 0) return ret_value;
  *ref_obj_id = (hid_t_f)c_ref_obj_id;
  ret_value = 0;
  return ret_value;
}

/****if* H5Rf/h5rget_region_region_object_c
 * NAME
 *  h5rget_region_region_object_c
 * PURPOSE
 *  Call H5Rget_region to dereference dataspace region
 * INPUTS
 *  dset_id - dataset identifier
 *  ref - reference to the dataset region
 * OUTPUTS
 *  space_id - dereferenced  dataset dataspace identifier
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Wednesday, December 1, 1999
 * HISTORY
 *
 * SOURCE
*/
int_f
h5rget_region_region_c(hid_t_f *dset_id, int_f *ref, hid_t_f *space_id)
/******/
{
     hid_t c_space_id;
     hdset_reg_ref_t ref_c;
     int_f ret_value = 0;

     /* Copy the reference to dereference */
     HDmemcpy(&ref_c, ref, H5R_DSET_REG_REF_BUF_SIZE);

     /*
      * Call H5Rget_region function.
      */
     if((c_space_id = H5Rget_region((hid_t)*dset_id, H5R_DATASET_REGION, &ref_c)) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the dataspace ID */
     *space_id = (hid_t_f)c_space_id;

done:
     return ret_value;
} /* end h5rget_region_region_c() */

/****if* H5Rf/h5rget_region_ptr_c
 * NAME
 *  h5rget_region_ptr_c
 * PURPOSE
 *  Call H5Rget_region to dereference dataspace region
 * INPUTS
 *  dset_id - dataset identifier
 *  ref - reference to the dataset region
 * OUTPUTS
 *  space_id - dereferenced  dataset dataspace identifier
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  August 4, 2012
 * HISTORY
 *
 * SOURCE
*/
int_f
h5rget_region_ptr_c(hid_t_f *dset_id, void *ref, hid_t_f *space_id)
/******/
{
     hid_t c_space_id;
     int_f ret_value = 0;

     /*
      * Call H5Rget_region function.
      */
     if((c_space_id = H5Rget_region((hid_t)*dset_id, H5R_DATASET_REGION, ref)) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the dataspace ID */
     *space_id = (hid_t_f)c_space_id;

done:
     return ret_value;
} /* end h5rget_region_ptr_c() */


/****if* H5Rf/h5rget_object_type_obj_c
 * NAME
 *  h5rget_object_type_obj_c
 * PURPOSE
 *  Call H5Rget_object_type to retrieve the type of the object reference points
 *  to
 * INPUTS
 *  dset_id - dataset identifier
 *  ref - reference to the dataset region
 * OUTPUTS
 *  obj_type - type of dereferenced object
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Wednesday, December 1, 1999
 * HISTORY
 *
 * SOURCE
*/
int_f
h5rget_object_type_obj_c(hid_t_f *dset_id, haddr_t_f *ref, int_f *obj_type)
/******/
{
     H5O_type_t c_obj_type;
     hobj_ref_t ref_c = (hobj_ref_t)*ref;
     int_f ret_value = 0;

     /*
      * Call H5Rget_object_type function.
      */
     if(H5Rget_obj_type2((hid_t)*dset_id, H5R_OBJECT, &ref_c, &c_obj_type) < 0)
         HGOTO_DONE(FAIL)

     /* Copy the object type */
     *obj_type = (int_f)c_obj_type;

done:
     return ret_value;
} /* end h5rget_object_type_obj_c() */

/****if* H5Rf/h5rget_name_ptr_c
 * NAME
 *  h5rget_name_ptr_c
 * PURPOSE
 *  Call H5Rget_name
 * INPUTS
 *
 *  loc_id - Identifier for the dataset containing the reference or for the group that dataset is in.
 *  ref_type - Type of reference.
 *  ref - An object or dataset region reference.
 *
 * OUTPUTS
 *  name - A name associated with the referenced object or dataset region.
 *  size - The size of the name buffer.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  June 20, 2008
 * HISTORY
 *
 * SOURCE
*/
int_f
h5rget_name_ptr_c (hid_t_f *loc_id, int_f *ref_type, void *ref, _fcd name, size_t_f *name_len, size_t_f *size_default)
/******/
{
     int_f ret_value = -1;
     ssize_t c_size;
     size_t c_bufsize;
     char *c_buf= NULL;  /* Buffer to hold C string */

     c_bufsize = (size_t)*name_len+1;
     /*
      * Allocate buffer to hold name of an attribute
      */
     if ((c_buf = (char *)HDmalloc(c_bufsize)) == NULL)
       return ret_value;

     /*
      * Call H5Rget_name function.
      */
     if((c_size=H5Rget_name((hid_t)*loc_id, (H5R_type_t)*ref_type, ref, c_buf, c_bufsize)) < 0){
       if(c_buf) HDfree(c_buf);
       return ret_value;
     }
     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
     HD5packFstring(c_buf, _fcdtocp(name), c_bufsize-1);

     *size_default = (size_t_f)c_size;
     ret_value = 0;
     if(c_buf) HDfree(c_buf);

     return ret_value;
}

/****if* H5Rf/h5rget_obj_type_c
 * NAME
 *  h5rget_obj_type_c
 * PURPOSE
 *  Call H5Rget_obj_type
 * INPUTS
 *  loc_id   - Identifier for the dataset containing the reference or
 *  for the group that dataset is in.
 *  ref_type - Type of reference to query.
 *  ref      - Reference to query.
 *
 * OUTPUTS
 *  obj_type - Type of referenced object. These are defined in H5Opublic.h,
 *  enum H5O_type_t
 *              
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  December 17, 2008
 *
 * SOURCE
*/
int_f
h5rget_obj_type_c (hid_t_f *loc_id, int_f *ref_type, void *ref, int_f *obj_type)
/******/
{
  int_f ret_value = -1;
  H5O_type_t obj_type_c;

  /*
   * Call H5Rget_obj_type function.
   */
  if((H5Rget_obj_type2((hid_t)*loc_id, (H5R_type_t)*ref_type, ref, &obj_type_c)) < 0)
    return ret_value;

  *obj_type = (int_f)obj_type_c;

  ret_value = 0;
  return ret_value;
}
