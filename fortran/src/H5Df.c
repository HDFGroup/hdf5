/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  * Copyright by the Board of Trustees of the University of Illinois.         *
  * All rights reserved.                                                      *
  *                                                                           *
  * This file is part of HDF5.  The full HDF5 copyright notice, including     *
  * terms governing use, modification, and redistribution, is contained in    *
  * the files COPYING and Copyright.html.  COPYING can be found at the root   *
  * of the source code distribution tree; Copyright.html can be found at the  *
  * root level of an installed copy of the electronic HDF5 document set and   *
  * is linked from the top-level documents page.  It can also be found at     *
  * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
  * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This file contains C stubs for H5D Fortran APIs */


#include "H5f90.h"

/*----------------------------------------------------------------------------
 * Name:        h5dcreate_c
 * Purpose:     Call H5Dcreate to create a dataset 
 * Inputs:      loc_id - file or group identifier 
 *              name - name of the dataset     
 *              namelen - name length
 *              type_id - datatype identifier
 *              space_id - dataspace identifier
 *              crt_pr  - identifier of creation property list
 * Outputs:     dset_id - dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 4, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dcreate_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *space_id, hid_t_f *crt_prp,  hid_t_f *dset_id)
{
     int ret_value = -1;
     char *c_name;
     int c_namelen;
     hid_t c_loc_id;
     hid_t c_type_id;
     hid_t c_space_id;
     hid_t c_dset_id;
     hid_t c_crt_prp;
     
     /*
      * Define creation property
      */
     c_crt_prp = (hid_t)*crt_prp;
/*
     if ( H5P_DEFAULT_F == c_crt_prp ) c_crt_prp = H5P_DEFAULT;
*/

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Dcreate function.
      */
     c_loc_id = *loc_id;
     c_type_id = *type_id;
     c_space_id = *space_id;
     c_dset_id = H5Dcreate(c_loc_id, c_name, c_type_id, c_space_id, c_crt_prp);
     if (c_dset_id < 0) return ret_value;
     *dset_id = (hid_t_f)c_dset_id;
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5dopen_c
 * Purpose:     Call H5Dopen to open a dataset 
 * Inputs:      loc_id - file or group identifier 
 *              name - name of the dataset     
 *              namelen - name length
 * Outputs:     dset_id - dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 4, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dopen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *dset_id)
{
     int ret_value = -1;
     char *c_name;
     int c_namelen;
     hid_t c_loc_id;
     hid_t c_dset_id;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Dopen function.
      */
     c_loc_id = *loc_id;
     c_dset_id = H5Dopen(c_loc_id, c_name);

     if (c_dset_id < 0) return ret_value;
     *dset_id = (hid_t_f)c_dset_id;
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5dwritec_c
 * Purpose:     Call h5dwrite_c to write a dataset of characters 
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              mem_space_id - memory dataspace identifier
 *              file_space_id - memory dataspace identifier
 *              xfer_pr  - identifier of transfer property list
 *              buf      - character data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 6, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dwritec_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, int_f *dims)
{
     int ret_value = -1;
     
     /*
      * Call h5dwrite_c  function.
      */
     ret_value = nh5dwrite_c(dset_id, mem_type_id, mem_space_id, file_space_id, xfer_prp, _fcdtocp(buf), dims);

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5dwrite_c
 * Purpose:     Call H5Dwrite to write a dataset 
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              mem_space_id - memory dataspace identifier
 *              file_space_id - memory dataspace identifier
 *              xfer_pr  - identifier of transfer property list
 *              buf      - data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 6, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dwrite_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, int_f *dims)
{
     int ret_value = -1;
     herr_t ret;
     hid_t c_dset_id;
     hid_t c_mem_type_id;
     hid_t c_mem_space_id;
     hid_t c_file_space_id;
     hid_t c_xfer_prp;

     /*
      * Define transfer property
      */
     c_xfer_prp = (hid_t)*xfer_prp;
/*
     if ( H5P_DEFAULT_F == c_xfer_prp ) c_xfer_prp = H5P_DEFAULT;
*/
     
     /*
      * Call H5Dwrite function.
      */
     c_dset_id = *dset_id;
     c_mem_type_id = *mem_type_id;
     c_mem_space_id = *mem_space_id;
     c_file_space_id = *file_space_id;
     ret = H5Dwrite(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5dwrite_ref_obj_c
 * Purpose:     Call H5Dwrite to write a dataset  of object references
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              mem_space_id - memory dataspace identifier
 *              file_space_id - memory dataspace identifier
 *              xfer_pr  - identifier of transfer property list
 *              buf      - data buffer with references to the objects.
 *              n - number of references to be stored.   
 * Returns:     0 on success,e-1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, July 24, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dwrite_ref_obj_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, int_f *dims)
{
     int ret_value = -1;
     herr_t ret;
     hid_t c_dset_id;
     hid_t c_mem_type_id;
     hid_t c_mem_space_id;
     hid_t c_file_space_id;
     hid_t c_xfer_prp;
     hobj_ref_t *buf_c;
     int i, n;
     n = (int)*dims;

     /*
      * Define transfer property
      */
     c_xfer_prp = (hid_t)*xfer_prp;
/*
     if ( H5P_DEFAULT_F == c_xfer_prp ) c_xfer_prp = H5P_DEFAULT;
*/

     /*
      * Allocate temporary buffer and copy references from Fortran.
      */
      buf_c = (hobj_ref_t*)HDmalloc(sizeof(hobj_ref_t)*(n));
      if ( buf_c != NULL ) {
      for (i = 0; i < n; i++) {
           HDmemcpy(buf_c[i].oid, buf, H5R_OBJ_REF_BUF_SIZE);
           buf = buf + REF_OBJ_BUF_LEN_F;
      }
      }  
      else return ret_value;
     
     /*
      * Call H5Dwrite function.
      */
     c_dset_id = (hid_t)*dset_id;
     c_mem_type_id = (hid_t)*mem_type_id;
     c_mem_space_id = (hid_t)*mem_space_id;
     c_file_space_id = (hid_t)*file_space_id;
     ret = H5Dwrite(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf_c);
     HDfree(buf_c);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5dwrite_ref_reg_c
 * Purpose:     Call H5Dwrite to write a dataset of dataset region references
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              mem_space_id - memory dataspace identifier
 *              file_space_id - memory dataspace identifier
 *              xfer_pr  - identifier of transfer property list
 *              buf      - data buffer with references to the objects.
 *              n - number of references to be stored.   
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, July 24, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dwrite_ref_reg_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, int_f *dims)
{
     int ret_value = -1;
     herr_t ret;
     hid_t c_dset_id;
     hid_t c_mem_type_id;
     hid_t c_mem_space_id;
     hid_t c_file_space_id;
     hid_t c_xfer_prp;
     hdset_reg_ref_t *buf_c;
     int i, n;

      n = (int)*dims;
     /*
      * Define transfer property
      */
     c_xfer_prp = (hid_t)*xfer_prp;
/*
     if ( H5P_DEFAULT_F == c_xfer_prp ) c_xfer_prp = H5P_DEFAULT;
*/
     
     /*
      * Allocate temporary buffer and copy references from Fortran.
      */
      buf_c = (hdset_reg_ref_t *)HDmalloc(sizeof(hdset_reg_ref_t)*(n));
      if ( buf_c != NULL ) {
      for (i = 0; i < n; i++) {
           HDmemcpy(buf_c[i].heapid, buf, H5R_DSET_REG_REF_BUF_SIZE);
           buf = buf + REF_REG_BUF_LEN_F;
      }
      }  
      else return ret_value;
        
     
     /*
      * Call H5Dwrite function.
      */
     c_dset_id = (hid_t)*dset_id;
     c_mem_type_id = (hid_t)*mem_type_id;
     c_mem_space_id = (hid_t)*mem_space_id;
     c_file_space_id = (hid_t)*file_space_id;
     ret = H5Dwrite(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf_c);
     HDfree(buf_c);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      


/*----------------------------------------------------------------------------
 * Name:        h5dreadc_c
 * Purpose:     Call h5dread_c to read a dataset of characters 
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              mem_space_id - memory dataspace identifier
 *              file_space_id - memory dataspace identifier
 *              xfer_pr  - identifier of transfer property list
 * Outputs:     buf      - character data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, August 9, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dreadc_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, int_f *dims)
{
     int ret_value = -1;
     
     /*
      * Call h5dread_c  function.
      */
     ret_value = nh5dread_c(dset_id, mem_type_id, mem_space_id, file_space_id, xfer_prp, _fcdtocp(buf), dims);

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5dread_c
 * Purpose:     Call H5Draed to read a dataset 
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              mem_space_id - memory dataspace identifier
 *              file_space_id - memory dataspace identifier
 *              xfer_pr  - identifier of transfer property list
 * Outputs:     buf      - data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, August 9, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dread_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, int_f *dims)
{
     int ret_value = -1;
     herr_t ret;
     hid_t c_dset_id;
     hid_t c_mem_type_id;
     hid_t c_mem_space_id;
     hid_t c_file_space_id;
     hid_t c_xfer_prp;

     /*
      * Define transfer property
      */
     c_xfer_prp = (hid_t)*xfer_prp;
/*
     if ( H5P_DEFAULT_F == c_xfer_prp ) c_xfer_prp = H5P_DEFAULT;
*/
     
     /*
      * Call H5Dread function.
      */
     c_dset_id = *dset_id;
     c_mem_type_id = *mem_type_id;
     c_mem_space_id = *mem_space_id;
     c_file_space_id = *file_space_id;
     ret = H5Dread(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5dread_ref_obj_c
 * Purpose:     Call H5Dread to read a dataset  of object references
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              mem_space_id - memory dataspace identifier
 *              file_space_id - memory dataspace identifier
 *              xfer_pr  - identifier of transfer property list
 *              buf      - data buffer to store references to the objects.
 *              n - number of references to be stored.   
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, July 24, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dread_ref_obj_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f * buf, int_f *dims)
{
     int ret_value = -1;
     herr_t ret;
     hid_t c_dset_id;
     hid_t c_mem_type_id;
     hid_t c_mem_space_id;
     hid_t c_file_space_id;
     hid_t c_xfer_prp;
     hobj_ref_t *buf_c;
     int i, n;
     n = (int)*dims;
     /*
      * Define transfer property
      */
     c_xfer_prp = (hid_t)*xfer_prp;
/*
     if ( H5P_DEFAULT_F == c_xfer_prp ) c_xfer_prp = H5P_DEFAULT;
*/
     
     /*
      * Allocate temporary buffer.
      */
     buf_c = (hobj_ref_t*)HDmalloc(sizeof(hobj_ref_t)*(n));
     if ( buf_c != NULL ) {
     /*
      * Call H5Dread function.
      */
     c_dset_id = (hid_t)*dset_id;
     c_mem_type_id = (hid_t)*mem_type_id;
     c_mem_space_id = (hid_t)*mem_space_id;
     c_file_space_id = (hid_t)*file_space_id;
     ret = H5Dread(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf_c);
     if (ret >=0) {
        for (i = 0; i < n; i++) {
           HDmemcpy(buf, buf_c[i].oid, H5R_OBJ_REF_BUF_SIZE);
           buf = buf + REF_OBJ_BUF_LEN_F;
        }  
     }
     if ( buf_c != NULL ) HDfree(buf_c);
     } 
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5dread_ref_reg_c
 * Purpose:     Call H5Dread to read a dataset of dataset region references
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              mem_space_id - memory dataspace identifier
 *              file_space_id - memory dataspace identifier
 *              xfer_pr  - identifier of transfer property list
 *              buf      - data buffer to store references to the objects.
 *              n - number of references to be stored.   
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Monday, July 24, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5dread_ref_reg_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f * buf, int_f *dims)
{
     int ret_value = -1;
     herr_t ret;
     hid_t c_dset_id;
     hid_t c_mem_type_id;
     hid_t c_mem_space_id;
     hid_t c_file_space_id;
     hid_t c_xfer_prp;
     hdset_reg_ref_t *buf_c;
     int i, n;
     n = (int)*dims;
     /*
      * Define transfer property
      */
     c_xfer_prp = (hid_t)*xfer_prp;
/*
     if ( H5P_DEFAULT_F == c_xfer_prp ) c_xfer_prp = H5P_DEFAULT;
*/
     
     /*
      * Allocate temporary buffer.
      */
     buf_c = (hdset_reg_ref_t *)HDmalloc(sizeof(hdset_reg_ref_t)*(n));
     if ( buf_c != NULL ) {
     /*
      * Call H5Dread function.
      */
     c_dset_id = (hid_t)*dset_id;
     c_mem_type_id = (hid_t)*mem_type_id;
     c_mem_space_id = (hid_t)*mem_space_id;
     c_file_space_id = (hid_t)*file_space_id;
     ret = H5Dread(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf_c);
     if (ret >=0) {
        for (i = 0; i < n; i++) {
           HDmemcpy(buf, buf_c[i].heapid, H5R_DSET_REG_REF_BUF_SIZE);
           buf = buf + REF_REG_BUF_LEN_F;
        }  
     }
     if ( buf_c != NULL ) HDfree(buf_c);
     } 
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      


/*----------------------------------------------------------------------------
 * Name:        h5dclose_c
 * Purpose:     Call H5Dclose to close a dataset 
 * Inputs:      dset_id - identifier of the dataset to be closed
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 4, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5dclose_c ( hid_t_f *dset_id )
{
  int ret_value = 0;
  hid_t c_dset_id;
  c_dset_id = *dset_id;
  if ( H5Dclose(c_dset_id) < 0  ) ret_value = -1;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5dget_space_c
 * Purpose:     Call H5Dget_space to obtain dataspace of a dataset 
 * Inputs:      dset_id - identifier of the dataset
 * Outputs:     space_id - identifier of the dataset's dataspace
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 19, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5dget_space_c ( hid_t_f *dset_id , hid_t_f *space_id)
{
  int ret_value = -1;
  hid_t c_dset_id;
  hid_t c_space_id;
  
  c_dset_id = *dset_id; 
  c_space_id = H5Dget_space(c_dset_id);
  if(c_space_id < 0 ) return ret_value;
  ret_value = 0;
  *space_id = (hid_t_f)c_space_id;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5dget_type_c
 * Purpose:     Call H5Dget_type to obtain datatype of a dataset 
 * Inputs:      dset_id - identifier of the dataset
 * Outputs:     type_id - identifier of the dataset's datatype
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 19, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5dget_type_c ( hid_t_f *dset_id , hid_t_f *type_id)
{
  int ret_value = -1;
  hid_t c_dset_id;
  hid_t c_type_id;
  
  c_dset_id = *dset_id;
  c_type_id = H5Dget_type(c_dset_id);

  if(c_type_id < 0 ) return ret_value;

  *type_id = (hid_t_f)c_type_id;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5dget_create_plist_c
 * Purpose:     Call H5Dget_create_plist to obtain creation property list
 *              of a dataset 
 * Inputs:      dset_id - identifier of the dataset
 * Outputs:     plist_id - identifier of he dataset creation property list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 19, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5dget_create_plist_c ( hid_t_f *dset_id , hid_t_f *plist_id)
{
  int ret_value = -1;
  hid_t c_dset_id;
  hid_t c_plist_id;
  
  c_dset_id = *dset_id;
  c_plist_id = H5Dget_create_plist(c_dset_id);

  if(c_plist_id < 0 ) return ret_value;

  ret_value = 0;
  *plist_id = (hid_t_f)c_plist_id;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5dextend_c
 * Purpose:     Call H5Dextend to extend dataset with unlimited dimensions 
 * Inputs:      dset_id - identifier of the dataset 
 * Outputs:     dims - array with the dimension sizes 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 19, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5dextend_c ( hid_t_f *dset_id , hsize_t_f *dims)
{
  int ret_value = -1;
  hsize_t *c_dims;
  int status;
  int rank;
  int i;
  hid_t c_dset_id;
  hid_t c_space_id;

  c_dset_id = *dset_id;
  c_space_id = H5Dget_space(c_dset_id);
  if (c_space_id < 0) return ret_value;

  rank = H5Sget_simple_extent_ndims(c_space_id);
  if (rank < 0) return ret_value;

  c_dims = malloc(sizeof(hsize_t)*rank);
  if (!c_dims) return ret_value;

  /*
   * Reverse dimensions due to C-FORTRAN storage order.
   */
  for (i=0; i < rank; i++) 
      c_dims[i] = dims[rank - i - 1];
  
  status = H5Dextend(c_dset_id, c_dims);

  if ( status >= 0  ) ret_value = 0;
  HDfree(c_dims);
  return ret_value;
}

