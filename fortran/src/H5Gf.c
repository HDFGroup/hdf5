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

/* This file contains C stubs for H5G Fortran APIs */


#include "H5f90.h"

/*----------------------------------------------------------------------------
 * Name:        h5gcreate_c
 * Purpose:     Call H5Gcreate to create a group 
 * Inputs:      loc_id - file or group identifier 
 *              name - name of the group     
 *              namelen - name length
 *              size_hint - length of names in the group
 * Outputs:     grp_id - group identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 5, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5gcreate_c (hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *size_hint,  hid_t_f *grp_id)
{
     int ret_value = -1;
     char *c_name;
     int c_namelen;
     size_t c_size_hint;
     hid_t c_grp_id;
     hid_t c_loc_id;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;
     /*
      * Call H5Gcreate function.
      */
     c_loc_id = *loc_id;
     if ( *size_hint == OBJECT_NAMELEN_DEFAULT_F ) 
     c_grp_id = H5Gcreate(c_loc_id, c_name, NULL);
     else {
          c_size_hint = *size_hint; 
          c_grp_id = H5Gcreate(c_loc_id, c_name, c_size_hint);
     }
     if (c_grp_id < 0) return ret_value;
     *grp_id = (hid_t_f)c_grp_id;
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5gopen_c
 * Purpose:     Call H5Gopen to open a dataset 
 * Inputs:      loc_id - file or group identifier 
 *              name - name of the group     
 *              namelen - name length
 * Outputs:     grp_id - group identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 5, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5gopen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *grp_id)
{
     int ret_value = -1;
     char *c_name;
     int c_namelen;
     hid_t c_grp_id;
     hid_t c_loc_id;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Gopen function.
      */
     c_loc_id = *loc_id;
     c_grp_id = H5Gopen(c_loc_id, c_name);

     HDfree(c_name);
     if (c_grp_id < 0) return ret_value;
     *grp_id = (hid_t_f)c_grp_id;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5gget_obj_info_idx_c
 * Purpose:     Call H5Gget_obj_info to return name and the type of group
 *              member 
 * Inputs:      loc_id - file or group identifier 
 *              name - name of the group     
 *              namelen - name length
 *              idx - index of the group member
 * Outputs:     obj_name - buffer to store member's name
 *              obj_namelen - length of the buffer
 *              obj_type - type of the object 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 5, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5gget_obj_info_idx_c 
(hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *idx, _fcd obj_name, int_f *obj_namelen, int_f *obj_type) 
{
     int ret_value = -1;
     hid_t c_loc_id;
     char *c_name;
     int c_namelen;
     int c_obj_namelen;
     char *c_obj_name = NULL;
     int type;
     int c_idx;
     herr_t c_ret_value;
     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;

     /*
      * Allocate buffer to hold name of the object
      */
     if (*obj_namelen) c_obj_name = (char *)HDmalloc(*obj_namelen + 1); 
     if (c_obj_name == NULL) return ret_value;
     /*
      * Call H5Gget_obj_info_idx function.
      */
      c_loc_id = *loc_id;
      c_idx = *idx;
      c_ret_value = H5Gget_obj_info_idx(c_loc_id, c_name, c_idx, &c_obj_name, (size_t)*obj_namelen, &type);

     if (c_ret_value < 0) {
                            HDfree(c_obj_name);
                            return ret_value;
                          }
     *obj_type = type;
/*
     switch (type) {
     case H5G_LINK:
          *obj_type = H5G_LINK_F;
           break;

     case H5G_GROUP:
          *obj_type = H5G_GROUP_F;
           break;

     case H5G_DATASET:
          *obj_type = H5G_DATASET_F;
           break;

     case H5G_TYPE:
          *obj_type = H5G_TYPE_F;
           break;
     default:
           return ret_value;
     }
*/
     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
     c_obj_namelen = *obj_namelen;
     HD5packFstring(c_obj_name, _fcdtocp(obj_name), c_obj_namelen);        
     if (c_obj_name) HDfree(c_obj_name);
     if (c_name) HDfree(c_name);

     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5gn_members_c
 * Purpose:     Call H5Gn_members to find number of objects in the group 
 * Inputs:      loc_id - file or group identifier 
 *              name - name of the group     
 *              namelen - name length
 * Outputs:     nmemebers - number of members 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 5, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5gn_members_c (hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *nmembers)
{
     int ret_value = -1;
     hid_t c_loc_id;
     char *c_name;
     int c_namelen;
     int c_nmembers; 

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Gn_members function.
      */
     c_loc_id = *loc_id;
     c_nmembers = H5Gn_members(c_loc_id, c_name);

     HDfree(c_name);
     if (c_nmembers < 0) return ret_value;
     *nmembers = (int_f)c_nmembers;
     ret_value = 0;
     return ret_value;
}      
/*----------------------------------------------------------------------------
 * Name:        h5gclose_c
 * Purpose:     Call H5Gclose to close the group 
 * Inputs:      grp_id - identifier of the group to be closed
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Wednesday, August 5, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5gclose_c ( hid_t_f *grp_id )
{
  int ret_value = 0;
  hid_t c_grp_id;
  
  c_grp_id = *grp_id;
  if ( H5Gclose(c_grp_id) < 0  ) ret_value = -1;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5glink_c
 * Purpose:     Call H5Glink to link the specified type 
 * Inputs:      loc_id - identifier of file or group 
 *              link_type - link type
 *              current_name - name of the existing object for hard link,
 *                             anything for the soft link
 *              current_namelen - current name lenghth
 *              new_name - new name for the object
 *              new_namelen - new_name lenghth
 * Returns:     0 on success, -1 on failure
 * Programmer:  Mingshi Chen
 *              Friday, August 6, 1999
 * Modifications: Elena Pourmal
 *---------------------------------------------------------------------------*/

int_f
nh5glink_c(hid_t_f *loc_id, int_f *link_type, _fcd current_name, int_f *current_namelen, _fcd new_name, int_f *new_namelen)
{
  int ret_value = -1;
  hid_t c_loc_id;
  H5G_link_t c_link_type;
  char *c_current_name, *c_new_name;
  int c_current_namelen, c_new_namelen;
  herr_t c_ret_value;
  /*
   *  Convert Fortran name to C name
   */
  c_current_namelen =*current_namelen;
  c_new_namelen =*new_namelen;
  c_current_name = (char *)HD5f2cstring(current_name, c_current_namelen);
  c_new_name = (char *)HD5f2cstring(new_name, c_new_namelen);
  if((c_current_name == NULL)||(c_new_name == NULL)) 
    return ret_value;
  /*
   *  Call H5Glink function
   */
  c_loc_id = *loc_id;
  c_link_type = (H5G_link_t)*link_type;
  c_ret_value = H5Glink(c_loc_id, c_link_type, c_current_name, c_new_name);
  if(c_current_name) HDfree(c_current_name);
  if(c_new_name) HDfree(c_new_name);
  if(c_ret_value < 0) return ret_value;

  ret_value = 0;
  return ret_value ;
}

/*----------------------------------------------------------------------------
 * Name:        h5gunlink_c
 * Purpose:     Call H5Gunlink to remove  the specified name 
 * Inputs:      loc_id - identifier of file or group             
 *              name - name of the object to unlink
 * Returns:     0 on success, -1 on failure
 * Programmer:  Mingshi Chen
 *              Friday, August 6, 1999
 * Modifications: Elena Pourmal
 *---------------------------------------------------------------------------*/

int_f
nh5gunlink_c(hid_t_f *loc_id, _fcd name, int_f *namelen)
{
  int ret_value = -1;
  hid_t c_loc_id;
  char *c_name;
  int c_namelen;
  herr_t c_ret_value;
  /*
   *  Convert Fortran name to C name
   */
  c_namelen = *namelen;
  c_name = (char *)HD5f2cstring(name, c_namelen);
  if(c_name == NULL) return ret_value;
  /*
   *  Call H5Gunlink function
   */
  c_loc_id = *loc_id;
  c_ret_value = H5Gunlink(c_loc_id, c_name);
  if(c_name) HDfree(c_name);
  if(c_ret_value < 0) return ret_value;
  ret_value = 0;
  return ret_value ;
}

/*----------------------------------------------------------------------------
 * Name:        h5gmove_c
 * Purpose:     Call H5Gmove to rename an object within an HDF5 file
 * Inputs:      loc_id - identifier of file or group 
 *              src_name - name of the original object 
 *              src_namelen - original name lenghth
 *              dst_name - new name for the object
 *              dst_namelen - new name lenghth
 * Returns:     0 on success, -1 on failure
 * Programmer:  Mingshi Chen
 *              Friday, August 6, 1999
 * Modifications: Elena Pourmal
 *---------------------------------------------------------------------------*/

int_f
nh5gmove_c(hid_t_f *loc_id, _fcd src_name, int_f *src_namelen, _fcd dst_name, int_f*dst_namelen)
{
  int ret_value = -1;
  hid_t c_loc_id;
  char *c_src_name, *c_dst_name;
  int c_src_namelen, c_dst_namelen;
  herr_t c_ret_value;
  /*
   *  Convert Fortran name to C name
   */
  c_src_namelen = *src_namelen;
  c_dst_namelen = *dst_namelen;
  c_src_name = (char *)HD5f2cstring(src_name, c_src_namelen);
  c_dst_name = (char *)HD5f2cstring(dst_name, c_dst_namelen);
  if((c_src_name == NULL)||(c_dst_name == NULL)) 
    return ret_value;
  /*
   *  Call H5Gmove function
   */
  c_loc_id = *loc_id;
  c_ret_value = H5Gmove(c_loc_id, c_src_name, c_dst_name);
  if(c_src_name) HDfree(c_src_name);
  if(c_dst_name) HDfree(c_dst_name);
  if(c_ret_value < 0) return ret_value;

  ret_value = 0;
  return ret_value ;
}

/*----------------------------------------------------------------------------
 * Name:        h5gget_linkval_c
 * Purpose:     Call H5Gget_linkval to return the name of object
 * Inputs:      loc_id - identifier of file or group 
 *              name - name of the object that symbolic link points to 
 *              namelen - the name lenghth
 *              size - lenghth of retrurned value
 * Outputs:     value - name to be returned
 * Returns:     0 on success, -1 on failure
 * Programmer:  Mingshi Chen
 *              Friday, August 6, 1999
 * Modifications: Elena Pourmal
 *---------------------------------------------------------------------------*/

int_f
nh5gget_linkval_c(hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *size, _fcd value )
{
  int ret_value = -1;
  hid_t c_loc_id;
  char *c_name;
  int c_namelen;
  char *c_value = NULL;
  size_t c_size;
  herr_t c_ret_value;
  /*
   *  Convert Fortran name to C name
   */
  c_namelen = *namelen;
  c_name = (char *)HD5f2cstring(name, c_namelen);
  if(c_name == NULL) return ret_value;

  /*
   *  Allocate buffer to hold name of the value
   */
  if(*size) c_value = (char *)HDmalloc(*size);
  if(c_value == NULL) {
                     HDfree(c_name); 
                     return ret_value;
                     }
  
  /*
   *  Call H5Gget_linkval function
   */

  c_size = (size_t)*size;
  c_loc_id = *loc_id;
  c_ret_value = H5Gget_linkval(c_loc_id, c_name, c_size, c_value);
  if(c_ret_value < 0) {
                       if(c_value) HDfree(c_value);
                       if(c_name) HDfree(c_name);
                       return ret_value;
                       }

  /*
   *  Convert C name to FORTRAN and place it in the given buffer
   */
  HD5packFstring(c_value, _fcdtocp(value), (int)*size);

  if(c_value) HDfree(c_value);
  if(c_name) HDfree(c_name);

  ret_value = 0;
  return ret_value ;
}

/*----------------------------------------------------------------------------
 * Name:        h5gset_comment_c
 * Purpose:     Call H5Gset_comment to set comments for the specified object
 * Inputs:      loc_id - identifier of file or group 
 *              name - name of object whose comment is to be set or reset 
 *              namelen - the name lenghth
 *              comment - the new comment
 *              commentlen - new comment lenghth
 * Returns:     0 on success, -1 on failure
 * Programmer:  Mingshi Chen
 *              Friday, August 6, 1999
 * Modifications: Elena Pourmal
 *---------------------------------------------------------------------------*/

int_f
nh5gset_comment_c(hid_t_f *loc_id, _fcd name, int_f *namelen, _fcd comment, int_f*commentlen)
{
  int ret_value = -1;
  hid_t c_loc_id;
  char *c_name, *c_comment;
  int c_namelen, c_commentlen;
  herr_t c_ret_value;
  /*
   *  Convert Fortran name to C name
   */
  c_namelen = *namelen;
  c_commentlen =*commentlen;
  c_name = (char *)HD5f2cstring(name, c_namelen);
  c_comment = (char *)HD5f2cstring(comment, c_commentlen);
  if((c_name == NULL)||(c_comment == NULL)) 
    return ret_value;
  /*
   *  Call H5Gset_comment function
   */
  c_loc_id = *loc_id;
  c_ret_value = H5Gset_comment(c_loc_id, c_name, c_comment);
  if(c_name) HDfree(c_name);
  if(c_comment) HDfree(c_comment);
  if(c_ret_value < 0) return ret_value;

  ret_value = 0;
  return ret_value ;
}


/*----------------------------------------------------------------------------
 * Name:        h5gget_comment_c
 * Purpose:     Call H5Gget_comment to retrieve comments for the specified object
 * Inputs:      loc_id - identifier of file or group 
 *              name - name of object whose comment is to be set or reset 
 *              namelen - the name lenghth
 *              bufsize - at most bufsize characters
 *              comment - the new comment
 * Returns:     0 on success, -1 on failure
 * Programmer:  Mingshi Chen
 *              Friday, August 6, 1999
 * Modifications: Elena Pourmal
 *---------------------------------------------------------------------------*/

int_f
nh5gget_comment_c(hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *bufsize, _fcd comment)
{
  int ret_value = -1;
  hid_t c_loc_id;
  char *c_name;
  int c_namelen;
  char *c_comment = NULL;
  size_t c_bufsize;
  herr_t c_ret_value;
  
  /*
   *  Convert Fortran name to C name
   */
  c_namelen = *namelen;
  c_name = (char *)HD5f2cstring(name, c_namelen);
  if(c_name == NULL)  return ret_value;

  /*
   *  Allocate buffer to hold the comment
   */
  c_bufsize = (size_t)*bufsize;
  if(c_bufsize) c_comment = (char *)malloc(c_bufsize);
  if(c_comment == NULL) {
                        HDfree(c_name);
                        return ret_value;
                        }
  
  /*
   *  Call H5Gget_comment function
   */
  c_loc_id = *loc_id;
  c_ret_value = H5Gget_comment(c_loc_id, c_name, c_bufsize, c_comment);
  if(c_ret_value < 0) {
                       HDfree(c_name);
                       HDfree(c_comment);
                       return ret_value;
                      }

  /*
   *  Convert C name to FORTRAN and place it in the given buffer
   */
  HD5packFstring(c_comment, _fcdtocp(comment), (int)*bufsize);

  if(c_name) HDfree(c_name);
  if(c_comment) HDfree(c_comment);

  ret_value = 0;
  return ret_value ;
}
