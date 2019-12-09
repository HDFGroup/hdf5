/****h* H5Of/H5Of
 * PURPOSE
 *  This file contains C stubs for H5O Fortran APIs
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
int_f
fill_h5o_info_t_f(H5O_info_t Oinfo, H5O_info_t_f *object_info);

int_f
fill_h5o_info_t_f(H5O_info_t Oinfo, H5O_info_t_f *object_info) {

  /* This function does not used the field parameter because we want 
   * this function to fill the unfilled fields with C's default values.
   */

  struct tm *ts;

  object_info->fileno    = Oinfo.fileno;
  object_info->addr      = (haddr_t_f)Oinfo.addr;

  object_info->type      = (int_f)Oinfo.type;
  object_info->rc        = (int_f)Oinfo.rc;

  ts = HDgmtime(&Oinfo.atime);

  object_info->atime[0]     = (int_f)ts->tm_year+1900; /* year starts at 1900 */
  object_info->atime[1]     = (int_f)ts->tm_mon+1; /* month starts at 0 in C */
  object_info->atime[2]     = (int_f)ts->tm_mday;
  object_info->atime[3]     = 0; /* time is expressed as UTC (or GMT timezone) */
  object_info->atime[4]     = (int_f)ts->tm_hour;
  object_info->atime[5]     = (int_f)ts->tm_min;
  object_info->atime[6]     = (int_f)ts->tm_sec;
  object_info->atime[7]     = -32767; /* millisecond is not available, assign it -HUGE(0) */

  ts = HDgmtime(&Oinfo.btime);

  object_info->btime[0]     = (int_f)ts->tm_year+1900; /* year starts at 1900 */
  object_info->btime[1]     = (int_f)ts->tm_mon+1; /* month starts at 0 in C */
  object_info->btime[2]     = (int_f)ts->tm_mday;
  object_info->btime[3]     = 0; /* time is expressed as UTC (or GMT timezone) */
  object_info->btime[4]     = (int_f)ts->tm_hour;
  object_info->btime[5]     = (int_f)ts->tm_min;
  object_info->btime[6]     = (int_f)ts->tm_sec;
  object_info->btime[7]     = -32767; /* millisecond is not available, assign it -HUGE(0) */

  ts = HDgmtime(&Oinfo.ctime);

  object_info->ctime[0]     = (int_f)ts->tm_year+1900; /* year starts at 1900 */
  object_info->ctime[1]     = (int_f)ts->tm_mon+1; /* month starts at 0 in C */
  object_info->ctime[2]     = (int_f)ts->tm_mday;
  object_info->ctime[3]     = 0; /* time is expressed as UTC (or GMT timezone) */
  object_info->ctime[4]     = (int_f)ts->tm_hour;
  object_info->ctime[5]     = (int_f)ts->tm_min;
  object_info->ctime[6]     = (int_f)ts->tm_sec;
  object_info->ctime[7]     = -32767; /* millisecond is not available, assign it -HUGE(0) */

  ts = HDgmtime(&Oinfo.mtime);

  object_info->mtime[0]     = (int_f)ts->tm_year+1900; /* year starts at 1900 */
  object_info->mtime[1]     = (int_f)ts->tm_mon+1; /* month starts at 0 in C */
  object_info->mtime[2]     = (int_f)ts->tm_mday;
  object_info->mtime[3]     = 0; /* time is expressed as UTC (or GMT timezone) */
  object_info->mtime[4]     = (int_f)ts->tm_hour;
  object_info->mtime[5]     = (int_f)ts->tm_min;
  object_info->mtime[6]     = (int_f)ts->tm_sec;
  object_info->mtime[7]     = -32767; /* millisecond is not available, assign it -HUGE(0) */

  object_info->num_attrs = (hsize_t_f)Oinfo.num_attrs;

  object_info->hdr.version = (int_f)Oinfo.hdr.version;
  object_info->hdr.nmesgs  = (int_f)Oinfo.hdr.nmesgs;
  object_info->hdr.nchunks = (int_f)Oinfo.hdr.nchunks;
  object_info->hdr.flags   = (int_f)Oinfo.hdr.flags;

  object_info->hdr.space.total = (hsize_t_f)Oinfo.hdr.space.total;
  object_info->hdr.space.meta  = (hsize_t_f)Oinfo.hdr.space.meta;
  object_info->hdr.space.mesg  = (hsize_t_f)Oinfo.hdr.space.mesg;
  object_info->hdr.space.free  = (hsize_t_f)Oinfo.hdr.space.free;

  object_info->hdr.mesg.present = Oinfo.hdr.mesg.present;
  object_info->hdr.mesg.shared  = Oinfo.hdr.mesg.shared;

  object_info->meta_size.obj.index_size = (hsize_t_f)Oinfo.meta_size.obj.index_size;
  object_info->meta_size.obj.heap_size  = (hsize_t_f)Oinfo.meta_size.obj.heap_size;
  object_info->meta_size.attr.index_size = (hsize_t_f)Oinfo.meta_size.attr.index_size;
  object_info->meta_size.attr.heap_size  = (hsize_t_f)Oinfo.meta_size.attr.heap_size;

  return 0;

}

/****if* H5Of/h5olink_c
 * NAME
 *  h5olink_c
 * PURPOSE
 *  Calls H5Olink
 * INPUTS
 *  object_id        - Object to be linked.
 *  new_loc_id       - File or group identifier specifying location at which object is to be linked.
 *  name             - Name of link to be created, relative to new_loc_id.
 *  namelen          - Length of buffer for link to be created.
 *  lcpl_id          - Link creation property list identifier.
 *  lapl_id          - Link access property list identifier.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  April 21, 2008
 * SOURCE
*/
int_f
h5olink_c (hid_t_f *object_id, hid_t_f *new_loc_id, _fcd name, size_t_f *namelen,
            hid_t_f *lcpl_id, hid_t_f *lapl_id)
/******/
{
  char *c_name = NULL;          /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if( (c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Olink function.
   */
  if((hid_t_f)H5Olink((hid_t)*object_id, (hid_t)*new_loc_id, c_name,
                      (hid_t)*lcpl_id, (hid_t)*lapl_id) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_name)
    HDfree(c_name);
  return ret_value;
}

/****if* H5Of/h5oopen_c
 * NAME
 *  h5oopen_c
 * PURPOSE
 *  Calls H5Oopen
 * INPUTS
 *  loc_id  - File or group identifier
 *  name    - Attribute access property list
 *  namelen - Size of name
 *  lapl_id - Link access property list
 * OUTPUTS
 *  obj_id  - Dataset identifier
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  April 18, 2008
 * SOURCE
*/
int_f
h5oopen_c (hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id, hid_t_f *obj_id)
/******/
{
  char *c_name = NULL;          /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Oopen function.
   */
  if((*obj_id = (hid_t_f)H5Oopen((hid_t)*loc_id, c_name, (hid_t)*lapl_id)) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_name)
    HDfree(c_name);
  return ret_value;
}
/****if* H5Of/h5oclose_c
 * NAME
 *  h5oclose_c
 * PURPOSE
 *  Call H5Oclose
 * INPUTS
 *  object_id   - Object identifier  
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *   M. Scot Breitenfeld
 *  December 17, 2008
 * SOURCE
*/
int_f
h5oclose_c ( hid_t_f *object_id )
/******/
{
  int_f ret_value=0;          /* Return value */
  
  if (H5Oclose((hid_t)*object_id) < 0)
    HGOTO_DONE(FAIL);
  
 done:
  return ret_value;
}

/****if* H5Of/h5ovisit_c
 * NAME
 *  h5ovisit_c
 * PURPOSE
 *  Calls H5Ovisit
 * INPUTS
 *  object_id - Identifier specifying subject group
 *  index_type - Type of index which determines the order
 *  order - Order within index
 *  idx - Iteration position at which to start
 *  op - Callback function passing data regarding the link to the calling application
 *  op_data - User-defined pointer to data required by the application for its processing of the link
 *  fields - Flags specifying the fields to include in object_info.
 *
 * OUTPUTS
 *  idx - Position at which an interrupted iteration may be restarted
 *
 * RETURNS
 *     >0 on success, 0< on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  November 19, 2008
 * SOURCE
*/
int_f
h5ovisit_c(hid_t_f *group_id, int_f *index_type, int_f *order, H5O_iterate_t op, void *op_data, int_f *fields )
/******/
{
  int_f ret_value = -1;       /* Return value */
  herr_t func_ret_value; /* H5Linterate return value */

  /*
   * Call H5Ovisit2
   */

  func_ret_value = H5Ovisit2( (hid_t)*group_id, (H5_index_t)*index_type, (H5_iter_order_t)*order, op, op_data, (unsigned)*fields);

  ret_value = (int_f)func_ret_value;

  return ret_value;
}

/****if* H5Of/h5oopen_by_addr_c
 * NAME
 *  h5oopen_by_addr_c
 * PURPOSE
 *  Calls H5open_by_addr
 * INPUTS
 *  loc_id  - File or group identifier
 *  addr  - Object’s address in the file
 *
 * OUTPUTS
 *  obj_id  - Dataset identifier      
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  September 14, 2009
 * SOURCE
*/
int_f
h5oopen_by_addr_c (hid_t_f *loc_id, haddr_t_f *addr, hid_t_f *obj_id)
/******/
{
  int_f ret_value = 0;          /* Return value */

  /*
   * Call H5Oopen_by_address function.
   */
  if((*obj_id = (hid_t_f)H5Oopen_by_addr((hid_t)*loc_id, (haddr_t)*addr)) < 0)
    HGOTO_DONE(FAIL);

 done:
  return ret_value;
}

/****if* H5Of/H5Oget_info_by_name_c
 * NAME
 *  H5Oget_info_by_name_c
 * PURPOSE
 *  Calls H5Oget_info_by_name
 * INPUTS
 *  loc_id       - File or group identifier specifying location of group in which object is located.
 *  name         - Name of group, relative to loc_id.
 *  namelen      - Name length.
 *  lapl_id      - Link access property list.
 *  fields       - Flags specifying the fields to include in object_info.
 * OUTPUTS
 *  object_info  - Buffer in which to return object information.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  December 1, 2008
 * SOURCE
*/
int_f
h5oget_info_by_name_c (hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id,
                       H5O_info_t_f *object_info, int_f *fields)
/******/
{
  char *c_name = NULL;          /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */
  H5O_info_t Oinfo;
  
  /*
   * Convert FORTRAN name to C name
   */
  if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Oinfo_by_name function.
   */
  if(H5Oget_info_by_name2((hid_t)*loc_id, c_name,
                          &Oinfo, (unsigned)*fields, (hid_t)*lapl_id) < 0)
    HGOTO_DONE(FAIL);

  ret_value = fill_h5o_info_t_f(Oinfo, object_info);

 done:
  if(c_name)
    HDfree(c_name);
  return ret_value;
}

/****if* H5Of/H5Oget_info_by_idx_c
 * NAME
 *  H5Oget_info_by_idx_c
 * PURPOSE
 *  Calls H5Oget_info_by_idx
 * INPUTS
 *  loc_id       - File or group identifier specifying location of group in which object is located.
 *  name         - Name of group, relative to loc_id.
 *  namelen      - Name length.
 *  lapl_id      - Link access property list.
 * OUTPUTS
 *  object_info  - Buffer in which to return object information.
 *  fields       - Flags specifying the fields to include in object_info.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  December 1, 2008
 * SOURCE
*/
int_f
h5oget_info_by_idx_c (hid_t_f *loc_id, _fcd  group_name, size_t_f *namelen, 
                      int_f *index_field, int_f *order, hsize_t_f *n, hid_t_f *lapl_id, H5O_info_t_f *object_info, int_f *fields)
/******/
{
  char *c_group_name = NULL;     /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */
  H5O_info_t Oinfo;
  H5_index_t c_index_field;
  H5_iter_order_t c_order;
  
  /*
   * Convert FORTRAN name to C name
   */
  if((c_group_name = HD5f2cstring( group_name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  c_index_field = (H5_index_t)*index_field;
  c_order = (H5_iter_order_t)*order;

  /*
   * Call H5Oinfo_by_idx function.
   */
  if(H5Oget_info_by_idx2((hid_t)*loc_id, c_group_name, c_index_field, c_order, (hsize_t)*n,
                         &Oinfo, (unsigned)*fields, (hid_t)*lapl_id) < 0)
    HGOTO_DONE(FAIL);

  ret_value = fill_h5o_info_t_f(Oinfo,object_info);

 done:
  if(c_group_name)
    HDfree(c_group_name);
  return ret_value;
}

/****if* H5Of/H5Oget_info_c
 * NAME
 *  H5Oget_info_c
 * PURPOSE
 *  Calls H5Oget_info
 * INPUTS
 *  object_id   - Identifier for target object.
 *  fields      - Flags specifying the fields to include in object_info.
 * OUTPUTS
 *  object_info - Buffer in which to return object information.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  May 16, 2012
 * SOURCE
*/
int_f
h5oget_info_c (hid_t_f *object_id, H5O_info_t_f *object_info, int_f *fields)
/******/
{
  int_f ret_value = 0;          /* Return value */
  H5O_info_t Oinfo;
  
  /*
   * Call H5Oinfo_by_name function.
   */
  if(H5Oget_info2((hid_t)*object_id, &Oinfo, (unsigned)*fields) < 0)
    HGOTO_DONE(FAIL);

  ret_value = fill_h5o_info_t_f(Oinfo,object_info);

 done:
  return ret_value;
}

/* ***if* H5Of/H5Ocopy_c
 * NAME
 *  H5Ocopy_c
 * PURPOSE
 *  Calls H5Ocopy
 * INPUTS  
 *  src_loc_id   - Object identifier indicating the location of the source object to be copied 
 *  src_name     - Name of the source object to be copied 
 *  src_name_len - Length of src_name
 *  dst_loc_id   - Location identifier specifying the destination 
 *  dst_name     - Name to be assigned to the new copy 
 *  dst_name_len - Length of dst_name
 *  ocpypl_id    - Object copy property list
 *  lcpl_id      - Link creation property list for the new hard link
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  March 14, 2012
 * SOURCE
*/
int_f
h5ocopy_c (hid_t_f *src_loc_id, _fcd src_name, size_t_f *src_name_len,
           hid_t_f *dst_loc_id, _fcd dst_name, size_t_f *dst_name_len, 
           hid_t_f *ocpypl_id, hid_t_f *lcpl_id )
/******/
{
  char *c_src_name = NULL;  /* Buffer to hold C string */
  char *c_dst_name = NULL;  /* Buffer to hold C string */
  
  int_f ret_value = 0;      /* Return value */
  
  /*
   * Convert FORTRAN name to C name
   */
  if((c_src_name = HD5f2cstring(src_name, (size_t)*src_name_len)) == NULL)
    HGOTO_DONE(FAIL);
  if((c_dst_name = HD5f2cstring(dst_name, (size_t)*dst_name_len)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Ocopy function.
   */
  if(H5Ocopy( (hid_t)*src_loc_id, c_src_name, (hid_t)*dst_loc_id, c_dst_name, 
              (hid_t)*ocpypl_id, (hid_t)*lcpl_id) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_src_name)
    HDfree(c_src_name);
  if(c_dst_name)
    HDfree(c_dst_name);

  return ret_value;

}

/****if* H5Of/h5ovisit_by_name_c
 * NAME
 *  h5ovisit_by_name_c
 * PURPOSE
 *  Calls H5Ovisit_by_name
 * INPUTS
 *  object_id - Identifier specifying subject group
 *  index_type - Type of index which determines the order
 *  order - Order within index
 *  idx - Iteration position at which to start
 *  op - Callback function passing data regarding the link to the calling application
 *  op_data - User-defined pointer to data required by the application for its processing of the link
 *  fields - Flags specifying the fields to include in object_info.
 *
 * OUTPUTS
 *  idx - Position at which an interrupted iteration may be restarted
 *
 * RETURNS
 *  >0 on success, 0< on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  May 16, 2012
 * SOURCE
*/
int_f
h5ovisit_by_name_c(hid_t_f *loc_id,  _fcd object_name, size_t_f *namelen, int_f *index_type, int_f *order,
                   H5O_iterate_t op, void *op_data, hid_t_f *lapl_id, int_f *fields )
/******/
{
  int_f ret_value = -1;       /* Return value */
  herr_t func_ret_value;      /* H5Linterate return value */
  char *c_object_name = NULL; /* Buffer to hold C string */

  /*
   * Convert FORTRAN name to C name
   */
  if( (c_object_name = HD5f2cstring(object_name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Ovisit
   */
  func_ret_value = H5Ovisit_by_name2( (hid_t)*loc_id, c_object_name, (H5_index_t)*index_type, (H5_iter_order_t)*order,
                                      op, op_data, (unsigned)*fields, (hid_t)*lapl_id);
  ret_value = (int_f)func_ret_value;

 done:
  if(c_object_name)
    HDfree(c_object_name);
  return ret_value;

}

/****if* H5Of/h5odecr_refcount_c
 * NAME
 *  h5odecr_refcount_c
 * PURPOSE
 *  Calls H5Odecr_refcount
 * INPUTS
 *  object_id - Object identifier.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  May 16, 2012
 * SOURCE
*/
int_f
h5odecr_refcount_c (hid_t_f *object_id)
/******/
{
  int_f ret_value = 0;  /* Return value */

  /*
   * Call H5Odecr_refcount function.
   */
  if((hid_t_f)H5Odecr_refcount((hid_t)*object_id) < 0)
    HGOTO_DONE(FAIL);

 done:
  return ret_value;
}

/****if* H5Of/h5oexists_by_name_c
 * NAME
 *  h5oexists_by_name_c
 * PURPOSE
 *  Calls H5Oexists_by_name
 * INPUTS
 *  loc_id  - File or group identifier
 *  name    - Attribute access property list
 *  namelen - Size of name
 *  lapl_id - Link access property list
 *
 * RETURNS
 *  link status: 0 = false, 1 = true, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  May 17, 2012
 * SOURCE
*/
int_f
h5oexists_by_name_c (hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id)
/******/
{
  char *c_name = NULL;          /* Buffer to hold C string */
  int_f ret_value = 0;          /* Return value */

  /*
   * Convert FORTRAN name to C name
   */
  if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Oopen function.
   */
  if((ret_value = (int_f)H5Oexists_by_name((hid_t)*loc_id, c_name, (hid_t)*lapl_id)) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_name)
    HDfree(c_name);
  return ret_value;
}

/****if* H5Of/h5oincr_refcount_c
 * NAME
 *  h5oincr_refcount_c
 * PURPOSE
 *  Calls H5Oincr_refcount
 * INPUTS
 *  object_id - Object identifier.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  May 16, 2012
 * SOURCE
*/
int_f
h5oincr_refcount_c (hid_t_f *object_id)
/******/
{
  int_f ret_value = 0;  /* Return value */

  /*
   * Call H5Oincr_refcount function.
   */
  if((hid_t_f)H5Oincr_refcount((hid_t)*object_id) < 0)
    HGOTO_DONE(FAIL);

 done:
  return ret_value;
}

/****if* H5Of/h5oset_comment_c
 * NAME
 *  h5oset_comment_c
 * PURPOSE
 *  Calls H5Oset_comment
 * INPUTS
 *  object_id  - Identifier of the target object.
 *  comment    - The new comment.
 *  commentlen - Length of the comment.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  May 17, 2012
 * SOURCE
*/
int_f
h5oset_comment_c (hid_t_f *object_id, _fcd comment, size_t_f *commentlen)
/******/
{
  char *c_comment = NULL;   /* Buffer to hold C string */
  int_f ret_value = 0;     /* Return value */

  /*
   * Convert FORTRAN string to C string
   */
  if((c_comment = HD5f2cstring(comment, (size_t)*commentlen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Oset_comment function.
   */
  if((hid_t_f)H5Oset_comment((hid_t)*object_id, c_comment) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_comment)
    HDfree(c_comment);
  return ret_value;
}

/****if* H5Of/h5oset_comment_by_name_c
 * NAME
 *  h5oset_comment_by_name_c
 * PURPOSE
 *  Calls H5Oset_comment_by_name
 * INPUTS
 *  object_id  - Identifier of the target object.
 *  name       - Name of the object whose comment is to be set or reset, 
 *  specified as a path relative to loc_id. 
 *  namelen    - Length of the name.
 *  comment    - The new comment.
 *  commentlen - Length of the comment.
 *  lapl_id    - Link access property list identifier.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  May 17, 2012
 * SOURCE
*/
int_f
h5oset_comment_by_name_c (hid_t_f *object_id, _fcd name, size_t_f *namelen,  _fcd comment, size_t_f *commentlen, hid_t_f *lapl_id)
/******/
{
  char *c_comment = NULL;   /* Buffer to hold C string */
  char *c_name = NULL;   /* Buffer to hold C string */
  int_f ret_value = 0;     /* Return value */

  /*
   * Convert FORTRAN string to C string
   */
  if((c_comment = HD5f2cstring(comment, (size_t)*commentlen)) == NULL)
    HGOTO_DONE(FAIL);
  /*
   * Convert FORTRAN string to C string
   */
  if((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * Call H5Oset_comment_by_name function.
   */
  if((hid_t_f)H5Oset_comment_by_name((hid_t)*object_id, c_name, c_comment, (hid_t)*lapl_id) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_name)
    HDfree(c_name);
  if(c_comment)
    HDfree(c_comment);
  return ret_value;
}
/****if* H5Of/h5oopen_by_idx_c
 * NAME
 *  h5oopen_by_idx_c
 * PURPOSE
 *  Calls H5Oopen_by_idx_c
 * INPUTS
 *  loc_id        - A file or group identifier.
 *  group_name    - Name of group, relative to loc_id, in which object is located.
 *  group_namelen - Length of group_name
 *  index_type    - Type of index by which objects are ordered.
 *  order         - Order of iteration within index.
 *  n             - Object to open.
 *  lapl_id       - Link access property list.
 * OUTPUTS
 *  obj_id       - An object identifier for the opened object.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  May 17, 2012
 * SOURCE
*/
int_f
h5oopen_by_idx_c (hid_t_f *loc_id, _fcd  group_name, size_t_f *group_namelen, 
                  int_f *index_type, int_f *order, hsize_t_f *n, hid_t_f *obj_id, hid_t_f *lapl_id)
/******/
{
  char *c_group_name = NULL;     /* Buffer to hold C string */
  int_f ret_value = 0;
  H5_index_t c_index_type;
  H5_iter_order_t c_order;
  
  /*
   * Convert FORTRAN string to C string
   */
  if((c_group_name = HD5f2cstring( group_name, (size_t)*group_namelen)) == NULL)
    HGOTO_DONE(FAIL);

  c_index_type = (H5_index_t)*index_type;
  c_order = (H5_iter_order_t)*order;

  /*
   * Call H5Oopen_by_idx function.
   */
  if((*obj_id =(hid_t_f)H5Oopen_by_idx((hid_t)*loc_id, c_group_name, c_index_type, c_order, (hsize_t)*n, (hid_t)*lapl_id)) < 0)
    HGOTO_DONE(FAIL);

 done:
  if(c_group_name)
    HDfree(c_group_name);
  return ret_value;
}

/****if* H5Of/h5oget_comment_c
 * NAME
 *  h5oget_comment_c
 * PURPOSE
 *  Calls  H5Oget_comment
 * INPUTS
 *  object_id  - Identifier for the target object.
 *  bufsize    - Anticipated required size of the comment buffer.
 * OUTPUTS
 *  comment    - The comment.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  June 24, 2012
 * SOURCE
*/
int_f
h5oget_comment_c (hid_t_f *object_id, _fcd comment, size_t_f *commentsize,  hssize_t_f *bufsize)
/******/
{
  char *c_comment = NULL;  /* Buffer to hold C string */
  int_f ret_value = 0;     /* Return value */
  size_t c_commentsize;

  c_commentsize = (size_t)*commentsize + 1;

  /*
   * Allocate buffer to hold comment name
   */
  
  if(NULL == (c_comment = (char *)HDmalloc(c_commentsize)))
    HGOTO_DONE(FAIL);

  /*
   * Call H5Oget_comment function.
   */ 

  if((*bufsize = (hssize_t_f)H5Oget_comment((hid_t)*object_id, c_comment, (size_t)*commentsize)) < 0)
    HGOTO_DONE(FAIL);

  /*
   * Convert C name to FORTRAN and place it in the given buffer
   */
  if(c_comment)
    HD5packFstring(c_comment, _fcdtocp(comment), c_commentsize - 1);
  return ret_value;

 done:
  if(c_comment)
    HDfree(c_comment);

  return ret_value;
}

/****if* H5Of/h5oget_comment_by_name_c
 * NAME
 *  h5oget_comment_by_name_c
 * PURPOSE
 *  Calls H5Oget_comment_by_name
 * INPUTS
 *  object_id  - Identifier for the target object.
 *  bufsize    - Anticipated required size of the comment buffer.
 * OUTPUTS
 *  comment    - The comment.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  July 6, 2012
 * SOURCE
*/
int_f
h5oget_comment_by_name_c (hid_t_f *loc_id, _fcd name, size_t_f *name_size, 
                          _fcd comment, size_t_f *commentsize, size_t_f *bufsize, hid_t_f *lapl_id)
/******/
{
  char *c_comment = NULL;  /* Buffer to hold C string */
  char *c_name = NULL;     /* Buffer to hold C string */
  int_f ret_value = 0;     /* Return value */
  ssize_t c_bufsize;
  size_t c_commentsize;

  /*
   * Convert FORTRAN string to C string
   */
  if((c_name = HD5f2cstring(name, (size_t)*name_size)) == NULL)
    HGOTO_DONE(FAIL);

  c_commentsize = (size_t)*commentsize + 1;

  /*
   * Allocate buffer to hold comment name
   */
  
  if(NULL == (c_comment = (char *)HDmalloc(c_commentsize)))
    HGOTO_DONE(FAIL);

  /*
   * Call H5Oget_comment_by_name function.
   */ 

  if((c_bufsize = H5Oget_comment_by_name((hid_t)*loc_id, c_name, c_comment, (size_t)*commentsize,(hid_t)*lapl_id )) < 0)
    HGOTO_DONE(FAIL);

  if(c_name)
    HDfree(c_name);

  *bufsize = (size_t_f)c_bufsize;

  /*
   * Convert C name to FORTRAN and place it in the given buffer
   */
  if(c_comment) {
    HD5packFstring(c_comment, _fcdtocp(comment), c_commentsize - 1);
    HDfree(c_comment);
  }

  return ret_value;

 done:
  if(c_comment)
    HDfree(c_comment);
  if(c_name)
    HDfree(c_name);

  return ret_value;
}
