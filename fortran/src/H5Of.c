/****h* H5Of/H5Of
 * PURPOSE
 *   This file contains C stubs for H5O Fortran APIs
 *
 * COPYRIGHT
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 ******
*/

#include "H5f90.h"
#include "H5Eprivate.h"

/****if* H5Of/h5olink_c
 * NAME
 *  h5olink_c
 * PURPOSE
 *  Calls H5Olink
 * INPUTS
 *      object_id        - Object to be linked.
 *      new_loc_id       - File or group identifier specifying location at which object is to be linked.
 *      name             - Name of link to be created, relative to new_loc_id.
 *      namelen          - Length of buffer for link to be created.
 *      lcpl_id          - Link creation property list identifier.
 *      lapl_id          - Link access property list identifier.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  April 21, 2008
 * SOURCE
*/
int_f
nh5olink_c (hid_t_f *object_id, hid_t_f *new_loc_id, _fcd name, size_t_f *namelen,
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
nh5oopen_c (hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id, hid_t_f *obj_id)
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
 *   h5oclose_c
 * PURPOSE
 *   Call H5Oclose
 * INPUTS
 *   object_id   - Object identifier  
 * RETURNS
 *   0 on success, -1 on failure
 * AUTHOR
 *   M. Scot Breitenfeld
 *   December 17, 2008
 * SOURCE
*/
int_f
nh5oclose_c ( hid_t_f *object_id )
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
 *    object_id - Identifier specifying subject group
 *   index_type - Type of index which determines the order
 *        order - Order within index
 *          idx - Iteration position at which to start
 *           op - Callback function passing data regarding the link to the calling application
 *      op_data - User-defined pointer to data required by the application for its processing of the link
 *
 * OUTPUTS
 *          idx - Position at which an interrupted iteration may be restarted
 *
 * RETURNS
 *     >0 on success, 0< on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  November 19, 2008
 * SOURCE
*/
int_f
nh5ovisit_c(hid_t_f *group_id, int_f *index_type, int_f *order, H5O_iterate_t op, void *op_data )
/******/
{
  int_f ret_value = -1;       /* Return value */
  herr_t func_ret_value; /* H5Linterate return value */

  /*
   * Call H5Ovisit
   */
  func_ret_value = H5Ovisit( (hid_t)*group_id, (H5_index_t)*index_type, (H5_iter_order_t)*order, op, op_data);

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
 *    addr  - Object’s address in the file
 *
 * OUTPUTS
 *  obj_id  - Dataset identifier      
 *
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  September 14, 2009
 * SOURCE
*/
int_f
nh5oopen_by_addr_c (hid_t_f *loc_id, haddr_t_f *addr, hid_t_f *obj_id)
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

/* ***if* H5Of/H5Oget_info_by_name_c
 * NAME
 *  H5Oget_info_by_name_c
 * PURPOSE
 *  Calls H5Oget_info_by_name
 * INPUTS
 *  loc_id       - File or group identifier specifying location of group in which object is located.
 *  name         - Name of group, relative to loc_id.
 *  namelen      - Name length.
 *  lapl_id      - Link access property list.
 * OUTPUTS
 *  corder_valid - Indicates whether the the creation order data is valid for this attribute.
 *  corder       - Is a positive integer containing the creation order of the attribute.
 *  cset         - Indicates the character set used for the attribute’s name.
 *  data_size    - indicates the size, in the number of characters, of the attribute.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  December 1, 2008
 * SOURCE
*/
int_f
nh5oget_info_by_name_c (hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id,
			H5O_info_t *object_info)
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
  if(H5Oget_info_by_name((hid_t)*loc_id, c_name,
			 &Oinfo, (hid_t)*lapl_id) < 0)
    HGOTO_DONE(FAIL);

  object_info->fileno    = Oinfo.fileno;
  object_info->addr      = Oinfo.addr;
  object_info->type      = Oinfo.type;
  object_info->rc        = Oinfo.rc;
  object_info->atime     = Oinfo.atime;
  object_info->mtime     = Oinfo.mtime;
  object_info->ctime     = Oinfo.ctime;
  object_info->btime     = Oinfo.btime;
  object_info->num_attrs = Oinfo.num_attrs;


/*      printf("fileno %d %d\n",object_info->fileno, Oinfo.fileno); */
/*   printf("string %d %d\n",object_info.rc, Oinfo.rc); */
  
/*   printf("atime %lld %lld\n",(long long int)object_info.atime, (long long int)Oinfo.atime); */
/*   printf("atime %lld %lld\n",(long long int)object_info.mtime, (long long int)Oinfo.mtime); */
/*   printf("atime %lld %lld\n",(long long int)object_info.ctime, (long long int)Oinfo.ctime); */
/*   printf("atime %lld %lld\n",(long long int)object_info.btime, (long long int)Oinfo.btime); */
/*   printf("string %f %f\n",object_info.addr, Oinfo.addr); */
/*   printf("num_attrs %d %d\n",object_info.num_attrs, Oinfo.num_attrs); */
/*   printf("num_attrs %f %f\n",object_info.hdr.version, Oinfo.hdr.version); */


 done:
  return ret_value;
}

