/****h* H5Ff/H5Ff
 * PURPOSE
 *   This file contains C stubs for H5F Fortran APIs
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

/****if* H5Ff/h5fcreate_c
 * NAME
 *        h5fcreate_c
 * PURPOSE
 *     Call H5Fcreate to create the file
 * INPUTS
 *      name - name of the file
 *              namelen - name length
 *              access_flags - file access  flags
 *              crt_pr  - identifier of creation property list
 *              acc_prp - identifier of access property list
 * OUTPUTS
 *     file_id - file identifier
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Monday, July 26, 1999
 * SOURCE
*/
int_f
nh5fcreate_c(_fcd name, int_f *namelen, int_f *access_flags, hid_t_f* crt_prp, hid_t_f *acc_prp, hid_t_f *file_id)
/******/
{
     int ret_value = -1;
     char *c_name;
     int_f c_namelen;
     hid_t c_file_id;
     unsigned c_access_flags;
     hid_t c_crt_prp;
     hid_t c_acc_prp;

     /*
      * Define access flags
      */
     c_access_flags = (unsigned) *access_flags;

     /*
      * Define creation property
      */
     c_crt_prp = *crt_prp;

     /*
      * Define access property
      */
     c_acc_prp = *acc_prp;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, (size_t)c_namelen);
     if(c_name == NULL)
         return ret_value;

     /*
      * Call H5Fcreate function.
      */
     c_file_id = H5Fcreate(c_name, c_access_flags, c_crt_prp, c_acc_prp);

     if (c_file_id >= 0) {
         ret_value = 0;
         *file_id = c_file_id;
     }

     HDfree(c_name);
     return ret_value;
}

/****if* H5Ff/h5fflush_c
 * NAME
 *        h5fflush_c
 * PURPOSE
 *     Call H5Fflush to flush the object
 * INPUTS
 *      object_id - identifier of either a file, a dataset,
 *                          a group, an attribute or a named data type
 *              scope - integer to specify the flushing action, either
 *                      H5F_SCOPE_GLOBAL or H5F_SCOPE_LOCAL
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Friday, November 5, 1999
 * SOURCE
*/
int_f
nh5fflush_c (hid_t_f *object_id, int_f *scope)
/******/
{
     int ret_value = -1;
     hid_t c_file_id;
     H5F_scope_t  c_scope;
     htri_t status;
     c_scope = (H5F_scope_t)*scope;

     /*
      * Call H5Fflush function.
      */

     c_file_id = *object_id;

     status = H5Fflush(c_file_id, c_scope);

     if (status >= 0)  ret_value = 0;

     return ret_value;
}


/****if* H5Ff/h5fmount_c
 * NAME
 *        h5fmount_c
 * PURPOSE
 *     Call H5Fmount to mount the file
 * INPUTS
 *      loc_id - Identifier for file or group
 *              dsetname - name of dataset
 *              namelen - dsetname length
 *              file_id - file identifier for the file to be mounted
 *              acc_prp - identifier of access property list
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Monday, October 25, 1999
 * HISTORY
*/
int_f
nh5fmount_c (hid_t_f *loc_id, _fcd dsetname, int_f *namelen, hid_t_f *file_id, hid_t_f *acc_prp)
/******/
{
     int ret_value = -1;
     char *c_name;
     int_f c_namelen;
     hid_t c_loc_id;
     hid_t c_file_id;
     hid_t c_acc_prp;
     htri_t status;

     /*
      * Define access property
      */
     c_acc_prp = *acc_prp;
/*
     if ( H5P_DEFAULT_F == c_acc_prp ) c_acc_prp = H5P_DEFAULT;
*/

     c_loc_id = *loc_id;
     c_file_id = *file_id;
     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(dsetname, (size_t)c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Fmount function.
      */
     status = H5Fmount(c_loc_id, c_name, c_file_id, c_acc_prp);

     if (status >= 0)  ret_value = 0;

     HDfree(c_name);
     return ret_value;
}

/****if* H5Ff/h5funmount_c
 * NAME
 *        h5funmount_c
 * PURPOSE
 *     Call H5Funmount to unmount the file
 * INPUTS
 *      loc_id - Identifier for file or group
 *              dsetname - name of dataset
 *              namelen - dsetname length
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Monday, October 25, 1999
 * SOURCE
*/
int_f
nh5funmount_c (hid_t_f *loc_id, _fcd dsetname, int_f *namelen)
/******/
{
     int ret_value = -1;
     char *c_name;
     int_f c_namelen;
     hid_t c_loc_id;
     htri_t status;

     c_loc_id = *loc_id;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(dsetname, (size_t)c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Fmount function.
      */
     status = H5Funmount(c_loc_id, c_name);

     if (status >= 0)  ret_value = 0;

     HDfree(c_name);
     return ret_value;
}



/****if* H5Ff/h5fopen_c
 * NAME
 *        h5fopen_c
 * PURPOSE
 *     Call H5Fopen to open the file
 * INPUTS
 *      name - name of the file
 *              namelen - name length
 *              access_flags - file access  flags
 *              acc_prp - identifier of access property list
 * OUTPUTS
 *     file_id - file identifier
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Tuesday, August 3, 1999
 * SOURCE
*/
int_f
nh5fopen_c (_fcd name, int_f *namelen, int_f *access_flags, hid_t_f *acc_prp, hid_t_f *file_id)
/******/
{
     int ret_value = -1;
     char *c_name;
     int_f c_namelen;
     hid_t c_file_id;
     unsigned c_access_flags;
     hid_t c_acc_prp;
     c_acc_prp = (hid_t)*acc_prp;

     /*
      * Define access flags
      */
     c_access_flags = (unsigned) *access_flags;

     /*
      * Define access property
      */
     c_acc_prp = *acc_prp;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, (size_t)c_namelen);
     if(c_name == NULL)
         return ret_value;

     /*
      * Call H5Fopen function.
      */
     c_file_id = H5Fopen(c_name, c_access_flags, c_acc_prp);

     if(c_file_id >= 0) {
         ret_value = 0;
         *file_id = (hid_t_f)c_file_id;
     } /* end if */

     HDfree(c_name);
     return ret_value;
}

/****if* H5Ff/h5freopen_c
 * NAME
 *        h5freopen_c
 * PURPOSE
 *     Call H5Freopen to open the file
 * INPUTS
 *      file_id1 - file identifier
 * OUTPUTS
 *     file_id2 - file identifier
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *              Wednesday, November 3, 1999
 * SOURCE
*/
int_f
nh5freopen_c (hid_t_f *file_id1, hid_t_f *file_id2)
/******/
{
     int ret_value = -1;
     hid_t c_file_id1, c_file_id2;

     c_file_id1 = *file_id1;
     c_file_id2 = H5Freopen(c_file_id1);

     if (c_file_id2 < 0) return ret_value;
     *file_id2 = (hid_t_f)c_file_id2;

     ret_value = 0;
     return ret_value;
}

/****if* H5Ff/h5fget_create_plist_c
 * NAME
 *        h5fget_create_plist_c
 * PURPOSE
 *     Call H5Fget_create_plist to get the file creation property list
 * INPUTS
 *      file_id - file identifier
 * OUTPUTS
 *     prop_id - creation property list identifier
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal, Xiangyang Su
 *              Wednesday, November 3, 1999
 * SOURCE
*/
int_f
nh5fget_create_plist_c (hid_t_f *file_id, hid_t_f *prop_id)
/******/
{
     int ret_value = -1;
     hid_t c_file_id, c_prop_id;

     c_file_id = (hid_t)*file_id;
     c_prop_id = H5Fget_create_plist(c_file_id);

     if (c_prop_id < 0) return ret_value;
     *prop_id = (hid_t_f)c_prop_id;

     ret_value = 0;
     return ret_value;
}

/****if* H5Ff/h5fget_access_plist_c
 * NAME
 *        h5fget_access_plist_c
 * PURPOSE
 *     Call H5Fget_access_plist to get the file access property list
 * INPUTS
 *      file_id - file identifier
 * OUTPUTS
 *     access_id - access property list identifier
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Monday, September 30, 2002
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5fget_access_plist_c (hid_t_f *file_id, hid_t_f *access_id)
/******/
{
     int ret_value = -1;
     hid_t c_file_id, c_access_id;

     c_file_id = (hid_t)*file_id;
     c_access_id = H5Fget_access_plist(c_file_id);

     if (c_access_id < 0) return ret_value;
     *access_id = (hid_t_f)c_access_id;

     ret_value = 0;
     return ret_value;
}

/****if* H5Ff/h5fis_hdf5_c
 * NAME
 *        h5fis_hdf5_c
 * PURPOSE
 *     Call H5Fis_hdf5 to determone if the file is an HDF5 file
 * INPUTS
 *      name - name of the file
 *              namelen - name length
 * OUTPUTS
 *     flag - 0 if file is not HDF5 file , positive if a file
 *                     is an HDF5 file, and negative on failure.
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Tuesday, August 3, 1999
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5fis_hdf5_c (_fcd name, int_f *namelen, int_f *flag)
/******/
{
     int ret_value = -1;
     char *c_name;
     int_f c_namelen;
     htri_t status;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, (size_t)c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Fopen function.
      */
     status = H5Fis_hdf5(c_name);
     *flag = (int_f)status;
     if (status >= 0) ret_value = 0;

     HDfree(c_name);
     return ret_value;
}
/****if* H5Ff/h5fclose_c
 * NAME
 *        h5fclose_c
 * PURPOSE
 *     Call H5Fclose to close the file
 * INPUTS
 *      file_id - identifier of the file to be closed
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Monday, July 26, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
nh5fclose_c ( hid_t_f *file_id )
/******/
{
  int ret_value = 0;
  hid_t c_file_id;

  c_file_id = (hid_t)*file_id;
  if ( H5Fclose(c_file_id) < 0  ) ret_value = -1;
  return ret_value;
}
/****if* H5Ff/h5fget_obj_count_c
 * NAME
 *        h5fget_obj_count_c
 * PURPOSE
 *     Call H5Fget_obj_count to get number of open objects within a file
 * INPUTS
 *      file_id - identifier of the file to be closed
 *              obj_type - type of the object
 * RETURNS
 *     obj_count - number of objects
 *              0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Monday, September 30, 2002
 * HISTORY
 *
 *              Changed type of obj_count to size_t_f
 *              Thursday, September 25, 2008
 * SOURCE
*/

int_f
nh5fget_obj_count_c ( hid_t_f *file_id , int_f *obj_type, size_t_f * obj_count)
/******/
{
  int ret_value = 0;
  hid_t c_file_id;
  unsigned c_obj_type;
  ssize_t c_obj_count;


  c_file_id = (hid_t)*file_id;
  c_obj_type = (unsigned) *obj_type;
  if ( (c_obj_count=H5Fget_obj_count(c_file_id, c_obj_type)) < 0  ) ret_value = -1;
  *obj_count = (size_t_f)c_obj_count;
  return ret_value;
}
/****if* H5Ff/h5fget_obj_ids_c
 * NAME
 *  h5fget_obj_ids_c
 * PURPOSE
 *  Call H5Fget_obj_count to get number of open objects within a file
 * INPUTS
 *  file_id  - identifier of the file to be closed
 *  obj_type - type of the object
 * RETURNS
 *  obj_ids  - iarray of open objects identifiers
 *              0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Monday, September 30, 2002
 * HISTORY
 *
 *  Changed type of max_obj to size_t_f; added parameter for the
 *  number of open objects
 *  Thursday, September 25, 2008 EIP
 *	
 * SOURCE
*/
int_f
nh5fget_obj_ids_c ( hid_t_f *file_id , int_f *obj_type, size_t_f *max_objs, 
    hid_t_f *obj_ids, size_t_f *num_objs)
/******/
{
    int ret_value = 0;
    hid_t c_file_id;
    unsigned c_obj_type;
    size_t u;
    size_t c_max_objs;
    ssize_t c_num_objs;
    hid_t *c_obj_ids;

    c_file_id = (hid_t)*file_id;
    c_obj_type = (unsigned) *obj_type;
    c_max_objs = (size_t)*max_objs;
    c_obj_ids = (hid_t *)HDmalloc(sizeof(hid_t)*c_max_objs);

    c_num_objs = H5Fget_obj_ids(c_file_id, c_obj_type, c_max_objs, c_obj_ids);
    if(c_num_objs < 0)
        ret_value = -1;
    for(u = 0; u < c_max_objs; u++)
        obj_ids[u] = (hid_t_f)c_obj_ids[u];

    HDfree(c_obj_ids);
    *num_objs = (size_t_f)c_num_objs;

    return ret_value;
}

/****if* H5Ff/h5fget_freespace_c
 * NAME
 *        h5fget_freespace_c
 * PURPOSE
 *     Call H5Fget_freespace to get amount of free space within a file
 * INPUTS
 *      file_id - identifier of the file to query
 * RETURNS
 *     free_space  - amount of free space in file
 *              0 on success, -1 on failure
 * AUTHOR
 *  Quincey Koziol
 *              Tuesday, October 7, 2003
 * SOURCE
*/

int_f
nh5fget_freespace_c ( hid_t_f *file_id , hssize_t_f *free_space)
/******/
{
  int ret_value = 0;
  hid_t c_file_id;
  hssize_t c_free_space;

  c_file_id = (hid_t)*file_id;
  if ( (c_free_space=H5Fget_freespace(c_file_id)) < 0  ) ret_value = -1;
  *free_space=(hssize_t_f)c_free_space;
  return ret_value;
}

/****if* H5Ff/h5fget_name_c
 * NAME
 *        h5fget_name_c
 * PURPOSE
 *     Call H5Fget_name to get file's name
 * INPUTS
 *      obj_id - object identifier
 *              buflen -size of the buffer
 * OUTPUTS
 *     buf - buffer to hold the name
 *              size - size of the file's name
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Tuesday, July 6, 2004
 * SOURCE
*/
int_f
nh5fget_name_c(hid_t_f *obj_id, size_t_f *size, _fcd buf, size_t_f *buflen)
/******/
{
    char *c_buf = NULL;           /* Buffer to hold C string */
    ssize_t size_c = -1;
    int_f ret_value = 0;          /* Return value */

     /*
      * Allocate buffer to hold name of an attribute
      */
     if(NULL == (c_buf = (char *)HDmalloc((size_t)*buflen + 1)))
         HGOTO_DONE(FAIL);

     /*
      * Call H5Fget_name function
      */
     if ((size_c = (size_t_f)H5Fget_name((hid_t)*obj_id, c_buf, (size_t)*buflen)) < 0)
         HGOTO_DONE(FAIL);

     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
      HD5packFstring(c_buf, _fcdtocp(buf), (size_t)*buflen);

done:
      *size = (size_t_f)size_c;
      if(c_buf) HDfree(c_buf);
      return ret_value;
}

/****if* H5Ff/h5fget_filesize_c
 * NAME
 *        h5fget_filesize_c
 * PURPOSE
 *     Call H5Fget_filesize to get file size
 * INPUTS
 *      file_id - file identifier
 * OUTPUTS
 *     size - size of the file
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Wednesday, July 7, 2004
 * SOURCE
*/
int_f
nh5fget_filesize_c(hid_t_f *file_id, hsize_t_f *size)
/******/
{
    hsize_t size_c;
    herr_t ret_value=0;          /* Return value */

     /*
      * Call H5Fget_filesize function
      */
     if ((ret_value = H5Fget_filesize((hid_t)*file_id, &size_c)) < 0)
         HGOTO_DONE(FAIL);
      *size = (hsize_t_f)size_c;

done:
      return ret_value;
}

/****if* H5Ff/h5fget_file_image_c
 * NAME
 *  h5fget_file_image_c
 * PURPOSE
 *  Calls h5fget_file_image
 * INPUTS
 *  file_id    - Target file identifier.
 *  buf_ptr    - Pointer to the buffer into which the image of the HDF5 file is to be copied.
 *  buf_len    - Size of the supplied buffer.
 * OUTPUTS
 *  buf_req    - The size in bytes of the buffer required to store the file image.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  November 26, 2012
 * SOURCE
*/
int_f
nh5fget_file_image_c(hid_t_f *file_id, void *buf_ptr, size_t_f *buf_len, size_t_f *buf_req)
/******/
{
    herr_t ret_value=0;          /* Return value */

    /*
     * Call h5fget_file_image function
     */
    if ((*buf_req = (size_t_f)H5Fget_file_image((hid_t)*file_id, buf_ptr, (size_t)*buf_len)) < 0)
         HGOTO_DONE(FAIL);

done:
      return ret_value;
}
