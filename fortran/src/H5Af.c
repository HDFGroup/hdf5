/****h* H5Af/H5Af
 * PURPOSE
 *  This file contains C stubs for H5A Fortran APIs
 *
 * COPYRIGHT
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 ******
 */

#include "H5f90.h"
#include "H5Eprivate.h"

/****if* H5Af/h5adelete_c
 * NAME
 *  h5adelete_c
 * PURPOSE
 *  Call H5Adelete to delete an attribute
 * INPUTS
 *  obj_id - object identifier
 *  name - name of the attribute
 *  namelen - name length
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5adelete_c(hid_t_f *obj_id, _fcd name, size_t_f *namelen)
/******/
{
    char *c_name    = NULL; /* Buffer to hold C string */
    int_f ret_value = 0;    /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if ((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
        HGOTO_DONE(FAIL);

    /*
     * Call H5Adelete function.
     */
    if (H5Adelete((hid_t)*obj_id, c_name) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_name)
        free(c_name);

    return ret_value;
}

/****if* H5Af/h5aget_num_attrs_c
 * NAME
 *  h5aget_num_attrs_c
 * PURPOSE
 *  Call H5Oget_info to determine number of
 *  attributes of an object
 * INPUTS
 *  obj_id - object identifier
 *  attr_num - number of attributes
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5aget_num_attrs_c(hid_t_f *obj_id, int_f *attr_num)
/******/
{
    H5O_info2_t oinfo;         /* Object info */
    int_f       ret_value = 0; /* Return value */

    /*
     * Call H5Oget_info function.
     */
    if (H5Oget_info3((hid_t)*obj_id, &oinfo, H5O_INFO_NUM_ATTRS) < 0)
        HGOTO_DONE(FAIL);

    /* Set number of attributes */
    *attr_num = (int_f)oinfo.num_attrs;

done:
    return ret_value;
}

/****if* H5Af/h5aget_name_c
 * NAME
 *  h5aget_name_c
 * PURPOSE
 *  Call H5Aget_name to get attribute's name
 * INPUTS
 *  attr_id - attribute identifier
 *  bufsize - size of the buffer
 * OUTPUTS
 *  buf - buffer to hold the name
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5aget_name_c(hid_t_f *attr_id, size_t_f *bufsize, _fcd buf)
/******/
{
    size_t c_bufsize;
    char  *c_buf     = NULL; /* Buffer to hold C string */
    int_f  ret_value = 0;    /* Return value */

    c_bufsize = (size_t)*bufsize + 1;

    /*
     * Allocate buffer to hold name of an attribute
     */
    if (NULL == (c_buf = (char *)malloc(c_bufsize)))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Aget_name function
     */
    if ((ret_value = (int_f)H5Aget_name((hid_t)*attr_id, c_bufsize, c_buf)) < 0)
        HGOTO_DONE(FAIL);

    /*
     * Convert C name to FORTRAN and place it in the given buffer
     */
    HD5packFstring(c_buf, _fcdtocp(buf), c_bufsize - 1);

done:
    if (c_buf)
        free(c_buf);
    return ret_value;
}

/****if* H5Af/h5adelete_by_name_c
 * NAME
 *  h5adelete_by_name_c
 * PURPOSE
 *  Call h5adelete_by_name to remove an attribute from a specified location
 * INPUTS
 *  loc_id - identifier for object to which attribute is attached
 *  obj_name - object identifier
 *  obj_namelen - name length
 *  attr_name - name of the attribute
 *  attr_namelen - name length
 *  lapl_id - link access property list
 *
 * OUTPUTS
 *     N/A
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5adelete_by_name_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, _fcd attr_name,
                    size_t_f *attr_namelen, hid_t_f *lapl_id)
/******/
{
    char *c_obj_name  = NULL; /* Buffer to hold C string */
    char *c_attr_name = NULL; /* Buffer to hold C string */
    int_f ret_value   = 0;    /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if ((c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)) == NULL)
        HGOTO_DONE(FAIL);
    if ((c_attr_name = HD5f2cstring(attr_name, (size_t)*attr_namelen)) == NULL)
        HGOTO_DONE(FAIL);

    /*
     * Call H5Adelete_by_name function.
     */
    if (H5Adelete_by_name((hid_t)*loc_id, c_obj_name, c_attr_name, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_attr_name)
        free(c_attr_name);
    if (c_obj_name)
        free(c_obj_name);
    return ret_value;
}
/****if* H5Af/h5adelete_by_idx_c
 * NAME
 *  h5adelete_by_idx_c
 * PURPOSE
 *  Call h5adelete_by_idx
 * INPUTS
 *  loc_id - Location or object identifier; may be dataset or group
 *  obj_name - object identifier
 *  obj_namelen - name length
 *  attr_name - name of the attribute
 *  attr_namelen - name length
 *  lapl_id - link access property list
 *
 * OUTPUTS
 *     N/A
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5adelete_by_idx_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, int_f *idx_type, int_f *order,
                   hsize_t_f *n, hid_t_f *lapl_id)
/******/
{
    char *c_obj_name = NULL; /* Buffer to hold C string */
    int_f ret_value  = 0;    /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if (NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Adelete_by_name function.
     */
    if (H5Adelete_by_idx((hid_t)*loc_id, c_obj_name, (H5_index_t)*idx_type, (H5_iter_order_t)*order,
                         (hsize_t)*n, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_obj_name)
        free(c_obj_name);

    return ret_value;
}
/****if* H5Af/h5aget_name_by_idx_c
 * NAME
 *  h5aget_name_by_idx_c
 * PURPOSE
 *  Call h5aget_name_by_idx
 * INPUTS
 *
 *
 *  loc_id - Identifier for object to which attribute is attached
 *  obj_name - Name of object, relative to location,
 *  from which attribute is to be removed *TEST* check NULL
 *  idx_type - Type of index; Possible values are:
 *                         H5_INDEX_UNKNOWN   - Unknown index type
 *                         H5_INDEX_NAME      - Index on names
 *                         H5_INDEX_CRT_ORDER - Index on creation order
 *                         H5_INDEX_N	      - Number of indices defined
 *
 *  order    - Order in which to iterate over index; Possible values are:
 *                          H5_ITER_UNKNOWN  - Unknown order
 *                          H5_ITER_INC      - Increasing order
 *                          H5_ITER_DEC      - Decreasing order
 *                          H5_ITER_NATIVE   - No particular order, whatever is fastest
 *                          H5_ITER_N	     - Number of iteration orders
 *
 *  n  - Attribute's position in index
 *  attr_id  - Attribute identifier
 *  size  - Buffer size ! *TEST* check for 0 value *CHECK* should this return the correct value
 *
 *  lapl_id   - Link access property list
 *  hdferr   - Error code:
 *  Returns attribute name size, -1 if fail
 *
 * OUTPUTS
 *  name - Attribute name
 *
 * RETURNS
 *  Size of buffer on success, -1 on failure
 * SOURCE
 */
int_f
h5aget_name_by_idx_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, int_f *idx_type, int_f *order,
                     hsize_t_f *n, _fcd name, size_t_f *size, hid_t_f *lapl_id)
/******/
{
    char   *c_obj_name = NULL; /* Buffer to hold C string */
    ssize_t c_size;
    size_t  c_buf_size;
    char   *c_buf     = NULL;
    int_f   ret_value = 0; /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if (NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL);

    /*
     * Allocate buffer to hold name of an attribute
     */
    c_buf_size = (size_t)*size + 1;
    if (NULL == (c_buf = (char *)malloc(c_buf_size)))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Aget_name_by_idx function.
     */
    c_size = H5Aget_name_by_idx((hid_t)*loc_id, c_obj_name, (H5_index_t)*idx_type, (H5_iter_order_t)*order,
                                (hsize_t)*n, c_buf, c_buf_size, (hid_t)*lapl_id);
    if (c_size < 0)
        HGOTO_DONE(FAIL);

    /*
     * Convert C name to FORTRAN and place it in the given buffer
     */
    HD5packFstring(c_buf, _fcdtocp(name), c_buf_size - 1);
    *size = (size_t_f)c_size;

done:
    if (c_obj_name)
        free(c_obj_name);
    if (c_buf)
        free(c_buf);
    return ret_value;
}

/****if* H5Af/h5aget_info_c
 * NAME
 *  h5aget_info_c
 * PURPOSE
 *  Call H5Aget_info
 * INPUTS
 *  loc_id  - Object identifier
 * OUTPUTS
 *
 *  corder_valid - Indicates whether the creation order data is valid for this attribute
 *  corder - Is a positive integer containing the creation order of the attribute
 *  cset - Indicates the character set used for the attribute's name
 *  data_size - indicates the size, in the number of characters, of the attribute
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5aget_info_c(hid_t_f *loc_id, int_f *corder_valid, int_f *corder, int_f *cset, hsize_t_f *data_size)
/******/
{

    int_f      ret_value = 0; /* Return value */
    H5A_info_t ainfo;

    /*
     * Call H5Aget_info function.
     */
    if (H5Aget_info((hid_t)*loc_id, &ainfo) < 0)
        HGOTO_DONE(FAIL);

    /* Unpack the structure */

    *corder_valid = 0;
    if (ainfo.corder_valid > 0)
        *corder_valid = 1;

    *corder    = (int_f)ainfo.corder;
    *cset      = (int_f)ainfo.cset;
    *data_size = (hsize_t_f)ainfo.data_size;

done:
    return ret_value;
}

/****if* H5Af/h5aget_info_by_idx_c
 * NAME
 *  h5aget_info_by_idx_c
 * PURPOSE
 *  Call  H5Aget_info_by_idx
 * INPUTS
 *  loc_id  - Object identifier
 *  obj_name - Name of object to which attribute is attached
 *  obj_namelen - name length
 *  idx_type - Type of index; Possible values are:
 *                         H5_INDEX_UNKNOWN   - Unknown index type
 *                         H5_INDEX_NAME      - Index on names
 *                         H5_INDEX_CRT_ORDER - Index on creation order
 *                         H5_INDEX_N	      - Number of indices defined
 *
 *  order - Order in which to iterate over index; Possible values are:
 *                          H5_ITER_UNKNOWN  - Unknown order
 *                          H5_ITER_INC      - Increasing order
 *                          H5_ITER_DEC      - Decreasing order
 *                          H5_ITER_NATIVE   - No particular order, whatever is fastest
 *                          H5_ITER_N	     - Number of iteration orders
 *
 *  n - Attribute's position in index
 *  lapl_id - Link access property list
 * OUTPUTS
 *
 *  corder_valid - Indicates whether the creation order data is valid for this attribute
 *  corder - Is a positive integer containing the creation order of the attribute
 *  cset - Indicates the character set used for the attribute's name
 *  data_size - indicates the size, in the number of characters, of the attribute
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5aget_info_by_idx_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, int_f *idx_type, int_f *order,
                     hsize_t_f *n, hid_t_f *lapl_id, int_f *corder_valid, int_f *corder, int_f *cset,
                     hsize_t_f *data_size)
/******/
{
    char      *c_obj_name = NULL; /* Buffer to hold C string */
    H5A_info_t ainfo;
    int_f      ret_value = 0; /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if (NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Ainfo_by_idx function.
     */
    if (H5Aget_info_by_idx((hid_t)*loc_id, c_obj_name, (H5_index_t)*idx_type, (H5_iter_order_t)*order,
                           (hsize_t)*n, &ainfo, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

    /* Unpack the structure */
    *corder_valid = 0;
    if (ainfo.corder_valid > 0)
        *corder_valid = 1;
    *corder    = (int_f)ainfo.corder;
    *cset      = (int_f)ainfo.cset;
    *data_size = (hsize_t_f)ainfo.data_size;

done:
    if (c_obj_name)
        free(c_obj_name);

    return ret_value;
}

/****if* H5Af/h5aget_info_by_name_c
 * NAME
 *  h5aget_info_by_name_c
 * PURPOSE
 *  Call  H5Aget_info_by_name
 * INPUTS
 *  loc_id - Object identifier
 *  obj_name - Name of object to which attribute is attached
 *  obj_namelen - name length
 *  attr_name - Attribute name
 *  attr_namelen - attribute name length
 *  lapl_id - Link access property list
 * OUTPUTS
 *
 *  corder_valid - Indicates whether the creation order data is valid for this attribute
 *  corder - Is a positive integer containing the creation order of the attribute
 *  cset - Indicates the character set used for the attribute's name
 *  data_size - indicates the size, in the number of characters, of the attribute
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5aget_info_by_name_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, _fcd attr_name,
                      size_t_f *attr_namelen, hid_t_f *lapl_id, int_f *corder_valid, int_f *corder,
                      int_f *cset, hsize_t_f *data_size)
/******/
{
    char      *c_obj_name  = NULL; /* Buffer to hold C string */
    char      *c_attr_name = NULL; /* Buffer to hold C string */
    H5A_info_t ainfo;
    int_f      ret_value = 0; /* Return value */

    /*
     * Convert FORTRAN name to C name
     */
    if (NULL == (c_obj_name = HD5f2cstring(obj_name, (size_t)*obj_namelen)))
        HGOTO_DONE(FAIL);
    if (NULL == (c_attr_name = HD5f2cstring(attr_name, (size_t)*attr_namelen)))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Ainfo_by_name function.
     */
    if (H5Aget_info_by_name((hid_t)*loc_id, c_obj_name, c_attr_name, &ainfo, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

    /* Unpack the structure */
    *corder_valid = 0;
    if (ainfo.corder_valid > 0)
        *corder_valid = 1;
    *corder    = (int_f)ainfo.corder;
    *cset      = (int_f)ainfo.cset;
    *data_size = (hsize_t_f)ainfo.data_size;

done:
    if (c_obj_name)
        free(c_obj_name);
    if (c_attr_name)
        free(c_attr_name);

    return ret_value;
}

/****if* H5Af/h5awrite_f_c
 * NAME
 *  h5awrite_f_c
 * PURPOSE
 *  Call H5Awrite to write a dataset
 * INPUTS
 *  attr_id     - Identifier of an attribute to write.
 *  mem_type_id - Identifier of the attribute datatype (in memory).
 *  buf         - data pointer buffer
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5awrite_f_c(hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf)
/******/
{
    int    ret_value = -1;
    herr_t ret;

    ret = H5Awrite((hid_t)*attr_id, (hid_t)*mem_type_id, buf);

    if (ret < 0)
        return ret_value;
    ret_value = 0;
    return ret_value;
}

/****if* H5Af/h5aread_f_c
 * NAME
 *  h5aread_f_c
 * PURPOSE
 *  Call H5Awrite to write a dataset
 * INPUTS
 *  attr_id     - Identifier of an attribute to write.
 *  mem_type_id - Identifier of the attribute datatype (in memory).
 *  buf         - data pointer buffer
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5aread_f_c(hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf)
/******/
{
    int    ret_value = -1;
    herr_t ret;

    ret = H5Aread((hid_t)*attr_id, (hid_t)*mem_type_id, buf);

    if (ret < 0)
        return ret_value;
    ret_value = 0;
    return ret_value;
}
