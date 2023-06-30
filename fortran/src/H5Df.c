/****h* H5Df/H5Df
 * PURPOSE
 *  This file contains C stubs for H5D Fortran APIs
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

/****if* H5Df/h5dwrite_ref_reg_c
 * NAME
 *  h5dwrite_ref_reg_c
 * PURPOSE
 *  Call H5Dwrite to write a dataset of dataset region references
 * INPUTS
 *  dset_id       - dataset identifier
 *  mem_type_id   - memory datatype identifier
 *  mem_space_id  - memory dataspace identifier
 *  file_space_id - memory dataspace identifier
 *  xfer_pr       - identifier of transfer property list
 *  buf           - data buffer with references to the objects.
 *  n             - number of references to be stored.
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5dwrite_ref_reg_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
                   hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims)
/******/
{
    int              ret_value = -1;
    herr_t           ret;
    hid_t            c_dset_id;
    hid_t            c_mem_type_id;
    hid_t            c_mem_space_id;
    hid_t            c_file_space_id;
    hid_t            c_xfer_prp;
    hdset_reg_ref_t *buf_c = NULL;
    unsigned int     i, n;

    n = (unsigned int)*dims;
    /*
     * Define transfer property
     */
    c_xfer_prp = (hid_t)*xfer_prp;

    /*
     * Allocate temporary buffer and copy references from Fortran.
     */
    buf_c = (hdset_reg_ref_t *)malloc(sizeof(hdset_reg_ref_t) * n);
    if (buf_c != NULL) {
        for (i = 0; i < n; i++) {
            memcpy(&buf_c[i], buf, H5R_DSET_REG_REF_BUF_SIZE);
            buf = buf + REF_REG_BUF_LEN_F;
        }
    }
    else
        return ret_value;

    /*
     * Call H5Dwrite function.
     */
    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    ret             = H5Dwrite(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf_c);
    free(buf_c);
    if (ret < 0)
        return ret_value;
    ret_value = 0;
    return ret_value;
}

/****if* H5Df/h5dread_ref_reg_c
 * NAME
 *  h5dread_ref_reg_c
 * PURPOSE
 *  Call H5Dread to read a dataset of dataset region references
 * INPUTS
 *  dset_id - dataset identifier
 *  mem_type_id - memory datatype identifier
 *  mem_space_id - memory dataspace identifier
 *  file_space_id - memory dataspace identifier
 *  xfer_pr  - identifier of transfer property list
 *  buf      - data buffer to store references to the objects.
 *  n - number of references to be stored.
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5dread_ref_reg_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
                  hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims)
/******/
{
    int              ret_value = -1;
    herr_t           ret       = -1;
    hid_t            c_dset_id;
    hid_t            c_mem_type_id;
    hid_t            c_mem_space_id;
    hid_t            c_file_space_id;
    hid_t            c_xfer_prp;
    hdset_reg_ref_t *buf_c = NULL;
    hsize_t          i, n;
    n = (hsize_t)*dims;
    /*
     * Define transfer property
     */
    c_xfer_prp = (hid_t)*xfer_prp;

    /*
     * Allocate temporary buffer.
     */
    buf_c = (hdset_reg_ref_t *)malloc(sizeof(hdset_reg_ref_t) * (size_t)n);
    if (buf_c != NULL) {
        /*
         * Call H5Dread function.
         */
        c_dset_id       = (hid_t)*dset_id;
        c_mem_type_id   = (hid_t)*mem_type_id;
        c_mem_space_id  = (hid_t)*mem_space_id;
        c_file_space_id = (hid_t)*file_space_id;
        ret = H5Dread(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf_c);
        if (ret >= 0) {
            for (i = 0; i < n; i++) {
                memcpy(buf, &buf_c[i], H5R_DSET_REG_REF_BUF_SIZE);
                buf = buf + REF_REG_BUF_LEN_F;
            }
        }
        if (buf_c != NULL)
            free(buf_c);
    }
    if (ret < 0)
        return ret_value;
    ret_value = 0;
    return ret_value;
}

/****if* H5Df/h5dget_type_c
 * NAME
 *  h5dget_type_c
 * PURPOSE
 *  Call H5Dget_type to obtain datatype of a dataset
 * INPUTS
 *  dset_id - identifier of the dataset
 * OUTPUTS
 *  type_id - identifier of the dataset's datatype
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dget_type_c(hid_t_f *dset_id, hid_t_f *type_id)
/******/
{
    int   ret_value = -1;
    hid_t c_dset_id;
    hid_t c_type_id;

    c_dset_id = (hid_t)*dset_id;
    c_type_id = H5Dget_type(c_dset_id);

    if (c_type_id < 0)
        return ret_value;

    *type_id  = (hid_t_f)c_type_id;
    ret_value = 0;
    return ret_value;
}

/****if* H5Df/h5dget_create_plist_c
 * NAME
 *  h5dget_create_plist_c
 * PURPOSE
 *  Call H5Dget_create_plist to obtain creation property list
 *  of a dataset
 * INPUTS
 *  dset_id - identifier of the dataset
 * OUTPUTS
 *  plist_id - identifier of he dataset creation property list
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dget_create_plist_c(hid_t_f *dset_id, hid_t_f *plist_id)
/******/
{
    int   ret_value = -1;
    hid_t c_dset_id;
    hid_t c_plist_id;

    c_dset_id  = (hid_t)*dset_id;
    c_plist_id = H5Dget_create_plist(c_dset_id);

    if (c_plist_id < 0)
        return ret_value;

    ret_value = 0;
    *plist_id = (hid_t_f)c_plist_id;
    return ret_value;
}

/****if* H5Df/h5dget_storage_size_c
 * NAME
 *  h5dget_storage_size_c
 * PURPOSE
 *  Call H5Dget_storage_size to return the amount of storage
 *  required for a dataset
 * INPUTS
 *  dset_id - identifier of the dataset
 * OUTPUTS
 *  size    - the amount of storage required for a dataset
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dget_storage_size_c(hid_t_f *dset_id, hsize_t_f *size)
/******/
{
    int     ret_value = -1;
    hsize_t c_size;
    hid_t   c_dset_id;

    c_dset_id = (hid_t)*dset_id;
    c_size    = H5Dget_storage_size(c_dset_id);
    if (c_size != 0) {
        ret_value = 0;
    }
    *size = (hsize_t_f)c_size;
    return ret_value;
}

/****if* H5Df/h5dvlen_get_max_len_c
 * NAME
 *  h5dvlen_get_max_len_c
 * PURPOSE
 *  Get the maximum size of the VL dataset element
 * INPUTS
 *  dset_id - identifier of the dataset
 *  type_id - datatype identifier
 *  space_id - dataspace identifier
 * OUTPUTS
 *  len      - maximum length of the VL dataset element
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dvlen_get_max_len_c(hid_t_f *dset_id, hid_t_f *type_id, hid_t_f *space_id, size_t_f *len)
/******/
{
    int      ret_value = -1;
    size_t   c_len;
    hid_t    c_dset_id;
    hid_t    c_type_id;
    hid_t    c_space_id;
    hvl_t   *c_buf;
    int      i;
    hssize_t num_elem;
    herr_t   status;

    c_dset_id  = (hid_t)*dset_id;
    c_type_id  = (hid_t)*type_id;
    c_space_id = (hid_t)*space_id;

    num_elem = H5Sget_select_npoints(c_space_id);
    if (num_elem < 0)
        return ret_value;

    c_buf = (hvl_t *)malloc(sizeof(hvl_t) * (size_t)num_elem);
    if (c_buf == NULL)
        return ret_value;
    status = H5Dread(c_dset_id, c_type_id, H5S_ALL, c_space_id, H5P_DEFAULT, c_buf);
    if (status < 0)
        goto DONE;

    c_len = 0;
    for (i = 0; i < num_elem; i++)
        c_len = H5_MAX(c_len, c_buf[i].len);
    *len = (size_t_f)c_len;
    H5Treclaim(c_type_id, c_space_id, H5P_DEFAULT, c_buf);
    ret_value = 0;

DONE:

    free(c_buf);
    return ret_value;
}
/****if* H5Df/h5dwrite_vl_integer_c
 * NAME
 *  h5dwrite_vl_integer_c
 * PURPOSE
 *  Write variable length dataset
 * INPUTS
 *  dset_id - identifier of the dataset
 *  mem_type_id - datatype identifier
 *  mem_space_id - dataspace identifier
 *  file_space_id - file dataspace identifier
 *  xfer          - file transfer property
 *  buf           - data buffer
 *  dims          - one-demnsional array of size 2
 *  dims[0] = MAXLENGTH
 *  dims[1] = number of elements of VL type
 *  len           - array element lengths
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dwrite_vl_integer_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
                      hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims, size_t_f *len)
/******/
{
    int    ret_value = -1;
    hid_t  c_dset_id;
    hid_t  c_mem_type_id;
    hid_t  c_mem_space_id;
    hid_t  c_file_space_id;
    hid_t  c_xfer_prp;
    herr_t status;
    int_f *tmp;
    size_t max_len;

    hvl_t  *c_buf;
    hsize_t i;
    hsize_t num_elem;

    max_len  = (size_t)dims[0];
    num_elem = (hsize_t)dims[1];

    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    c_xfer_prp      = (hid_t)*xfer_prp;

    c_buf = (hvl_t *)malloc((size_t)num_elem * sizeof(hvl_t));
    if (c_buf == NULL)
        return ret_value;
    tmp = (int_f *)buf;
    for (i = 0; i < num_elem; i++) {
        c_buf[i].len = (size_t)len[i];
        c_buf[i].p   = tmp;
        tmp          = tmp + max_len;
    }
    /*
     * Call H5Dwrite function.
     */
    status = H5Dwrite(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, c_buf);

    if (status < 0)
        goto DONE;
    ret_value = 0;
DONE:
    free(c_buf);
    return ret_value;
}

/****if* H5Df/h5dread_vl_integer_c
 * NAME
 *  h5dread_vl_integer_c
 * PURPOSE
 *  Read variable length dataset
 * INPUTS
 *  dset_id - identifier of the dataset
 *  mem_type_id - datatype identifier
 *  mem_space_id - dataspace identifier
 *  file_space_id - file dataspace identifier
 *  xfer          - file transfer property
 *  dims          - one-demnsional array of size 2
 *  dims[0] = MAXLENGTH
 *  dims[1] = number of elements of VL type
 * OUTPUTS
 *  buf           - data buffer
 *  len           - array element lengths
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dread_vl_integer_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
                     hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims, size_t_f *len)
/******/
{
    int    ret_value = -1;
    hid_t  c_dset_id;
    hid_t  c_mem_type_id;
    hid_t  c_mem_space_id;
    hid_t  c_file_space_id;
    hid_t  c_xfer_prp;
    herr_t status;
    size_t max_len;

    hvl_t   *c_buf;
    hsize_t  i;
    hssize_t num_elem;

    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    c_xfer_prp      = (hid_t)*xfer_prp;

    max_len  = (size_t)dims[0];
    num_elem = H5Sget_select_npoints(c_mem_space_id);
    if (num_elem != (hssize_t)dims[1])
        return ret_value;

    c_buf = (hvl_t *)malloc((size_t)num_elem * sizeof(hvl_t));
    if (c_buf == NULL)
        return ret_value;
    /*
     * Call H5Dread function.
     */
    status = H5Dread(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, c_buf);
    if (status < 0)
        goto DONE;
    for (i = 0; i < (hsize_t)num_elem; i++) {
        len[i] = (size_t_f)c_buf[i].len;
        memcpy(&buf[i * max_len], c_buf[i].p, c_buf[i].len * sizeof(int_f));
    }
    H5Treclaim(c_mem_type_id, c_mem_space_id, H5P_DEFAULT, c_buf);
    ret_value = 0;
DONE:
    free(c_buf);
    return ret_value;
}

/****if* H5Df/h5dwrite_vl_string_c
 * NAME
 *  h5dwrite_vl_string_c
 * PURPOSE
 *  Write variable length strings from Fortran program
 * INPUTS
 *  dset_id - identifier of the dataset
 *  mem_type_id - datatype identifier
 *  mem_space_id - dataspace identifier
 *  file_space_id - file dataspace identifier
 *  xfer          - file transfer property
 *  buf           - data buffer
 *  dims          - one-demnsional array of size 2
 *  dims[0] = number of strings of size max_len
 *  len           - array of strings lengths
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dwrite_vl_string_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
                     hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims, size_t_f *len)
/******/
{
    int    ret_value = -1;
    hid_t  c_dset_id;
    hid_t  c_mem_type_id;
    hid_t  c_mem_space_id;
    hid_t  c_file_space_id;
    hid_t  c_xfer_prp;
    herr_t status;
    char  *tmp, *tmp_p;
    size_t max_len;

    char  **c_buf;
    hsize_t i;
    hsize_t num_elem;

    max_len  = (size_t)dims[0];
    num_elem = (hsize_t)dims[1];

    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    c_xfer_prp      = (hid_t)*xfer_prp;

    /*
     * Allocate arra of character pointers
     */
    c_buf = (char **)malloc((size_t)num_elem * sizeof(char *));
    if (c_buf == NULL)
        return ret_value;

    /* Copy data to long C string */
    tmp = (char *)HD5f2cstring(buf, (size_t)(max_len * num_elem));
    if (tmp == NULL) {
        free(c_buf);
        return ret_value;
    }
    /*
     * Move data from temporary buffer
     */
    tmp_p = tmp;
    for (i = 0; i < num_elem; i++) {
        c_buf[i] = (char *)malloc((size_t)len[i] + 1);
        memcpy(c_buf[i], tmp_p, (size_t)len[i]);
        c_buf[i][len[i]] = '\0';
        tmp_p            = tmp_p + max_len;
    }

    /*
     * Call H5Dwrite function.
     */
    status = H5Dwrite(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, c_buf);

    if (status < 0)
        goto DONE;
    ret_value = 0;
DONE:
    H5Treclaim(c_mem_type_id, c_mem_space_id, H5P_DEFAULT, c_buf);
    free(c_buf);
    free(tmp);
    return ret_value;
}
/****if* H5Df/h5dread_vl_string_c
 * NAME
 *  h5dread_vl_string_c
 * PURPOSE
 *  Read variable length strings from Fortran program
 * INPUTS
 *  dset_id - identifier of the dataset
 *  mem_type_id - datatype identifier
 *  mem_space_id - dataspace identifier
 *  file_space_id - file dataspace identifier
 *  xfer          - file transfer property
 *  dims          - one-demnsional array of size 2
 *  dims[0] = number of strings of size max_len
 *  Output:      buf           - data buffer
 *  len           - array of strings lengths
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dread_vl_string_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
                    hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims, size_t_f *len)
/******/
{
    int    ret_value = -1;
    hid_t  c_dset_id;
    hid_t  c_mem_type_id;
    hid_t  c_mem_space_id;
    hid_t  c_file_space_id;
    hid_t  c_xfer_prp;
    herr_t status;
    char  *tmp, *tmp_p;
    size_t max_len;

    char  **c_buf;
    hsize_t i;
    hsize_t num_elem;

    max_len  = (size_t)dims[0];
    num_elem = (hsize_t)dims[1];

    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    c_xfer_prp      = (hid_t)*xfer_prp;

    /*
     * Allocate array of character pointers
     */
    c_buf = (char **)malloc((size_t)num_elem * sizeof(char *));
    if (c_buf == NULL)
        return ret_value;

    /*
     * Call H5Dread function.
     */
    status = H5Dread(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, c_buf);
    if (status < 0) {
        free(c_buf);
        return ret_value;
    }
    /* Copy data to long C string */
    tmp   = (char *)malloc((size_t)(max_len * num_elem) + 1);
    tmp_p = tmp;
    for (i = 0; i < max_len * num_elem; i++)
        tmp[i] = ' ';
    tmp[max_len * num_elem] = '\0';
    for (i = 0; i < num_elem; i++) {
        memcpy(tmp_p, c_buf[i], strlen(c_buf[i]));
        len[i] = (size_t_f)strlen(c_buf[i]);
        tmp_p  = tmp_p + max_len;
    }
    HD5packFstring(tmp, _fcdtocp(buf), (size_t)(max_len * num_elem));
    ret_value = 0;
    H5Treclaim(c_mem_type_id, c_mem_space_id, H5P_DEFAULT, c_buf);
    free(c_buf);
    free(tmp);
    return ret_value;
}

/****if* H5Df/h5dwrite_vl_real_c
 * NAME
 *  h5dwrite_vl_real_c
 * PURPOSE
 *  Write variable length dataset
 * INPUTS
 *  dset_id - identifier of the dataset
 *  mem_type_id - datatype identifier
 *  mem_space_id - dataspace identifier
 *  file_space_id - file dataspace identifier
 *  xfer          - file transfer property
 *  buf           - data buffer
 *  dims          - one-demnsional array of size 2
 *  dims[0] = MAXLENGTH
 *  dims[1] = number of elements of VL type
 *  len           - array element lengths
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dwrite_vl_real_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
                   hid_t_f *xfer_prp, real_f *buf, hsize_t_f *dims, size_t_f *len)
/******/
{
    int     ret_value = -1;
    hid_t   c_dset_id;
    hid_t   c_mem_type_id;
    hid_t   c_mem_space_id;
    hid_t   c_file_space_id;
    hid_t   c_xfer_prp;
    herr_t  status;
    real_f *tmp;
    size_t  max_len;

    hvl_t  *c_buf;
    hsize_t i;
    hsize_t num_elem;

    max_len  = (size_t)dims[0];
    num_elem = (hsize_t)dims[1];

    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    c_xfer_prp      = (hid_t)*xfer_prp;

    c_buf = (hvl_t *)malloc((size_t)num_elem * sizeof(hvl_t));
    if (c_buf == NULL)
        return ret_value;
    tmp = (real_f *)buf;
    for (i = 0; i < num_elem; i++) {
        c_buf[i].len = (size_t)len[i];
        c_buf[i].p   = tmp;
        tmp          = tmp + max_len;
    }
    /*
     * Call H5Dwrite function.
     */
    status = H5Dwrite(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, c_buf);

    if (status < 0)
        goto DONE;
    ret_value = 0;
DONE:
    free(c_buf);
    return ret_value;
}

/****if* H5Df/h5dread_vl_real_c
 * NAME
 *  h5dread_vl_real_c
 * PURPOSE
 *  Read variable length dataset
 * INPUTS
 *  dset_id - identifier of the dataset
 *  mem_type_id - datatype identifier
 *  mem_space_id - dataspace identifier
 *  file_space_id - file dataspace identifier
 *  xfer          - file transfer property
 *  dims          - one-demnsional array of size 2
 *  dims[0] = MAXLENGTH
 *  dims[1] = number of elements of VL type
 * OUTPUTS
 *  buf           - data buffer
 *  len           - array element lengths
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5dread_vl_real_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
                  hid_t_f *xfer_prp, real_f *buf, hsize_t_f *dims, size_t_f *len)
/******/
{
    int    ret_value = -1;
    hid_t  c_dset_id;
    hid_t  c_mem_type_id;
    hid_t  c_mem_space_id;
    hid_t  c_file_space_id;
    hid_t  c_xfer_prp;
    herr_t status;
    size_t max_len;

    hvl_t   *c_buf;
    hsize_t  i;
    hssize_t num_elem;

    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    c_xfer_prp      = (hid_t)*xfer_prp;

    max_len  = (size_t)dims[0];
    num_elem = H5Sget_select_npoints(c_mem_space_id);
    if (num_elem != (hssize_t)dims[1])
        return ret_value;

    c_buf = (hvl_t *)malloc((size_t)num_elem * sizeof(hvl_t));
    if (c_buf == NULL)
        return ret_value;
    /*
     * Call H5Dread function.
     */
    status = H5Dread(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, c_buf);
    if (status < 0)
        goto DONE;
    for (i = 0; i < (hsize_t)num_elem; i++) {
        len[i] = (size_t_f)c_buf[i].len;
        memcpy(&buf[i * max_len], c_buf[i].p, c_buf[i].len * sizeof(real_f));
    }

    H5Treclaim(c_mem_type_id, c_mem_space_id, H5P_DEFAULT, c_buf);
    ret_value = 0;
DONE:
    free(c_buf);
    return ret_value;
}

/****if* H5Df/h5dget_space_status_c
 * NAME
 *  h5dget_space_status_c
 * PURPOSE
 *  Call H5Dget_space_status to request dataspace allocation status
 * INPUTS
 *  dset_id - dataset identifier
 * OUTPUTS
 *  flag - status flag
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5dget_space_status_c(hid_t_f *dset_id, int_f *flag)
/******/
{
    int                ret_value = -1;
    herr_t             ret;
    hid_t              c_dset_id;
    H5D_space_status_t c_flag;

    c_dset_id = (hid_t)*dset_id;

    /*
     * Call H5Dget_space_status
     */
    ret = H5Dget_space_status(c_dset_id, &c_flag);

    if (ret < 0)
        return ret_value;
    *flag     = (int_f)c_flag;
    ret_value = 0;
    return ret_value;
}
/****if* H5Df/h5dcreate_anon_c
 * NAME
 *  h5dcreate_anon_c
 * PURPOSE
 *  Call H5Dcreate_anon
 * INPUTS
 *
 *		loc_id	   - Identifier of the file or group within which to create the dataset.
 *		type_id	   - Identifier of the datatype to use when creating the dataset.
 *		space_id   - Identifier of the dataspace to use when creating the dataset.
 *  dcpl_id    - Dataset creation property list identifier.
 *  dapl_id    - Dataset access property list identifier.
 * OUTPUTS
 *
 *  dset_id - dataset identifier
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5dcreate_anon_c(hid_t_f *loc_id, hid_t_f *type_id, hid_t_f *space_id, hid_t_f *dcpl_id, hid_t_f *dapl_id,
                 hid_t_f *dset_id)
/******/
{
    int ret_value = -1;

    /*
     * Call H5Dcreate2 function.
     */
    if ((*dset_id = (hid_t_f)H5Dcreate_anon((hid_t)*loc_id, (hid_t)*type_id, (hid_t)*space_id,
                                            (hid_t)*dcpl_id, (hid_t)*dapl_id)) < 0)
        goto DONE;

    ret_value = 0;

DONE:
    return ret_value;
}

/****if* H5Df/h5dwrite_f_c
 * NAME
 *  h5dwrite_f_c
 * PURPOSE
 *  Call H5Dwrite to write a dataset
 * INPUTS
 *  dset_id       - dataset identifier
 *  mem_type_id   - memory datatype identifier
 *  mem_space_id  - memory dataspace identifier
 *  file_space_id - memory dataspace identifier
 *  xfer_pr       - identifier of transfer property list
 *  buf           - data buffer
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5dwrite_f_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
             hid_t_f *xfer_prp, void *buf)
/******/
{
    int    ret_value = -1;
    herr_t ret;
    hid_t  c_dset_id;
    hid_t  c_mem_type_id;
    hid_t  c_mem_space_id;
    hid_t  c_file_space_id;
    hid_t  c_xfer_prp;

    /*
     * Define transfer property
     */
    c_xfer_prp = (hid_t)*xfer_prp;

    /*
     * Call H5Dwrite function.
     */
    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    ret             = H5Dwrite(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf);

    if (ret < 0)
        return ret_value;
    ret_value = 0;
    return ret_value;
}

/****if* H5Df/h5dread_f_c
 * NAME
 *  h5dread_f_c
 * PURPOSE
 *  Read variable length dataset
 * INPUTS
 *  dset_id       - identifier of the dataset
 *  mem_type_id   - datatype identifier
 *  mem_space_id  - dataspace identifier
 *  file_space_id - file dataspace identifier
 *  xfer          - file transfer property
 *  dims          - one-demnsional array of size 2
 *  dims[0] = MAXLENGTH
 *  dims[1] = number of elements of VL type
 * OUTPUTS
 *  buf - data buffer
 *  len - array element lengths
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5dread_f_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id,
            hid_t_f *xfer_prp, void *buf)
/******/
{
    int    ret_value = -1;
    hid_t  c_dset_id;
    hid_t  c_mem_type_id;
    hid_t  c_mem_space_id;
    hid_t  c_file_space_id;
    hid_t  c_xfer_prp;
    herr_t status;

    c_dset_id       = (hid_t)*dset_id;
    c_mem_type_id   = (hid_t)*mem_type_id;
    c_mem_space_id  = (hid_t)*mem_space_id;
    c_file_space_id = (hid_t)*file_space_id;
    c_xfer_prp      = (hid_t)*xfer_prp;
    /*
     * Call H5Dread function.
     */
    status = H5Dread(c_dset_id, c_mem_type_id, c_mem_space_id, c_file_space_id, c_xfer_prp, buf);
    if (status < 0)
        return ret_value;

    ret_value = 0;
    return ret_value;
}
/****if* H5Df/h5dget_access_plist_c
 * NAME
 *  h5dget_access_plist_c
 * PURPOSE
 *  Call H5Dget_access_plist
 * INPUTS
 *  dset_id   - dataset identifier
 * OUTPUTS
 *  plist_id  - the dataset access property list identifier
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5dget_access_plist_c(hid_t_f *dset_id, hid_t_f *plist_id)
/******/
{
    int ret_value = -1;
    /*
     * Call H5Dget_access_plist function.
     */
    if ((*plist_id = (hid_t_f)H5Dget_access_plist((hid_t)*dset_id)) < 0)
        goto DONE;

    ret_value = 0;

DONE:
    return ret_value;
}

/****if* H5Df/h5dvlen_reclaim_c
 * NAME
 *  h5dvlen_reclaim_c
 * PURPOSE
 *  Call H5Treclaim
 * INPUTS
 *  type_id   - Identifier of the datatype.
 *  space_id  - Identifier of the dataspace.
 *  plist_id  - Identifier of the property list used to create the buffer.
 *  buf       - Pointer to the buffer to be reclaimed.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5dvlen_reclaim_c(hid_t_f *type_id, hid_t_f *space_id, hid_t_f *plist_id, void *buf)
/******/
{
    int    ret_value = -1;
    herr_t status;

    /*
     * Call H5Treclaim function.
     */
    status = H5Treclaim((hid_t)*type_id, (hid_t)*space_id, (hid_t)*plist_id, buf);
    if (status < 0)
        return ret_value;

    ret_value = 0;
    return ret_value;
}
