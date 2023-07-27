/****h* H5Of/H5Of
 * PURPOSE
 *  This file contains C stubs for H5O Fortran APIs
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
int_f fill_h5o_info_t_f(H5O_info2_t Oinfo, H5O_info_t_f *object_info);

int_f
fill_h5o_info_t_f(H5O_info2_t Oinfo, H5O_info_t_f *object_info)
{

    /* This function does not used the field parameter because we want
     * this function to fill the unfilled fields with C's default values.
     */

    struct tm *ts;

    object_info->fileno = Oinfo.fileno;
    object_info->token  = Oinfo.token;

    object_info->type = (int_f)Oinfo.type;
    object_info->rc   = (int_f)Oinfo.rc;

    ts = gmtime(&Oinfo.atime);

    object_info->atime[0] = (int_f)ts->tm_year + 1900; /* year starts at 1900 */
    object_info->atime[1] = (int_f)ts->tm_mon + 1;     /* month starts at 0 in C */
    object_info->atime[2] = (int_f)ts->tm_mday;
    object_info->atime[3] = 0; /* time is expressed as UTC (or GMT timezone) */
    object_info->atime[4] = (int_f)ts->tm_hour;
    object_info->atime[5] = (int_f)ts->tm_min;
    object_info->atime[6] = (int_f)ts->tm_sec;
    object_info->atime[7] = -32767; /* millisecond is not available, assign it -HUGE(0) */

    ts = gmtime(&Oinfo.btime);

    object_info->btime[0] = (int_f)ts->tm_year + 1900; /* year starts at 1900 */
    object_info->btime[1] = (int_f)ts->tm_mon + 1;     /* month starts at 0 in C */
    object_info->btime[2] = (int_f)ts->tm_mday;
    object_info->btime[3] = 0; /* time is expressed as UTC (or GMT timezone) */
    object_info->btime[4] = (int_f)ts->tm_hour;
    object_info->btime[5] = (int_f)ts->tm_min;
    object_info->btime[6] = (int_f)ts->tm_sec;
    object_info->btime[7] = -32767; /* millisecond is not available, assign it -HUGE(0) */

    ts = gmtime(&Oinfo.ctime);

    object_info->ctime[0] = (int_f)ts->tm_year + 1900; /* year starts at 1900 */
    object_info->ctime[1] = (int_f)ts->tm_mon + 1;     /* month starts at 0 in C */
    object_info->ctime[2] = (int_f)ts->tm_mday;
    object_info->ctime[3] = 0; /* time is expressed as UTC (or GMT timezone) */
    object_info->ctime[4] = (int_f)ts->tm_hour;
    object_info->ctime[5] = (int_f)ts->tm_min;
    object_info->ctime[6] = (int_f)ts->tm_sec;
    object_info->ctime[7] = -32767; /* millisecond is not available, assign it -HUGE(0) */

    ts = gmtime(&Oinfo.mtime);

    object_info->mtime[0] = (int_f)ts->tm_year + 1900; /* year starts at 1900 */
    object_info->mtime[1] = (int_f)ts->tm_mon + 1;     /* month starts at 0 in C */
    object_info->mtime[2] = (int_f)ts->tm_mday;
    object_info->mtime[3] = 0; /* time is expressed as UTC (or GMT timezone) */
    object_info->mtime[4] = (int_f)ts->tm_hour;
    object_info->mtime[5] = (int_f)ts->tm_min;
    object_info->mtime[6] = (int_f)ts->tm_sec;
    object_info->mtime[7] = -32767; /* millisecond is not available, assign it -HUGE(0) */

    object_info->num_attrs = (hsize_t_f)Oinfo.num_attrs;

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
 * SOURCE
 */
int_f
h5olink_c(hid_t_f *object_id, hid_t_f *new_loc_id, _fcd name, size_t_f *namelen, hid_t_f *lcpl_id,
          hid_t_f *lapl_id)
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
     * Call H5Olink function.
     */
    if ((hid_t_f)H5Olink((hid_t)*object_id, (hid_t)*new_loc_id, c_name, (hid_t)*lcpl_id, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_name)
        free(c_name);
    return ret_value;
}

/****if* H5Of/h5oopen_by_token_c
 * NAME
 *  h5oopen_by_token_c
 * PURPOSE
 *  Calls H5open_by_token
 * INPUTS
 *  loc_id - File or group identifier
 *  token  - Object's token in the file
 *
 * OUTPUTS
 *  obj_id  - Object identifier
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5oopen_by_token_c(hid_t_f *loc_id, H5O_token_t *token, hid_t_f *obj_id)
/******/
{
    int_f ret_value = 0; /* Return value */

    /*
     * Call H5Oopen_by_token function.
     */
    if ((*obj_id = (hid_t_f)H5Oopen_by_token((hid_t)*loc_id, *token)) < 0)
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
 *  file         - Filename the async subroutine is being called from
 *  func         - Function name the async subroutine is being called in
 *  line         - Line number the async subroutine is being called at
 *  es_id        - Event set identifier
 *
 * OUTPUTS
 *  object_info  - Buffer in which to return object information.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5oget_info_by_name_c(hid_t_f *loc_id, char *name, hid_t_f *lapl_id, H5O_info_t_f *object_info, int_f *fields,
                      hid_t_f *es_id, char *file, char *func, int_f *line)
/******/
{
    int_f       ret_value = 0; /* Return value */
    H5O_info2_t Oinfo;

    /*
     * Call H5Oinfo_by_name function.
     */

    if ((hid_t)*es_id != -1) {
        if (H5Oget_info_by_name3((hid_t)*loc_id, name, &Oinfo, (unsigned)*fields, (hid_t)*lapl_id) < 0)
            HGOTO_DONE(FAIL);
    }
    else {
        if (H5Oget_info_by_name_async_wrap(file, func, (unsigned)*line, (hid_t)*loc_id, name, &Oinfo,
                                           (unsigned)*fields, (hid_t)*lapl_id, (hid_t)*es_id) < 0)
            HGOTO_DONE(FAIL);
    }

    ret_value = fill_h5o_info_t_f(Oinfo, object_info);

done:
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
 * SOURCE
 */
int_f
h5oget_info_by_idx_c(hid_t_f *loc_id, _fcd group_name, size_t_f *namelen, int_f *index_field, int_f *order,
                     hsize_t_f *n, hid_t_f *lapl_id, H5O_info_t_f *object_info, int_f *fields)
/******/
{
    char           *c_group_name = NULL; /* Buffer to hold C string */
    int_f           ret_value    = 0;    /* Return value */
    H5O_info2_t     Oinfo;
    H5_index_t      c_index_field;
    H5_iter_order_t c_order;

    /*
     * Convert FORTRAN name to C name
     */
    if ((c_group_name = HD5f2cstring(group_name, (size_t)*namelen)) == NULL)
        HGOTO_DONE(FAIL);

    c_index_field = (H5_index_t)*index_field;
    c_order       = (H5_iter_order_t)*order;

    /*
     * Call H5Oinfo_by_idx function.
     */
    if (H5Oget_info_by_idx3((hid_t)*loc_id, c_group_name, c_index_field, c_order, (hsize_t)*n, &Oinfo,
                            (unsigned)*fields, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

    ret_value = fill_h5o_info_t_f(Oinfo, object_info);

done:
    if (c_group_name)
        free(c_group_name);
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
 * SOURCE
 */
int_f
h5oget_info_c(hid_t_f *object_id, H5O_info_t_f *object_info, int_f *fields)
/******/
{
    int_f       ret_value = 0; /* Return value */
    H5O_info2_t Oinfo;

    /*
     * Call H5Oinfo_by_name function.
     */
    if (H5Oget_info3((hid_t)*object_id, &Oinfo, (unsigned)*fields) < 0)
        HGOTO_DONE(FAIL);

    ret_value = fill_h5o_info_t_f(Oinfo, object_info);

done:
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
 * SOURCE
 */
int_f
h5odecr_refcount_c(hid_t_f *object_id)
/******/
{
    int_f ret_value = 0; /* Return value */

    /*
     * Call H5Odecr_refcount function.
     */
    if ((hid_t_f)H5Odecr_refcount((hid_t)*object_id) < 0)
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
 * SOURCE
 */
int_f
h5oexists_by_name_c(hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id)
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
     * Call H5Oopen function.
     */
    if ((ret_value = (int_f)H5Oexists_by_name((hid_t)*loc_id, c_name, (hid_t)*lapl_id)) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_name)
        free(c_name);
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
 * SOURCE
 */
int_f
h5oincr_refcount_c(hid_t_f *object_id)
/******/
{
    int_f ret_value = 0; /* Return value */

    /*
     * Call H5Oincr_refcount function.
     */
    if ((hid_t_f)H5Oincr_refcount((hid_t)*object_id) < 0)
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
 * SOURCE
 */
int_f
h5oset_comment_c(hid_t_f *object_id, _fcd comment, size_t_f *commentlen)
/******/
{
    char *c_comment = NULL; /* Buffer to hold C string */
    int_f ret_value = 0;    /* Return value */

    /*
     * Convert FORTRAN string to C string
     */
    if ((c_comment = HD5f2cstring(comment, (size_t)*commentlen)) == NULL)
        HGOTO_DONE(FAIL);

    /*
     * Call H5Oset_comment function.
     */
    if ((hid_t_f)H5Oset_comment((hid_t)*object_id, c_comment) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_comment)
        free(c_comment);
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
 * SOURCE
 */
int_f
h5oset_comment_by_name_c(hid_t_f *object_id, _fcd name, size_t_f *namelen, _fcd comment, size_t_f *commentlen,
                         hid_t_f *lapl_id)
/******/
{
    char *c_comment = NULL; /* Buffer to hold C string */
    char *c_name    = NULL; /* Buffer to hold C string */
    int_f ret_value = 0;    /* Return value */

    /*
     * Convert FORTRAN string to C string
     */
    if ((c_comment = HD5f2cstring(comment, (size_t)*commentlen)) == NULL)
        HGOTO_DONE(FAIL);
    /*
     * Convert FORTRAN string to C string
     */
    if ((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
        HGOTO_DONE(FAIL);

    /*
     * Call H5Oset_comment_by_name function.
     */
    if ((hid_t_f)H5Oset_comment_by_name((hid_t)*object_id, c_name, c_comment, (hid_t)*lapl_id) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_name)
        free(c_name);
    if (c_comment)
        free(c_comment);
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
 * SOURCE
 */
int_f
h5oget_comment_c(hid_t_f *object_id, _fcd comment, size_t_f *commentsize, hssize_t_f *bufsize)
/******/
{
    char  *c_comment = NULL; /* Buffer to hold C string */
    int_f  ret_value = 0;    /* Return value */
    size_t c_commentsize;

    c_commentsize = (size_t)*commentsize + 1;

    /*
     * Allocate buffer to hold comment name
     */

    if (NULL == (c_comment = (char *)malloc(c_commentsize)))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Oget_comment function.
     */

    if ((*bufsize = (hssize_t_f)H5Oget_comment((hid_t)*object_id, c_comment, (size_t)*commentsize)) < 0)
        HGOTO_DONE(FAIL);

    /*
     * Convert C name to FORTRAN and place it in the given buffer
     */
    if (c_comment)
        HD5packFstring(c_comment, _fcdtocp(comment), c_commentsize - 1);
    return ret_value;

done:
    if (c_comment)
        free(c_comment);

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
 * SOURCE
 */
int_f
h5oget_comment_by_name_c(hid_t_f *loc_id, _fcd name, size_t_f *name_size, _fcd comment, size_t_f *commentsize,
                         size_t_f *bufsize, hid_t_f *lapl_id)
/******/
{
    char   *c_comment = NULL; /* Buffer to hold C string */
    char   *c_name    = NULL; /* Buffer to hold C string */
    int_f   ret_value = 0;    /* Return value */
    ssize_t c_bufsize;
    size_t  c_commentsize;

    /*
     * Convert FORTRAN string to C string
     */
    if ((c_name = HD5f2cstring(name, (size_t)*name_size)) == NULL)
        HGOTO_DONE(FAIL);

    c_commentsize = (size_t)*commentsize + 1;

    /*
     * Allocate buffer to hold comment name
     */

    if (NULL == (c_comment = (char *)malloc(c_commentsize)))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Oget_comment_by_name function.
     */

    if ((c_bufsize = H5Oget_comment_by_name((hid_t)*loc_id, c_name, c_comment, (size_t)*commentsize,
                                            (hid_t)*lapl_id)) < 0)
        HGOTO_DONE(FAIL);

    if (c_name)
        free(c_name);

    *bufsize = (size_t_f)c_bufsize;

    /*
     * Convert C name to FORTRAN and place it in the given buffer
     */
    if (c_comment) {
        HD5packFstring(c_comment, _fcdtocp(comment), c_commentsize - 1);
        free(c_comment);
    }

    return ret_value;

done:
    if (c_comment)
        free(c_comment);
    if (c_name)
        free(c_name);

    return ret_value;
}

/****if* H5Of/h5otoken_cmp_c
 * NAME
 *  h5otoken_cmp_c
 * PURPOSE
 *  Calls H5Otoken_cmp
 * INPUTS
 *  loc_id  - Identifier of an object in the file / container.
 *  token1  - The first token to compare.
 *  token2  - The second token to compare.
 *  cmp_value - Whether the tokens are equal.
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5otoken_cmp_c(hid_t_f *loc_id, H5O_token_t *token1, H5O_token_t *token2, int_f *cmp_value_f)
/******/
{
    int   cmp_value;     /* Token comparison result */
    int_f ret_value = 0; /* Return value */

    /* Call H5Otoken_cmp function */
    cmp_value = 0;
    if (H5Otoken_cmp((hid_t)*loc_id, token1, token2, &cmp_value) < 0)
        HGOTO_DONE(FAIL);

    /* Set the comparison value to return */
    *cmp_value_f = cmp_value;

done:
    return ret_value;
}
