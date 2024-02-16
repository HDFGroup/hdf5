/****h* H5Ef/H5Ef
 * PURPOSE
 *  This file contains C stubs for H5E Fortran APIs
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

/****if* H5Ef/h5eprint_c1
 * NAME
 *  h5eprint_c1
 * PURPOSE
 *  Call H5Eprint to print the error stack in a default manner.
 * INPUTS
 *  name    - file name
 *  namelen - length of name
 * OUTPUTS
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5eprint_c1(_fcd name, int_f *namelen)
/******/
{
    FILE *file      = NULL;
    char *c_name    = NULL;
    int_f ret_value = 0;

    if (NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL);
    if (NULL == (file = fopen(c_name, "a")))
        HGOTO_DONE(FAIL);

    /*
     * Call H5Eprint2 function.
     */
    if (H5Eprint2(H5E_DEFAULT, file) < 0)
        HGOTO_DONE(FAIL);

done:
    if (file)
        fclose(file);
    if (c_name)
        free(c_name);

    return ret_value;
}

/****if* H5Ef/h5eprint_c2
 * NAME
 *  h5eprint_c2
 * PURPOSE
 *  Call H5Eprint to print the error stack to stderr
 *  in a default manner.
 * INPUTS
 *
 * OUTPUTS
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5eprint_c2(void)
/******/
{
    int_f ret_value = 0;

    /*
     * Call H5Eprint2 function.
     */
    if (H5Eprint2(H5E_DEFAULT, NULL) < 0)
        HGOTO_DONE(FAIL);

done:
    return ret_value;
}

/****if* H5Ef/h5eget_major_c
 * NAME
 *  h5eget_major_c
 * PURPOSE
 *  Get a character string describing an error specified by a
 *  major error number.
 * INPUTS
 *  error_no - Major error number
 * OUTPUTS
 *  name - character string describing the error
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5eget_major_c(int_f *error_no, _fcd name, size_t_f *namelen)
/******/
{
    char  *c_name    = NULL;
    size_t c_namelen = (size_t)*namelen;
    int_f  ret_value = 0;

    if (c_namelen > 0)
        c_name = (char *)malloc(c_namelen + 1);

    if (!c_name)
        HGOTO_DONE(FAIL);

    /*
     * Call H5Eget_msg function.
     */
    H5Eget_msg((hid_t)*error_no, NULL, c_name, c_namelen);
    HD5packFstring((char *)c_name, _fcdtocp(name), c_namelen);
    if (!strcmp(c_name, "Invalid major error number"))
        HGOTO_DONE(FAIL);

done:
    if (c_name)
        free(c_name);

    return ret_value;
}

/****if* H5Ef/h5eget_minor_c
 * NAME
 *  h5eget_minor_c
 * PURPOSE
 *  Get a character string describing an error specified by a
 *  minor error number.
 * INPUTS
 *  error_no - Major error number
 * OUTPUTS
 *  name - character string describing the error
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5eget_minor_c(int_f *error_no, _fcd name, size_t_f *namelen)
/******/
{
    char  *c_name    = NULL;
    size_t c_namelen = (size_t)*namelen;
    int_f  ret_value = 0;

    if (c_namelen > 0)
        c_name = (char *)malloc(c_namelen + 1);

    if (!c_name)
        HGOTO_DONE(FAIL);

    /*
     * Call H5Eget_msg function.
     */
    H5Eget_msg((hid_t)*error_no, NULL, c_name, c_namelen);
    HD5packFstring((char *)c_name, _fcdtocp(name), c_namelen);
    if (!strcmp(c_name, "Invalid minor error number"))
        HGOTO_DONE(FAIL);

done:
    if (c_name)
        free(c_name);

    return ret_value;
}

/****if* H5Ef/h5eset_auto2_c
 * NAME
 *  h5eset_auto2_c
 * PURPOSE
 *  Calls H5Eset_auto2
 * INPUTS
 *  estack_id   - Error stack identifier.
 *  func 	- Function to be called upon an error condition.
 *  client_data - Data passed to the error function.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
/* int_f */
/* h5eset_auto2_c(hid_t_f *estack_id, H5E_auto2_t *func, void *client_data) */
/* /\******\/ */
/* { */
/*   int ret_val = -1; */
/*   herr_t status = -1; */

/*   status = H5Eset_auto2((hid_t)*estack_id, *func, client_data); */
/*   if (status >= 0) ret_val = 0; */
/*   return ret_val; */
/* } */

int_f
h5eset_auto2_c(int_f *printflag, hid_t_f *estack_id, H5E_auto2_t func, void *client_data)
/******/
{
    int    ret_val = -1;
    herr_t status  = -1;

    if (*printflag == 1 && *estack_id == -1)
        status = H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)H5Eprint2, stderr);
    else if (*printflag == 1)
        status = H5Eset_auto2((hid_t)*estack_id, func, client_data);
    else if (*printflag == 0)
        status = H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
    if (status >= 0)
        ret_val = 0;

    return ret_val;
}

int_f
h5epush_c(hid_t_f *err_stack, hid_t_f *cls_id, hid_t_f *maj_id, hid_t_f *min_id, _fcd msg, size_t_f *msg_len,
          char *file, char *func, int *line, const char *arg1, const char *arg2, char *arg3, char *arg4,
          char *arg5, char *arg6, char *arg7, char *arg8, char *arg9, char *arg10, char *arg11, char *arg12,
          char *arg13, char *arg14, char *arg15, char *arg16, char *arg17, char *arg18, char *arg19,
          char *arg20)
/******/
{

    char *c_msg     = NULL; /* Buffer to hold C string */
    int_f ret_value = 0;    /* Return value */

    /*
     * Convert FORTRAN name to C name
     */

    if (NULL == (c_msg = HD5f2cstring(msg, (size_t)*msg_len)))
        HGOTO_DONE(FAIL);

    if (H5Epush2((hid_t)*err_stack, file, func, (unsigned int)*line, (hid_t)*cls_id, (hid_t)*maj_id,
                 (hid_t)*min_id, c_msg, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                 arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_msg)
        free(c_msg);
    return ret_value;
}
