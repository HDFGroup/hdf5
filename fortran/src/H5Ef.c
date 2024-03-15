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

/****if* H5Ef/h5eprint_c
 * NAME
 *  h5eprint_c
 * PURPOSE
 *  Call H5Eprint to print the error stack in a default manner.
 * INPUTS
 *  err_stack - error stack identifier
 *  name      - file name
 *  namelen   - length of name
 * OUTPUTS
 *
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5eprint_c(hid_t_f *err_stack, _fcd name, size_t_f *namelen)
/******/
{
    FILE *file      = NULL;
    char *c_name    = NULL;
    int_f ret_value = 0;

    if (namelen) {
        if (NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
            HGOTO_DONE(FAIL);
        if (NULL == (file = fopen(c_name, "a")))
            HGOTO_DONE(FAIL);
    }

    /*
     * Call H5Eprint2 function.
     */
    if (H5Eprint2((hid_t)*err_stack, file) < 0)
        HGOTO_DONE(FAIL);

done:
    if (file)
        fclose(file);
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
h5epush_c(hid_t_f *err_stack, _fcd file, int_f *file_len, _fcd func, int_f *func_len, int line,
          hid_t_f *cls_id, hid_t_f *maj_id, hid_t_f *min_id, _fcd msg, int_f *msg_len, const char *arg1,
          const char *arg2, const char *arg3, const char *arg4, const char *arg5, const char *arg6,
          const char *arg7, const char *arg8, const char *arg9, const char *arg10, const char *arg11,
          const char *arg12, const char *arg13, const char *arg14, const char *arg15, const char *arg16,
          const char *arg17, const char *arg18, const char *arg19, const char *arg20)
/******/
{

    char *c_file    = NULL; /* Buffer to hold C string */
    char *c_func    = NULL; /* Buffer to hold C string */
    char *c_msg     = NULL; /* Buffer to hold C string */
    int_f ret_value = 0;    /* Return value */

    /*
     * Convert FORTRAN string to C string
     */
    if (NULL == (c_file = HD5f2cstring(file, (size_t)*file_len)))
        HGOTO_DONE(FAIL);
    if (NULL == (c_func = HD5f2cstring(func, (size_t)*func_len)))
        HGOTO_DONE(FAIL);
    if (NULL == (c_msg = HD5f2cstring(msg, (size_t)*msg_len)))
        HGOTO_DONE(FAIL);

    if (H5Epush2((hid_t)*err_stack, c_file, c_func, (unsigned int)line, (hid_t)*cls_id, (hid_t)*maj_id,
                 (hid_t)*min_id, c_msg, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                 arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20) < 0)
        HGOTO_DONE(FAIL);

done:
    if (c_msg)
        free(c_msg);
    return ret_value;
}
