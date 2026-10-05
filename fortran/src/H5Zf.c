/****h* H5Zf/H5Zf
 * PURPOSE
 *  This file contains C stubs for H5Z Fortran APIs
 *
 * COPYRIGHT
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 ******
 */

#include "H5f90.h"

/****if* H5Zf/h5zunregister_c
 * NAME
 *  h5zunregister_c
 * PURPOSE
 *  Call H5Zunregister to unregister filter
 * INPUTS
 *  filter identifier
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */
int_f
h5zunregister_c(int_f *filter)
/******/
{
    int          ret_value = -1;
    herr_t       status;
    H5Z_filter_t c_filter;

    /*
     * Call H5Zunregister function.
     */
    c_filter = (H5Z_filter_t)*filter;
    status   = H5Zunregister(c_filter);
    if (status < 0)
        return ret_value;
    ret_value = 0;
    return ret_value;
}
/****if* H5Zf/h5zfiletr_avail_c
 * NAME
 *  h5zfiletr_avail_c
 * PURPOSE
 *  Call H5Zfilter_avail to find if filter is available
 * INPUTS
 *  filter - filter identifier
 * OUTPUTS
 *  flag - status flag
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5zfilter_avail_c(int_f *filter, int_f *flag)
/******/
{
    int          ret_value = 0;
    H5Z_filter_t c_filter;
    htri_t       status;

    c_filter = (H5Z_filter_t)*filter;
    status   = H5Zfilter_avail(c_filter);
    *flag    = (int_f)status;
    if (status < 0)
        ret_value = -1;
    return ret_value;
}

/****if* H5Zf/h5zget_filter_info_c
 * NAME
 *  h5zget_filter_info_c
 * PURPOSE
 *  Call H5Zget_filter_info to find if filter has its encoder
 *  and/or its decoder available
 * INPUTS
 *  filter - filter identifier
 * OUTPUTS
 *  flag - status flag
 * RETURNS
 *  0 on success, -1 on failure
 * SOURCE
 */

int_f
h5zget_filter_info_c(int_f *filter, int_f *flag)
/******/
{
    int          ret_value = 0;
    H5Z_filter_t c_filter;
    unsigned int c_flag;

    c_filter  = (H5Z_filter_t)*filter;
    ret_value = H5Zget_filter_info(c_filter, &c_flag);
    *flag     = (int_f)c_flag;

    return ret_value;
}

/* Fortran cannot represent H5Z_params_t (a C union); these BIND(C) helpers
   build it before calling H5Pappend_filter. */

herr_t
h5pappend_filter_str_c(hid_t plist, H5Z_filter_t id, unsigned flags, const char *params)
{
    H5Z_params_t p;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = params;
    return H5Pappend_filter(plist, id, flags, &p);
}

herr_t
h5pappend_filter_raw_c(hid_t plist, H5Z_filter_t id, unsigned flags, size_t cd_nelmts,
                       const unsigned *cd_values)
{
    H5Z_params_t p;
    p.type            = H5Z_PARAMS_CDVALUES;
    p.u.raw.cd_nelmts = cd_nelmts;
    p.u.raw.cd_values = cd_values;
    return H5Pappend_filter(plist, id, flags, &p);
}

/* Same two calling modes for H5Pmodify_filter_by_idx, selecting the filter
   by pipeline index instead of filter ID. */

herr_t
h5pmodify_filter_by_idx_str_c(hid_t plist, unsigned filter_idx, unsigned flags, const char *params)
{
    H5Z_params_t p;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = params;
    return H5Pmodify_filter_by_idx(plist, filter_idx, flags, &p);
}

herr_t
h5pmodify_filter_by_idx_raw_c(hid_t plist, unsigned filter_idx, unsigned flags, size_t cd_nelmts,
                              const unsigned *cd_values)
{
    H5Z_params_t p;
    p.type            = H5Z_PARAMS_CDVALUES;
    p.u.raw.cd_nelmts = cd_nelmts;
    p.u.raw.cd_values = cd_values;
    return H5Pmodify_filter_by_idx(plist, filter_idx, flags, &p);
}
