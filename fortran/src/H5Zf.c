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
#include "H5Zpublic.h"

/* Thin C helpers for Fortran BIND(C) -- Fortran cannot represent H5Z_params_t
   (a C union), so these helpers construct the appropriate H5Z_params_t value
   and call H5Pappend_filter. */

herr_t
H5Pappend_filter_str_c(hid_t plist, H5Z_filter_t id, unsigned flags, const char *params)
{
    H5Z_params_t p;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = params;
    return H5Pappend_filter(plist, id, flags, &p);
}

herr_t
H5Pappend_filter_raw_c(hid_t plist, H5Z_filter_t id, unsigned flags, size_t cd_nelmts,
                       const unsigned *cd_values)
{
    H5Z_params_t p;
    p.type            = H5Z_PARAMS_CDVALUES;
    p.u.raw.cd_nelmts = cd_nelmts;
    p.u.raw.cd_values = cd_values;
    return H5Pappend_filter(plist, id, flags, &p);
}

/* The same two calling modes for H5Pmodify_filter_by_idx.  The only
   difference from the append helpers above is that the entry is selected
   by pipeline index rather than by filter ID. */

herr_t
H5Pmodify_filter_by_idx_str_c(hid_t plist, unsigned filter_idx, unsigned flags, const char *params)
{
    H5Z_params_t p;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = params;
    return H5Pmodify_filter_by_idx(plist, filter_idx, flags, &p);
}

herr_t
H5Pmodify_filter_by_idx_raw_c(hid_t plist, unsigned filter_idx, unsigned flags, size_t cd_nelmts,
                              const unsigned *cd_values)
{
    H5Z_params_t p;
    p.type            = H5Z_PARAMS_CDVALUES;
    p.u.raw.cd_nelmts = cd_nelmts;
    p.u.raw.cd_values = cd_values;
    return H5Pmodify_filter_by_idx(plist, filter_idx, flags, &p);
}
