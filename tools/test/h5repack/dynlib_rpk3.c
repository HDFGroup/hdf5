/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 * Purpose:    Tests h5repack's "-f UD=<id>,<flags>,<key>=<value>,..." string-based
 *             filter configuration form.  This is a v3 (H5Z_class3_t) plugin
 *             whose set_config parses "mode=rate,rate=<N>" into cd_values, and
 *             whose get_config reconstructs the same string for h5dump -p's
 *             PARAMS_STRING output.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "H5PLextern.h"

#define H5Z_FILTER_DYNLIB3 261

static size_t H5Z_filter_dynlib3(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                                 hid_t dxpl_id, const hsize_t *scaled, size_t ndims, size_t nbytes,
                                 size_t *buf_size, void **buf);
static herr_t H5Z_dynlib3_set_config(const char *params, unsigned *flags, size_t *cd_nelmts,
                                     unsigned cd_values[], size_t cd_values_size);
static herr_t H5Z_dynlib3_get_config(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], char *buf,
                                     size_t *buf_size);

/* This message derives from H5Z */
static const H5Z_class3_t H5Z_DYNLIB3[1] = {{
    2,                                                          /* H5Z_class3_t version (literal 2) */
    H5Z_FILTER_DYNLIB3,                                        /* Filter id number               */
    1, 1,                                                      /* Encoding and decoding enabled  */
    "dynlib3",                                                 /* Filter name for debugging      */
    "Test filter with string-based configuration (mode=rate)", /* Description                   */
    NULL,                                                      /* The "can apply" callback       */
    NULL,                                                      /* The "set local" callback       */
    H5Z_filter_dynlib3,                                        /* The actual filter function     */
    H5Z_dynlib3_set_config,                                    /* String configuration setter    */
    H5Z_dynlib3_get_config,                                    /* String configuration getter    */
}};

H5PL_type_t
H5PLget_plugin_type(void)
{
    return H5PL_TYPE_FILTER;
}
const void *
H5PLget_plugin_info(void)
{
    return H5Z_DYNLIB3;
}

/*-------------------------------------------------------------------------
 * Function:    H5Z_dynlib3_set_config
 *
 * Purpose:     Parses "mode=rate,rate=<float>" into a single cd_value that
 *              encodes the rate to one decimal place (rate=3.5 -> 35), so
 *              that H5Z_dynlib3_get_config() can reconstruct it exactly.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_dynlib3_set_config(const char *params, unsigned *flags, size_t *cd_nelmts, unsigned cd_values[],
                       size_t cd_values_size)
{
    double rate = 0.0;

    (void)flags;

    *cd_nelmts = 1;

    /* Size-query pass: report how many cd_values slots are needed. */
    if (cd_values == NULL)
        return 0;

    if (cd_values_size < 1)
        return -1;

    if (params == NULL || H5Zconfig_get_double(params, "rate", &rate) <= 0)
        return -1;

    cd_values[0] = (unsigned)(rate * 10.0 + 0.5);

    return 0;
} /* end H5Z_dynlib3_set_config() */

/*-------------------------------------------------------------------------
 * Function:    H5Z_dynlib3_get_config
 *
 * Purpose:     Reconstructs "mode=rate,rate=<float>" from cd_values[0].
 *
 * Return:      Success:    0
 *              Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_dynlib3_get_config(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], char *buf,
                       size_t *buf_size)
{
    char   tmp[64];
    int    rv;
    size_t n;

    (void)flags;

    rv = snprintf(tmp, sizeof(tmp), "mode=rate,rate=%.1f", (cd_nelmts >= 1) ? (cd_values[0] / 10.0) : 0.0);
    if (rv < 0 || (size_t)rv >= sizeof(tmp))
        return -1;
    n = (size_t)rv;

    if (buf_size)
        *buf_size = n;
    if (buf)
        memcpy(buf, tmp, n);

    return 0;
} /* end H5Z_dynlib3_get_config() */

/*-------------------------------------------------------------------------
 * Function:    H5Z_filter_dynlib3
 *
 * Purpose:     Identity filter -- this plugin exists to exercise the
 *              string-based configuration path, not data transformation.
 *
 * Return:      Success:    Data chunk size (unchanged)
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_filter_dynlib3(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, hid_t dxpl_id,
                   const hsize_t *scaled, size_t ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)dxpl_id;
    (void)scaled;
    (void)ndims;
    (void)buf_size;
    (void)buf;

    return nbytes;
} /* end H5Z_filter_dynlib3() */
