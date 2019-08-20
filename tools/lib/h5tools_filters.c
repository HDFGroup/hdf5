/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"
#include "h5tools.h"

/*-------------------------------------------------------------------------
 * print a warning message
 *-------------------------------------------------------------------------
 */
static void print_filter_warning(const char *dname, const char *fname)
{
    HDfprintf(stderr,
            "Warning: dataset <%s> cannot be read, %s filter is not available\n",
            dname, fname);
}

/*-------------------------------------------------------------------------
 * Function: h5tools_canreadf
 *
 * Purpose:  check if the dataset creation property list has filters that
 *           are not registered in the current configuration
 *               1) the external filters GZIP and SZIP might not be available
 *               2) the internal filters might be turned off
 *
 * Return:
 *           1 can read,
 *           0 cannot,
 *           -1 error
 *-------------------------------------------------------------------------
 */
int
h5tools_canreadf(const char* name,     /* object name, serves also as boolean print */
                     hid_t dcpl_id)    /* dataset creation property list */
{
    int ret_value = 1;
    int nfilters;       /* number of filters */
    H5Z_filter_t filtn; /* filter identification number */
    int i;              /* index */
    int udfilter_avail; /* index */

    /* get information about filters */
    if ((nfilters = H5Pget_nfilters(dcpl_id)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_nfilters failed");

    /* if we do not have filters, we can read the dataset safely */
    if (!nfilters)
        HGOTO_DONE(1);

    /* check availability of filters */
    for (i = 0; i < nfilters; i++) {
        if ((filtn = H5Pget_filter2(dcpl_id, (unsigned) i, 0, 0, 0, (size_t) 0, 0, NULL)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_filter2 failed");

        switch (filtn) {
        /*-------------------------------------------------------------------------
         * user defined filter
         *-------------------------------------------------------------------------
         */
        default:
            if ((udfilter_avail = H5Zfilter_avail(filtn)) < 0) {
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Zfilter_avail failed");
            }
            else if (!udfilter_avail) {
                if (name)
                    print_filter_warning(name, "user defined");
                ret_value = 0;
            }
            break;

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_DEFLATE      1 , deflation like gzip
             *-------------------------------------------------------------------------
             */
        case H5Z_FILTER_DEFLATE:
#ifndef H5_HAVE_FILTER_DEFLATE
            if (name)
                print_filter_warning(name,"deflate");
            ret_value = 0;
#endif
            break;
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SZIP       4 , szip compression
             *-------------------------------------------------------------------------
             */
        case H5Z_FILTER_SZIP:
#ifndef H5_HAVE_FILTER_SZIP
            if (name)
                print_filter_warning(name,"SZIP");
            ret_value = 0;
#endif
            break;
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SHUFFLE    2 , shuffle the data
             *-------------------------------------------------------------------------
             */
        case H5Z_FILTER_SHUFFLE:
            break;
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_FLETCHER32 3 , fletcher32 checksum of EDC
             *-------------------------------------------------------------------------
             */
        case H5Z_FILTER_FLETCHER32:
            break;
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_NBIT
             *-------------------------------------------------------------------------
             */
        case H5Z_FILTER_NBIT:
            break;
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SCALEOFFSET
             *-------------------------------------------------------------------------
             */
        case H5Z_FILTER_SCALEOFFSET:
            break;
        }/*switch*/
    }/*for*/

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_canwritef
 *
 * Purpose:  check if the filter is available and can write data.
 *
 * Return:   1 can write,
 *           0 cannot,
 *           -1 error
 *-------------------------------------------------------------------------
 */
H5_ATTR_CONST int
h5tools_can_encode(H5Z_filter_t filtn)
{
    int ret_value = 1;

    switch (filtn) {
    /* user defined filter     */
    default:
        HGOTO_DONE(0)
    case H5Z_FILTER_DEFLATE:
#ifndef H5_HAVE_FILTER_DEFLATE
        HGOTO_DONE(0)
#endif
            break;

    case H5Z_FILTER_SZIP:
#ifndef H5_HAVE_FILTER_SZIP
        HGOTO_DONE(0)
#else
        {
            unsigned int filter_config_flags;

            if (H5Zget_filter_info(filtn, &filter_config_flags) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Zget_filter_info failed");
            if ((filter_config_flags
                    & (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) == 0) {
                /* filter present but neither encode nor decode is supported (???) */
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "neither encode nor decode is supported");
            }
            else if ((filter_config_flags
                        & (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) == H5Z_FILTER_CONFIG_DECODE_ENABLED) {
                /* decoder only: read but not write */
                HGOTO_DONE(0)
            }
            else if ((filter_config_flags
                        & (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) == H5Z_FILTER_CONFIG_ENCODE_ENABLED) {
                /* encoder only: write but not read (???) */
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "encoder only: write but not read");
            }
            else if ((filter_config_flags
                        & (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED))
                        == (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) {
                HGOTO_DONE(1)
            }
        }
#endif
            break;

    case H5Z_FILTER_SHUFFLE:
            break;

    case H5Z_FILTER_FLETCHER32:
            break;

    case H5Z_FILTER_NBIT:
            break;

    case H5Z_FILTER_SCALEOFFSET:
            break;
    }/*switch*/

done:
    return ret_value;
}

