/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:	Test dataset filter plugin for the filter_pluging.c test.
 *          This filter makes an HDF5 API call to ensure that works correctly.
 */

#include <stdlib.h>
#include <stdio.h>

#include "H5PLextern.h"

#define FILTER3_ID              259

#define PUSH_ERR(func, minor, str) H5Epush2(H5E_DEFAULT, __FILE__, func, __LINE__, H5E_ERR_CLS, H5E_PLUGIN, minor, str)

static size_t add_sub_value_hdf5(unsigned int flags, size_t cd_nelmts,
                const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* Filter class struct */
const H5Z_class2_t FILTER_INFO[1] = {{
    H5Z_CLASS_T_VERS,                   /* H5Z_class_t version              */
    FILTER3_ID,                         /* Filter ID number                 */
    1,                                  /* Encoding enabled                 */
    1,                                  /* Decoding enabled                 */
    "test filter plugin 3",             /* Filter name for debugging        */
    NULL,                               /* The "can apply" callback         */
    NULL,                               /* The "set local" callback         */
    (H5Z_func_t)add_sub_value_hdf5,     /* The actual filter function       */
}};

H5PL_type_t   H5PLget_plugin_type(void) {return H5PL_TYPE_FILTER;}
const void    *H5PLget_plugin_info(void) {return FILTER_INFO;}


/*-------------------------------------------------------------------------
 * Function:	add_sub_value_hdf5
 *
 * Purpose:     On write:
 *                  Adds a caller-supplied value to the element
 *              On read:
 *                  Subtracts a caller-supplied value from the element
 *
 *              NOTE:   This filter is identical to filter #1 only it makes
 *                      an HDF5 library call to ensure doing that doesn't
 *                      cause problems.
 *
 * Return:      Success:    Data chunk size in bytes
 *              Failure:    0
 *
 *-------------------------------------------------------------------------
 */
static size_t
add_sub_value_hdf5(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
        size_t nbytes, size_t *buf_size, void **buf)
{
    int *int_ptr = (int *)*buf;         /* Pointer to the data values               */
    size_t buf_left = *buf_size;        /* Amount of data buffer left to process    */
    int value = 0;                      /* Data value to add/subtract               */
    unsigned majnum = 0;                /* Output data from the HDF5 library call   */
    unsigned minnum = 0;
    unsigned relnum = 0;

    /* Check for the library version.
     * We don't do anything with this information - it's just to ensure that
     * HDF5 library calls work properly from inside filter plugins.
     */
    if (H5get_libversion(&majnum, &minnum, &relnum) < 0) {
        PUSH_ERR("filter plugin 3", H5E_CALLBACK, "H5get_libversion");
        return 0;
    }

    /* Check for the correct number of parameters */
    if (cd_nelmts == 0)
        return 0;

    /* Check that permanent parameters are set correctly */
    if (cd_values[0] > 9)
        return 0;

    /* Ensure that the version numbers match what was passed in.
     * Again, this is trivial work, just to ensure that the library calls are
     * working properly.
     */
    if (majnum != cd_values[1] || minnum != cd_values[2]) {
        PUSH_ERR("filter plugin 3", H5E_CALLBACK, "library versions do not match");
        return 0;
    }

    value = (int)cd_values[0];

    if (flags & H5Z_FLAG_REVERSE) {
        /* READ - Substract the given value from all the data values */
        while (buf_left > 0) {
            *int_ptr++ -= value;
            buf_left -= sizeof(int);
        }
    }
    else {
        /* WRITE - Add the given value to all the data values */
        while (buf_left > 0) {
            *int_ptr++ += value;
            buf_left -= sizeof(int);
        }
    }

    return nbytes;
} /* end add_sub_value_hdf5() */

