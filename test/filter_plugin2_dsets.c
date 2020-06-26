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
 */

#include <stdlib.h>
#include <stdio.h>

#include "H5PLextern.h"

#define FILTER2_ID              258
#define MULTIPLIER              3

static size_t mult_div_value(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* Filter class struct */
const H5Z_class2_t FILTER_INFO[1] = {{
    H5Z_CLASS_T_VERS,                   /* H5Z_class_t version              */
    FILTER2_ID,                         /* Filter ID number                 */
    1,                                  /* Encoding enabled                 */
    1,                                  /* Decoding enabled                 */
    "test filter plugin 2",             /* Filter name for debugging        */
    NULL,                               /* The "can apply" callback         */
    NULL,                               /* The "set local" callback         */
    (H5Z_func_t)mult_div_value,         /* The actual filter function       */
}};

H5PL_type_t   H5PLget_plugin_type(void) {return H5PL_TYPE_FILTER;}
const void    *H5PLget_plugin_info(void) {return FILTER_INFO;}


/*-------------------------------------------------------------------------
 * Function:	mult_div_value
 *
 * Purpose:     On write:
 *                  Multiplies an element by a constant value.
 *              On read:
 *                  Divides an element by a constant value.
 *
 * Return:      Success:    Data chunk size in bytes
 *              Failure:    0
 *
 *-------------------------------------------------------------------------
 */
static size_t
mult_div_value(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf)
{
    int *int_ptr = (int *)*buf;         /* Pointer to the data values               */
    size_t buf_left = *buf_size;        /* Amount of data buffer left to process    */

    /* Check for the correct number of parameters */
    if (cd_nelmts > 0)
        return 0;

    /* Assignment to eliminate unused parameter warning */
    cd_values = cd_values;

    if (flags & H5Z_FLAG_REVERSE) {
        /* READ - Divide the original value by MULTIPLIER */
        while (buf_left > 0) {
            *int_ptr++ /= MULTIPLIER;
            buf_left -= sizeof(int);
        }
    }
    else {
        /* WRITE - Multiply the original value by MULTIPLIER */
        while (buf_left > 0) {
            *int_ptr++ *= MULTIPLIER;
            buf_left -= sizeof(int);
        }
    }

    return nbytes;
} /* end mult_div_value() */

