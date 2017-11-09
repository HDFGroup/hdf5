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

#define FILTER1_ID              257

static size_t add_sub_value(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* Filter class struct */
const H5Z_class2_t FILTER_INFO[1] = {{
    H5Z_CLASS_T_VERS,                   /* H5Z_class_t version              */
    FILTER1_ID,                         /* Filter ID number                 */
    1,                                  /* Encoding enabled                 */
    1,                                  /* Decoding enabled                 */
    "test filter plugin 1",             /* Filter name for debugging        */
    NULL,                               /* The "can apply" callback         */
    NULL,                               /* The "set local" callback         */
    (H5Z_func_t)add_sub_value,          /* The actual filter function       */
}};

H5PL_type_t   H5PLget_plugin_type(void) {return H5PL_TYPE_FILTER;}
const void    *H5PLget_plugin_info(void) {return FILTER_INFO;}


/*-------------------------------------------------------------------------
 * Function:	add_sub_value
 *
 * Purpose:     On write:
 *                  Adds a caller-supplied value to the element
 *              On read:
 *                  Subtracts a caller-supplied value from the element
 *
 * Return:      Success:    Data chunk size in bytes
 *              Failure:    0
 *
 *-------------------------------------------------------------------------
 */
static size_t
add_sub_value(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
        size_t nbytes, size_t *buf_size, void **buf)
{
    int *int_ptr = (int *)*buf;         /* Pointer to the data values               */
    size_t buf_left = *buf_size;        /* Amount of data buffer left to process    */
    int value = 0;                      /* Data value to add/subtract               */

    /* Check for the correct number of parameters */
    if (0 == cd_nelmts)
        return 0;

    /* Check that permanent parameters are set correctly */
    if (cd_values[0] > 9)
        return 0;
  
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

} /* end add_sub_value() */

