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
 * Purpose:	Test group filter plugin for the filter_pluging.c test.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "H5PLextern.h"

#define FILTER4_ID              260
#define SUFFIX_LEN              8
#define GROUP_SUFFIX            ".h5group"

static size_t append_to_group_name(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* Filter class struct */
const H5Z_class2_t FILTER_INFO[1] = {{
    H5Z_CLASS_T_VERS,                   /* H5Z_class_t version              */
    FILTER4_ID,                         /* Filter ID number                 */
    1,                                  /* Encoding enabled                 */
    1,                                  /* Decoding enabled                 */
    "test filter plugin 4",             /* Filter name for debugging        */
    NULL,                               /* The "can apply" callback         */
    NULL,                               /* The "set local" callback         */
    (H5Z_func_t)append_to_group_name,   /* The actual filter function       */
}};

H5PL_type_t   H5PLget_plugin_type(void) {return H5PL_TYPE_FILTER;}
const void    *H5PLget_plugin_info(void) {return FILTER_INFO;}


/*-------------------------------------------------------------------------
 * Function:	append_to_group_name
 *
 * Purpose:     On write:
 *                  Appends the suffix ".h5group" to the group name
 *              On read:
 *                  Removes the ".h5group" suffix from the group name
 *
 * Return:      Success:    Data size in bytes
 *              Failure:    0
 *
 *-------------------------------------------------------------------------
 */
static size_t
append_to_group_name(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf)
{
    size_t   new_name_size = 0;             /* Return value */

    /* Check for the correct number of parameters */
    if (cd_nelmts > 0)
        return 0;

    /* Assignment to eliminate unused parameter warning. */
    cd_values = cd_values;

    if (flags & H5Z_FLAG_REVERSE) {
        /* READ - Remove the suffix from the group name */
        new_name_size = *buf_size = nbytes - SUFFIX_LEN;
    }
    else {
        /* WRITE - Append the suffix to the group name */
        void    *outbuf = NULL;         /* Pointer to new buffer                    */
        unsigned char *dst = NULL;      /* Temporary pointer to destination buffer  */

        /* Get memory for the new, larger string buffer using the
         * library's memory allocator.
         */
        if (NULL == (dst = (unsigned char *)(outbuf = H5allocate_memory(nbytes + SUFFIX_LEN, 0))))
            return 0;

        /* Copy raw data */
        memcpy((void *)dst, (const void *)(*buf), nbytes);

        /* Append suffix to raw data for storage */
        dst += nbytes;
        memcpy((void *)dst, (const void *)GROUP_SUFFIX, SUFFIX_LEN);

        /* Free the passed-in buffer using the library's allocator */
        H5free_memory(*buf);

        /* Set return values */
        *buf_size = nbytes + SUFFIX_LEN;
        *buf = outbuf;
        outbuf = NULL;
        new_name_size = *buf_size;
    }

    return new_name_size;
} /* append_to_group_name() */

