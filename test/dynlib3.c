/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic document set and is     *
 * linked from the top-level documents page.  It can also be found at        *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have access   *
 * to either file, you may request a copy from help@hdfgroup.org.            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 * Programmer:	Raymond Lu
 *		1 April 2013
 *
 * Purpose:	Tests the plugin module (H5PL)
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "H5PLextern.h"

#define H5Z_FILTER_DYNLIB3      259
#define SUFFIX_LEN              8
#define GROUP_SUFFIX            ".h5group"

static size_t H5Z_filter_dynlib3(unsigned int flags, size_t cd_nelmts,
                const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_DYNLIB3[1] = {{
    H5Z_CLASS_T_VERS,                /* H5Z_class_t version             */
    H5Z_FILTER_DYNLIB3,		     /* Filter id number		*/
    1, 1,                            /* Encoding and decoding enabled   */
    "dynlib3",			     /* Filter name for debugging	*/
    NULL,                            /* The "can apply" callback        */
    NULL,                            /* The "set local" callback        */
    (H5Z_func_t)H5Z_filter_dynlib3,    /* The actual filter function	*/
}};

H5PL_type_t   H5PLget_plugin_type(void) {return H5PL_TYPE_FILTER;}
const void   *H5PLget_plugin_info(void) {return H5Z_DYNLIB3;}

/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_dynlib3
 *
 * Purpose:	A dynlib3 filter method that is used to test groups.  It 
 *              appends the suffix ".h5group" to each group name during 
 *              write and takes it out during read.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Raymond Lu
 *              1 April 2013
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_filter_dynlib3(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    size_t   ret_value;         /* Return value */

    /* Check for the correct number of parameters */
    if(cd_nelmts > 0)
        return(0);

    if(flags & H5Z_FLAG_REVERSE) { /*read*/
        ret_value = *buf_size = nbytes - SUFFIX_LEN;
    } /* end if */
    else { /*write*/
        void    *outbuf = NULL;     /* Pointer to new buffer */
        unsigned char *dst;         /* Temporary pointer to destination buffer */

	dst = (unsigned char *)(outbuf = malloc(nbytes + SUFFIX_LEN));

        /* Copy raw data */
        memcpy((void*)dst, (void*)(*buf), nbytes);

        /* Append suffix to raw data for storage */
        dst += nbytes;
        memcpy(dst, (void*)GROUP_SUFFIX, SUFFIX_LEN);

        /* Free input buffer */
 	free(*buf);

        /* Set return values */
        *buf_size = nbytes + SUFFIX_LEN;
	*buf = outbuf;
	outbuf = NULL;
	ret_value = *buf_size;
    } /* end else */

    return ret_value;
} /* H5Z_filter_dynlib3() */

