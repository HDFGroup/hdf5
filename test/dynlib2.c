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
 *		13 February 2013
 *
 * Purpose:	Tests the plugin module (H5PL)
 */

#include <stdlib.h>
#include <stdio.h>
#include "H5PLextern.h"

#define H5Z_FILTER_DYNLIB2      258 
#define MULTIPLIER              3

static size_t H5Z_filter_dynlib2(unsigned int flags, size_t cd_nelmts,
                const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_DYNLIB2[1] = {{
    H5Z_CLASS_T_VERS,                /* H5Z_class_t version             */
    H5Z_FILTER_DYNLIB2,		     /* Filter id number		*/
    1, 1,                            /* Encoding and decoding enabled   */
    "dynlib2",			     /* Filter name for debugging	*/
    NULL,                            /* The "can apply" callback        */
    NULL,                            /* The "set local" callback        */
    (H5Z_func_t)H5Z_filter_dynlib2,    /* The actual filter function	*/
}};

H5PL_type_t   H5PLget_plugin_type(void) {return H5PL_TYPE_FILTER;}
const void   *H5PLget_plugin_info(void) {return H5Z_DYNLIB2;}

/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_dynlib2
 *
 * Purpose:	A dynlib2 filter method that multiplies the original value 
 *              during write and divide the original value during read. It
 *              will be built as a shared library.  plugin.c test will load 
 *              and use this filter library.    
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Raymond Lu
 *              29 March 2013
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_filter_dynlib2(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    int *int_ptr = (int *)*buf;          /* Pointer to the data values */
    size_t buf_left = *buf_size;  /* Amount of data buffer left to process */

    /* Check for the correct number of parameters */
    if(cd_nelmts > 0)
        return(0);

    /* Assignment to eliminate unused parameter warning. */
    cd_values = cd_values;

    if(flags & H5Z_FLAG_REVERSE) { /*read*/
        /* Divide the original value with MULTIPLIER */
        while(buf_left > 0) {
            *int_ptr++ /= MULTIPLIER;
            buf_left -= sizeof(int);
        } /* end while */
    } /* end if */
    else { /*write*/
        /* Multiply the original value with MULTIPLIER */
        while(buf_left > 0) {
            *int_ptr++ *= MULTIPLIER;
            buf_left -= sizeof(int);
        } /* end while */
    } /* end else */

    return nbytes;
} /* end H5Z_filter_dynlib2() */

