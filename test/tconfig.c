/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* $Id$ */

/***********************************************************
*
* Test program:  tconfig
*
* Test the definitions in the H5config.h as much as possible
*
*************************************************************/

#include "hdf5.h"
#include "H5private.h"
#include "testhdf5.h"

/* macros definitions */
/* verify C type sizes */
#define vrfy_ctype(ctype, ctype_macro) \
    if (sizeof(ctype) != ctype_macro){ \
	print_func("Error verifying %s expected: %d, got: %d\n", \
	    #ctype_macro, ctype_macro, sizeof(ctype)); \
	    num_errs++; \
    }

/* local routine prototypes */
void test_config_ctypes(void);


/*-------------------------------------------------------------------------
 * Function:	test_configure
 *
 * Purpose:	Main configure definitions testing routine
 *
 * Return:	none (error is fed back via global variable num_errs)
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void 
test_configure(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing configure definitions\n"));
    test_config_ctypes();
}


/*-------------------------------------------------------------------------
 * Function:	cleanup_configure
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_configure(void)
{
    /* no file to clean */
}


/*-------------------------------------------------------------------------
 * Function:	test_config_ctypes
 *
 * Purpose:	test C language data type sizes
 *
 * Return:	none (error is fed back via global variable num_errs)
 *
 * Programmer:	Albert Cheng
 *              September 25, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void 
test_config_ctypes(void)
{
    /* standard basic types */
    vrfy_ctype(char, H5_SIZEOF_CHAR);
    vrfy_ctype(int, H5_SIZEOF_INT);
    vrfy_ctype(short, H5_SIZEOF_SHORT);
    vrfy_ctype(long, H5_SIZEOF_LONG);
    vrfy_ctype(float, H5_SIZEOF_FLOAT);
    vrfy_ctype(double, H5_SIZEOF_DOUBLE);

    /* non-standard basic types */
#if H5_SIZEOF_LONG_LONG > 0
    vrfy_ctype(long_long, H5_SIZEOF_LONG_LONG);
#endif

#if H5_SIZEOF_LONG_DOUBLE > 0
    vrfy_ctype(long double, H5_SIZEOF_LONG_DOUBLE);
#endif

#if H5_SIZEOF_UINT8_T > 0
    vrfy_ctype(uint8_t, H5_SIZEOF_UINT8_T);
#endif

#if H5_SIZEOF_UINT16_T > 0
    vrfy_ctype(uint16_t, H5_SIZEOF_UINT16_T);
#endif

#if H5_SIZEOF_UINT32_T > 0
    vrfy_ctype(uint32_t, H5_SIZEOF_UINT32_T);
#endif

#if H5_SIZEOF_UINT64_T > 0
    vrfy_ctype(uint64_t, H5_SIZEOF_UINT64_T);
#endif

#if H5_SIZEOF_UINT_FAST8_T > 0
    vrfy_ctype(uint_fast8_t, H5_SIZEOF_UINT_FAST8_T);
#endif

#if H5_SIZEOF_UINT_FAST16_T > 0
    vrfy_ctype(uint_fast16_t, H5_SIZEOF_UINT_FAST16_T);
#endif

#if H5_SIZEOF_UINT_FAST32_T > 0
    vrfy_ctype(uint_fast32_t, H5_SIZEOF_UINT_FAST32_T);
#endif

#if H5_SIZEOF_UINT_FAST64_T > 0
    vrfy_ctype(uint_fast64_t, H5_SIZEOF_UINT_FAST64_T);
#endif

#if H5_SIZEOF_UINT_LEAST8_T > 0
    vrfy_ctype(uint_least8_t, H5_SIZEOF_UINT_LEAST8_T);
#endif

#if H5_SIZEOF_UINT_LEAST16_T > 0
    vrfy_ctype(uint_least16_t, H5_SIZEOF_UINT_LEAST16_T);
#endif

#if H5_SIZEOF_UINT_LEAST32_T > 0
    vrfy_ctype(uint_least32_t, H5_SIZEOF_UINT_LEAST32_T);
#endif

#if H5_SIZEOF_UINT_LEAST64_T > 0
    vrfy_ctype(uint_least64_t, H5_SIZEOF_UINT_LEAST64_T);
#endif

    /* pseudo standard basic types */
#if H5_SIZEOF___INT64 > 0
    vrfy_ctype(__int64, H5_SIZEOF___INT64);
#endif

#if H5_SIZEOF_OFF_T > 0
    vrfy_ctype(off_t, H5_SIZEOF_OFF_T);
#endif

#if H5_SIZEOF_SIZE_T > 0
    vrfy_ctype(size_t, H5_SIZEOF_SIZE_T);
#endif

#if H5_SIZEOF_SSIZE_T > 0
    vrfy_ctype(ssize_t, H5_SIZEOF_SSIZE_T);
#endif

}
