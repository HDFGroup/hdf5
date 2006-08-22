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

/*-------------------------------------------------------------------------
 *
 * Created:		H5checksum.c
 *			Aug 21 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Internal code for computing fletcher32 checksums
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5_fletcher32
 *
 * Purpose:	This routine provides a generic, fast checksum algorithm for
 *              use in the library.
 *
 * Note:        See the Wikipedia page for Fletcher's checksum:
 *                  http://en.wikipedia.org/wiki/Fletcher%27s_checksum
 *              for more details, etc.
 *
 * Note #2:     The algorithm below differs from that given in the Wikipedia
 *              page by copying the data into 'sum1' in a more portable way
 *              and also by initializing 'sum1' and 'sum2' to 0 instead of
 *              0xffff (for backward compatibility reasons, mostly).
 *
 * Return:	32-bit fletcher checksum of input buffer (can't fail)
 *
 * Programmer:	Quincey Koziol
 *              Monday, August 21, 2006
 *
 *-------------------------------------------------------------------------
 */
uint32_t
H5_fletcher32(const void *_data, size_t _len)
{
    const uint8_t *data = (const uint8_t *)_data;  /* Pointer to the data to be summed */
    size_t len = _len / 2;      /* Length in 16-bit words */
    uint32_t sum1 = 0, sum2 = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5_fletcher32)

    /* Sanity check */
    HDassert(_data);
    HDassert(_len > 0);

    /* Compute checksum for pairs of bytes */
    /* (the magic "360" value is is the largest number of sums that can be
     *  performed without numeric overflow)
     */
    while (len) {
        unsigned tlen = len > 360 ? 360 : len;
        len -= tlen;
        do {
            sum1 += (((uint16_t)data[0]) << 8) | ((uint16_t)data[1]);
            data += 2;
            sum2 += sum1;
        } while (--tlen);
        sum1 = (sum1 & 0xffff) + (sum1 >> 16);
        sum2 = (sum2 & 0xffff) + (sum2 >> 16);
    }

    /* Check for odd # of bytes */
    if(_len % 2) {
        sum1 += ((uint16_t)*data) << 8;
        sum2 += sum1;
        sum1 = (sum1 & 0xffff) + (sum1 >> 16);
        sum2 = (sum2 & 0xffff) + (sum2 >> 16);
    } /* end if */

    /* Second reduction step to reduce sums to 16 bits */
    sum1 = (sum1 & 0xffff) + (sum1 >> 16);
    sum2 = (sum2 & 0xffff) + (sum2 >> 16);

    FUNC_LEAVE_NOAPI((sum2 << 16) | sum1)
} /* end H5_fletcher32() */


/*-------------------------------------------------------------------------
 * Function:	H5_checksum_metadata
 *
 * Purpose:	Provide a more abstract routine for checksumming metadata
 *              in a file, where the policy of which algorithm to choose
 *              is centralized.
 *
 * Return:	checksum of input buffer (can't fail)
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 22, 2006
 *
 *-------------------------------------------------------------------------
 */
uint32_t
H5_checksum_metadata(const void *data, size_t len)
{
    uint32_t chksum;            /* Checksum value to return */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5_checksum_metadata)

    /* Sanity check */
    HDassert(data);
    HDassert(len > 0);

    /* Choose the appropriate checksum routine */
    /* (use fletcher32 for everything right now, but will probably go
     *  with a CRC algorithm for "shorter" pieces of metadata eventually)
     */
    chksum = H5_fletcher32(data, len);

    FUNC_LEAVE_NOAPI(chksum)
} /* end H5_checksum_metadata() */

