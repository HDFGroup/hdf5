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

/* Polynomial quotient */
/* (same as the IEEE 802.3 (Ethernet) quotient) */
#define H5_CRC_QUOTIENT 0x04C11DB7


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

/* Table of CRCs of all 8-bit messages. */
static uint32_t H5_crc_table[256];

/* Flag: has the table been computed? */
static hbool_t H5_crc_table_computed = FALSE;



/*-------------------------------------------------------------------------
 * Function:	H5_checksum_fletcher32
 *
 * Purpose:	This routine provides a generic, fast checksum algorithm for
 *              use in the library.
 *
 * Note:        See the Wikipedia page for Fletcher's checksum:
 *                  http://en.wikipedia.org/wiki/Fletcher%27s_checksum
 *              for more details, etc.
 *
 * Note #2:     Per the information in RFC 3309:
 *                      (http://tools.ietf.org/html/rfc3309)
 *              Fletcher's checksum is not reliable for small buffers.
 *
 * Note #3:     The algorithm below differs from that given in the Wikipedia
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
H5_checksum_fletcher32(const void *_data, size_t _len)
{
    const uint8_t *data = (const uint8_t *)_data;  /* Pointer to the data to be summed */
    size_t len = _len / 2;      /* Length in 16-bit words */
    uint32_t sum1 = 0, sum2 = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5_checksum_fletcher32)

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
} /* end H5_checksum_fletcher32() */


/*-------------------------------------------------------------------------
 * Function:	H5_checksum_crc_make_table
 *
 * Purpose:	Compute the CRC table for the CRC checksum algorithm
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static void
H5_checksum_crc_make_table(void)
{
    uint32_t c;         /* Checksum for each byte value */
    unsigned n, k;      /* Local index variables */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5_checksum_crc_make_table)

    /* Compute the checksum for each possible byte value */
    for(n = 0; n < 256; n++) {
        c = (uint32_t) n;
        for(k = 0; k < 8; k++)
            if(c & 1)
                c = H5_CRC_QUOTIENT ^ (c >> 1);
            else
                c = c >> 1;
        H5_crc_table[n] = c;
    }
    H5_crc_table_computed = TRUE;

    FUNC_LEAVE_NOAPI_VOID
} /* end H5_checksum_crc_make_table() */


/*-------------------------------------------------------------------------
 * Function:	H5_checksum_crc_make_table
 *
 * Purpose:	Update a running CRC with the bytes buf[0..len-1]--the CRC
 *              should be initialized to all 1's, and the transmitted value
 *              is the 1's complement of the final running CRC (see the
 *              H5_checksum_crc() routine below)).
 *
 * Return:	32-bit CRC checksum of input buffer (can't fail)
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static uint32_t
H5_checksum_crc_update(uint32_t crc, const uint8_t *buf, size_t len)
{
    size_t n;           /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5_checksum_crc_update)

    /* Initialize the CRC table if necessary */
    if(!H5_crc_table_computed)
        H5_checksum_crc_make_table();

    /* Update the CRC with the results from this buffer */
    for(n = 0; n < len; n++)
        crc = H5_crc_table[(crc ^ buf[n]) & 0xff] ^ (crc >> 8);

    FUNC_LEAVE_NOAPI(crc)
} /* end H5_checksum_crc_update() */


/*-------------------------------------------------------------------------
 * Function:	H5_checksum_crc
 *
 * Purpose:	This routine provides a generic checksum algorithm for
 *              use in the library.
 *
 * Note:        This algorithm was based on the implementation described
 *              in the document describing the PNG image format:
 *                  http://www.w3.org/TR/PNG/#D-CRCAppendix
 *
 * Return:	32-bit CRC checksum of input buffer (can't fail)
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September  5, 2006
 *
 *-------------------------------------------------------------------------
 */
uint32_t
H5_checksum_crc(const void *_data, size_t len)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5_checksum_crc)

    /* Sanity check */
    HDassert(_data);
    HDassert(len > 0);

    FUNC_LEAVE_NOAPI(H5_checksum_crc_update((uint32_t)0xffffffffL, (const uint8_t *)_data, len) ^ 0xffffffffL)
} /* end H5_checksum_crc() */


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
    /* (use Fletcher's checksum for "larger" buffers and CRC for "shorter" ones) */
    if(len < 256)
        chksum = H5_checksum_crc(data, len);
    else
        chksum = H5_checksum_fletcher32(data, len);

    FUNC_LEAVE_NOAPI(chksum)
} /* end H5_checksum_metadata() */

