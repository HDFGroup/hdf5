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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Thursday, March 10, 2005
 *
 * Purpose:	v2 B-tree callback routines for block info records
 */

#define H5BT_PACKAGE		/*suppress error about including H5BTpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BTpkg.h"		/* Block tracker			*/
#include "H5Eprivate.h"		/* Error handling		  	*/

/* Static Prototypes */
static herr_t H5BT_store(void *nrecord, const void *udata);
static herr_t H5BT_retrieve(void *udata, const void *nrecord);
static herr_t H5BT_compare(const void *rec1, const void *rec2);
static herr_t H5BT_encode(const H5F_t *f, uint8_t *raw,
    const void *nrecord);
static herr_t H5BT_decode(const H5F_t *f, const uint8_t *raw,
    void *nrecord);
static herr_t H5BT_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);

/* Package variables */
const H5B2_class_t H5B2_BLKTRK[1]={{   /* B-tree class information */
    H5B2_BLK_TRK_ID,            /* Type of B-tree */
    sizeof(H5BT_blk_info_t),    /* Size of native key */
    H5BT_store,                 /* Record storage callback */
    H5BT_retrieve,              /* Record retrieval callback */
    H5BT_compare,               /* Record comparison callback */
    H5BT_encode,                /* Record encoding callback */
    H5BT_decode,                /* Record decoding callback */
    H5BT_debug                  /* Record debugging callback */
}};


/*-------------------------------------------------------------------------
 * Function:	H5BT_store
 *
 * Purpose:	Store native information into record for B-tree
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_store(void *nrecord, const void *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_store)

    *(H5BT_blk_info_t *)nrecord = *(const H5BT_blk_info_t *)udata;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5BT_store() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_retrieve
 *
 * Purpose:	Retrieve native information from record for B-tree
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_retrieve(void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_retrieve)

    *(H5BT_blk_info_t *)udata = *(const H5BT_blk_info_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5BT_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_compare
 *
 * Purpose:	Compare two native information records, according to some key
 *
 * Return:	<0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_compare(const void *rec1, const void *rec2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_compare)

    FUNC_LEAVE_NOAPI(H5F_addr_cmp(((const H5BT_blk_info_t *)rec1)->addr,((const H5BT_blk_info_t *)rec2)->addr));
} /* H5BT_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_encode(const H5F_t *f, uint8_t *raw, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_encode)

    H5F_addr_encode(f, &raw, ((const H5BT_blk_info_t *)nrecord)->addr);
    H5F_ENCODE_LENGTH(f, raw, ((const H5BT_blk_info_t *)nrecord)->len);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5BT_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_decode(const H5F_t *f, const uint8_t *raw, void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_decode)

    H5F_addr_decode(f, &raw, &(((H5BT_blk_info_t *)nrecord)->addr));
    H5F_DECODE_LENGTH(f, raw, ((H5BT_blk_info_t *)nrecord)->len);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5BT_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, int indent, int fwidth,
		      const void *record, const void UNUSED *_udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_debug)

    HDassert (record);

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth, "Block Address:",
        ((const H5BT_blk_info_t *)record)->addr);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth, "Block Length:",
        ((const H5BT_blk_info_t *)record)->len);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5BT_debug() */

