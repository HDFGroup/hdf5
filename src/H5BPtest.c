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
 *              Thursday, April 19, 2005
 *
 * Purpose:	B+ tree testing functions.
 */

#define H5BP_PACKAGE		/*suppress error about including H5BPpkg  */
#define H5BP_TESTING		/*suppress warning about H5BP testing funcs*/

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BPpkg.h"		/* B+ trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MMprivate.h"	/* Memory management			*/

/* Static Prototypes */
static void *H5BP_test_copy(const void *rec);
static herr_t H5BP_test_compare(const void *rec1, const void *rec2);
static herr_t H5BP_test_raw_len(const H5F_t *f, const void *record, size_t *len);
static herr_t H5BP_test_encode(const H5F_t *f, const uint8_t *native,
    uint8_t *raw, size_t *rec_len);
static void *H5BP_test_decode(const H5F_t *f, H5MP_pool_t *pool,
    const uint8_t *raw, size_t rec_len);
static herr_t H5BP_test_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);

/* Package variables */
const H5BP_class_t H5BP_TEST[1]={{   /* B+ tree class information */
    H5BP_TEST_ID,                   /* Type of B+ tree */
    H5BP_test_copy,                 /* Record copy callback */
    H5BP_test_compare,              /* Record comparison callback */
    H5BP_test_raw_len,              /* Record 'raw len' callback */
    H5BP_test_encode,               /* Record encoding callback */
    H5BP_test_decode,               /* Record decoding callback */
    H5BP_test_debug                 /* Record debugging callback */
}};


/*-------------------------------------------------------------------------
 * Function:	H5BP_test_copy
 *
 * Purpose:	Copy native information record
 *
 * Return:	Pointer to new record on success, NULL on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5BP_test_copy(const void *rec)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_test_copy)

    /* Copy record */
    FUNC_LEAVE_NOAPI(HDstrdup(rec));
} /* H5BP_test_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_test_compare
 *
 * Purpose:	Compare two native information records, according to some key
 *
 * Return:	<0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BP_test_compare(const void *rec1, const void *rec2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_test_compare)

    FUNC_LEAVE_NOAPI(HDstrcmp(*(const char **)rec1, *(const char **)rec2));
} /* H5BP_test_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_test_raw_len
 *
 * Purpose:	Determine the length of the raw form of a record
 *
 * Return:	<0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BP_test_raw_len(const H5F_t UNUSED *f, const void *record, size_t *len)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_test_raw_len)

    *len = HDstrlen((const char *)record);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5BP_test_raw_len() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_test_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BP_test_encode(const H5F_t *f, const uint8_t *native, uint8_t *raw, size_t *rec_len)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_test_encode)

    /* Sanity check */
    HDassert(f);
    HDassert(native);
    HDassert(raw);
    HDassert(rec_len);

    /* Get the record length */
    *rec_len = HDstrlen((const char *)native);

    /* "Encode" the record */
    HDmemcpy(raw, native, *rec_len);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5BP_test_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_test_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5BP_test_decode(const H5F_t *f, H5MP_pool_t *pool, const uint8_t *raw, size_t rec_len)
{
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5BP_test_decode)

    /* Sanity check */
    HDassert(f);
    HDassert(pool);
    HDassert(raw);
    HDassert(rec_len > 0);

    /* Allocate space for the record */
    if((ret_value = H5MP_malloc(pool, rec_len + 1)) == NULL)
        HGOTO_ERROR(H5E_BTREE, H5E_NOSPACE, NULL, "Can't allocate space for record")

    /* "Decode" the record */
    HDmemcpy(ret_value, raw, rec_len);
    *((unsigned char *)ret_value + rec_len) = '\0';

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BP_test_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_test_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BP_test_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, int indent, int fwidth,
		      const void *record, const void UNUSED *_udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_test_debug)

    HDassert (record);

    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth, "Record:",
        *(const char *)record);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5BP_test_debug() */

