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
 * Created:		H5HFbtree2.c
 *			Aug  7 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		v2 B-tree callbacks for "huge" object tracker
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5MFprivate.h"	/* File memory management		*/


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

/* v2 B-tree function callbacks */
herr_t H5HF_huge_bt2_found(const void *nrecord, void *op_data);
herr_t H5HF_huge_bt2_remove(const void *nrecord, void *op_data);

/* v2 B-tree driver callbacks */
static herr_t H5HF_huge_btree2_store(const H5B2_class_t *cls, void *native, const void *udata);
static herr_t H5HF_huge_btree2_retrieve(const H5B2_class_t *cls, void *udata, const void *native);
static herr_t H5HF_huge_btree2_compare(const H5B2_class_t *cls, const void *rec1, const void *rec2);
static herr_t H5HF_huge_btree2_encode(const H5F_t *f, const H5B2_class_t *cls, uint8_t *raw,
    const void *native);
static herr_t H5HF_huge_btree2_decode(const H5F_t *f, const H5B2_class_t *cls, const uint8_t *raw,
    void *native);
static herr_t H5HF_huge_btree2_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const H5B2_class_t *cls, const void *record, const void *_udata);

/*********************/
/* Package Variables */
/*********************/
const H5B2_class_t H5HF_BTREE2[1]={{     /* B-tree class information */
    H5B2_FHEAP_ID,              /* Type of B-tree */
    0,                          /* Size of native record */
                                /* (computed at run-time for each heap) */
    NULL,                       /* Class private information */
    H5HF_huge_btree2_store,          /* Record storage callback */
    H5HF_huge_btree2_retrieve,       /* Record retrieval callback */
    H5HF_huge_btree2_compare,        /* Record comparison callback */
    H5HF_huge_btree2_encode,         /* Record encoding callback */
    H5HF_huge_btree2_decode,         /* Record decoding callback */
    H5HF_huge_btree2_debug           /* Record debugging callback */
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_found
 *
 * Purpose:	Retrieve record for 'huge' object, when it's found in the
 *              v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August  8, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_bt2_found(const void *nrecord, void *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_bt2_found)

#ifdef QAK
HDfprintf(stderr, "%s: nrecord = {%a, %Hu, %Hu}\n", "H5HF_huge_bt2_store",
        ((const H5HF_huge_bt2_rec_t *)nrecord)->addr,
        ((const H5HF_huge_bt2_rec_t *)nrecord)->len,
        ((const H5HF_huge_bt2_rec_t *)nrecord)->id);
#endif /* QAK */
    *(H5HF_huge_bt2_rec_t *)op_data = *(const H5HF_huge_bt2_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_bt2_found() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_remove
 *
 * Purpose:	Free space for 'huge' object, as v2 B-tree is being deleted
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August  8, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_bt2_remove(const void *nrecord, void *_udata)
{
    H5HF_huge_remove_ud1_t *udata = (H5HF_huge_remove_ud1_t *)_udata;   /* User callback data */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_bt2_remove)

    /* Free the space in the file for the object being removed */
    if(H5MF_xfree(udata->hdr->f, H5FD_MEM_FHEAP_HUGE_OBJ, udata->dxpl_id, ((const H5HF_huge_bt2_rec_t *)nrecord)->addr, ((const H5HF_huge_bt2_rec_t *)nrecord)->len) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free space for huge object on disk")

    /* Set the length of the object removed */
    udata->obj_len = ((const H5HF_huge_bt2_rec_t *)nrecord)->len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_huge_bt2_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_store
 *
 * Purpose:	Store native information into record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_store(const H5B2_class_t UNUSED *cls, void *nrecord, const void *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_store)

    *(H5HF_huge_bt2_rec_t *)nrecord = *(const H5HF_huge_bt2_rec_t *)udata;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_store() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_retrieve
 *
 * Purpose:	Retrieve native information from record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_retrieve(const H5B2_class_t UNUSED *cls, void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_retrieve)

    *(H5HF_huge_bt2_rec_t *)udata = *(const H5HF_huge_bt2_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_compare
 *
 * Purpose:	Compare two native information records, according to some key
 *
 * Return:	<0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_compare(const H5B2_class_t *cls, const void *_rec1, const void *_rec2)
{
    const H5HF_huge_bt2_rec_t *rec1 = (const H5HF_huge_bt2_rec_t *)_rec1;
    const H5HF_huge_bt2_rec_t *rec2 = (const H5HF_huge_bt2_rec_t *)_rec2;
    const H5HF_hdr_t *hdr = (const H5HF_hdr_t *)cls->cls_private;
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_compare)

#ifdef QAK
HDfprintf(stderr, "%s: hdr->huge_ids_direct = %t\n", "H5HF_huge_btree2_compare", hdr->huge_ids_direct);
HDfprintf(stderr, "%s: rec1 = {%a, %Hu, %Hu}\n", "H5HF_huge_btree2_compare", rec1->addr, rec1->len, rec1->id);
HDfprintf(stderr, "%s: rec2 = {%a, %Hu, %Hu}\n", "H5HF_huge_btree2_compare", rec2->addr, rec2->len, rec2->id);
#endif /* QAK */
    /* Sort differently, depending on whether 'huge' object directly reference disk */
    if(hdr->huge_ids_direct) {
        if(rec1->addr < rec2->addr)
            ret_value = -1;
        else if(rec1->addr > rec2->addr)
            ret_value = 1;
        else if(rec1->len < rec2->len)
            ret_value = -1;
        else if(rec1->len > rec2->len)
            ret_value = 1;
        else
            ret_value = 0;
    } /* end if */
    else
        ret_value = (herr_t)(rec1->id - rec2->id);

    FUNC_LEAVE_NOAPI(ret_value);
} /* H5HF_huge_btree2_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_encode(const H5F_t *f, const H5B2_class_t *cls, uint8_t *raw, const void *_nrecord)
{
    const H5HF_huge_bt2_rec_t *nrecord = (const H5HF_huge_bt2_rec_t *)_nrecord;
    const H5HF_hdr_t *hdr = (const H5HF_hdr_t *)cls->cls_private;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_encode)

    /* Encode the record's common fields */
    H5F_addr_encode(f, &raw, nrecord->addr);
    H5F_ENCODE_LENGTH(f, raw, nrecord->len);

    /* If 'huge' objects in this heap are not accessed directly, encode the ID also */
    if(!hdr->huge_ids_direct)
        UINT64ENCODE_VAR(raw, nrecord->id, hdr->huge_id_size)

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_decode(const H5F_t *f, const H5B2_class_t *cls, const uint8_t *raw, void *_nrecord)
{
    H5HF_huge_bt2_rec_t *nrecord = (H5HF_huge_bt2_rec_t *)_nrecord;
    const H5HF_hdr_t *hdr = (const H5HF_hdr_t *)cls->cls_private;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_decode)

    /* Decode the record's common fields */
    H5F_addr_decode(f, &raw, &nrecord->addr);
    H5F_DECODE_LENGTH(f, raw, nrecord->len);

    /* If 'huge' objects in this heap are not accessed directly, decode the ID also */
    if(!hdr->huge_ids_direct)
        UINT64DECODE_VAR(raw, nrecord->id, hdr->huge_id_size)
    else
        nrecord->id = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const H5B2_class_t *cls, const void *_nrecord,
    const void UNUSED *_udata)
{
    const H5HF_huge_bt2_rec_t *nrecord = (const H5HF_huge_bt2_rec_t *)_nrecord;
    const H5HF_hdr_t *hdr = (const H5HF_hdr_t *)cls->cls_private;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_debug)

    HDassert(nrecord);

    if(hdr->huge_ids_direct)
        HDfprintf(stream, "%*s%-*s {%a, %Hu}\n", indent, "", fwidth, "Record:",
            nrecord->addr, nrecord->len);
    else
        HDfprintf(stream, "%*s%-*s {%a, %Hu, %Hu}\n", indent, "", fwidth, "Record:",
            nrecord->addr, nrecord->len, nrecord->id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_debug() */

