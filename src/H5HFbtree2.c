/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
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

/* v2 B-tree driver callbacks */
static herr_t H5HF_huge_btree2_indir_store(void *native, const void *udata);
static herr_t H5HF_huge_btree2_indir_retrieve(void *udata, const void *native);
static herr_t H5HF_huge_btree2_indir_compare(const void *rec1, const void *rec2);
static herr_t H5HF_huge_btree2_indir_encode(const H5F_t *f, uint8_t *raw,
    const void *native);
static herr_t H5HF_huge_btree2_indir_decode(const H5F_t *f, const uint8_t *raw,
    void *native);
static herr_t H5HF_huge_btree2_indir_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);

static herr_t H5HF_huge_btree2_filt_indir_store(void *native, const void *udata);
static herr_t H5HF_huge_btree2_filt_indir_retrieve(void *udata, const void *native);
static herr_t H5HF_huge_btree2_filt_indir_compare(const void *rec1, const void *rec2);
static herr_t H5HF_huge_btree2_filt_indir_encode(const H5F_t *f, uint8_t *raw,
    const void *native);
static herr_t H5HF_huge_btree2_filt_indir_decode(const H5F_t *f, const uint8_t *raw,
    void *native);
static herr_t H5HF_huge_btree2_filt_indir_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);

static herr_t H5HF_huge_btree2_dir_store(void *native, const void *udata);
static herr_t H5HF_huge_btree2_dir_retrieve(void *udata, const void *native);
static herr_t H5HF_huge_btree2_dir_compare(const void *rec1, const void *rec2);
static herr_t H5HF_huge_btree2_dir_encode(const H5F_t *f, uint8_t *raw,
    const void *native);
static herr_t H5HF_huge_btree2_dir_decode(const H5F_t *f, const uint8_t *raw,
    void *native);
static herr_t H5HF_huge_btree2_dir_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);

static herr_t H5HF_huge_btree2_filt_dir_store(void *native, const void *udata);
static herr_t H5HF_huge_btree2_filt_dir_retrieve(void *udata, const void *native);
static herr_t H5HF_huge_btree2_filt_dir_compare(const void *rec1, const void *rec2);
static herr_t H5HF_huge_btree2_filt_dir_encode(const H5F_t *f, uint8_t *raw,
    const void *native);
static herr_t H5HF_huge_btree2_filt_dir_decode(const H5F_t *f, const uint8_t *raw,
    void *native);
static herr_t H5HF_huge_btree2_filt_dir_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);

/*********************/
/* Package Variables */
/*********************/
/* v2 B-tree class for indirectly accessed 'huge' objects */
const H5B2_class_t H5HF_BT2_INDIR[1]={{     /* B-tree class information */
    H5B2_FHEAP_HUGE_INDIR_ID,           /* Type of B-tree */
    sizeof(H5HF_huge_bt2_indir_rec_t),  /* Size of native record */
    H5HF_huge_btree2_indir_store,       /* Record storage callback */
    H5HF_huge_btree2_indir_retrieve,    /* Record retrieval callback */
    H5HF_huge_btree2_indir_compare,     /* Record comparison callback */
    H5HF_huge_btree2_indir_encode,      /* Record encoding callback */
    H5HF_huge_btree2_indir_decode,      /* Record decoding callback */
    H5HF_huge_btree2_indir_debug        /* Record debugging callback */
}};

/* v2 B-tree class for indirectly accessed, filtered 'huge' objects */
const H5B2_class_t H5HF_BT2_FILT_INDIR[1]={{     /* B-tree class information */
    H5B2_FHEAP_HUGE_FILT_INDIR_ID,              /* Type of B-tree */
    sizeof(H5HF_huge_bt2_filt_indir_rec_t),     /* Size of native record */
    H5HF_huge_btree2_filt_indir_store,          /* Record storage callback */
    H5HF_huge_btree2_filt_indir_retrieve,       /* Record retrieval callback */
    H5HF_huge_btree2_filt_indir_compare,        /* Record comparison callback */
    H5HF_huge_btree2_filt_indir_encode,         /* Record encoding callback */
    H5HF_huge_btree2_filt_indir_decode,         /* Record decoding callback */
    H5HF_huge_btree2_filt_indir_debug           /* Record debugging callback */
}};

/* v2 B-tree class for directly accessed 'huge' objects */
const H5B2_class_t H5HF_BT2_DIR[1]={{     /* B-tree class information */
    H5B2_FHEAP_HUGE_DIR_ID,             /* Type of B-tree */
    sizeof(H5HF_huge_bt2_dir_rec_t),    /* Size of native record */
    H5HF_huge_btree2_dir_store,         /* Record storage callback */
    H5HF_huge_btree2_dir_retrieve,      /* Record retrieval callback */
    H5HF_huge_btree2_dir_compare,       /* Record comparison callback */
    H5HF_huge_btree2_dir_encode,        /* Record encoding callback */
    H5HF_huge_btree2_dir_decode,        /* Record decoding callback */
    H5HF_huge_btree2_dir_debug          /* Record debugging callback */
}};

/* v2 B-tree class for directly accessed, filtered 'huge' objects */
const H5B2_class_t H5HF_BT2_FILT_DIR[1]={{     /* B-tree class information */
    H5B2_FHEAP_HUGE_FILT_DIR_ID,        /* Type of B-tree */
    sizeof(H5HF_huge_bt2_filt_dir_rec_t),/* Size of native record */
    H5HF_huge_btree2_filt_dir_store,    /* Record storage callback */
    H5HF_huge_btree2_filt_dir_retrieve, /* Record retrieval callback */
    H5HF_huge_btree2_filt_dir_compare,  /* Record comparison callback */
    H5HF_huge_btree2_filt_dir_encode,   /* Record encoding callback */
    H5HF_huge_btree2_filt_dir_decode,   /* Record decoding callback */
    H5HF_huge_btree2_filt_dir_debug     /* Record debugging callback */
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_indir_found
 *
 * Purpose:	Retrieve record for indirectly accessed 'huge' object, when
 *              it's found in the v2 B-tree
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
H5HF_huge_bt2_indir_found(const void *nrecord, void *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_bt2_indir_found)

#ifdef QAK
HDfprintf(stderr, "%s: nrecord = {%a, %Hu, %Hu}\n", "H5HF_huge_bt2_indir_found",
        ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->addr,
        ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->len,
        ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->id);
#endif /* QAK */
    *(H5HF_huge_bt2_indir_rec_t *)op_data = *(const H5HF_huge_bt2_indir_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_bt2_indir_found() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_indir_remove
 *
 * Purpose:	Free space for indirectly accessed 'huge' object, as v2 B-tree
 *              is being deleted or v2 B-tree node is removed
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
H5HF_huge_bt2_indir_remove(const void *nrecord, void *_udata)
{
    H5HF_huge_remove_ud1_t *udata = (H5HF_huge_remove_ud1_t *)_udata;   /* User callback data */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_bt2_indir_remove)

    /* Free the space in the file for the object being removed */
    if(H5MF_xfree(udata->hdr->f, H5FD_MEM_FHEAP_HUGE_OBJ, udata->dxpl_id, ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->addr, ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->len) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free space for huge object on disk")

    /* Set the length of the object removed */
    udata->obj_len = ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_huge_bt2_indir_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_indir_store
 *
 * Purpose:	Store native information into record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_indir_store(void *nrecord, const void *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_indir_store)

    *(H5HF_huge_bt2_indir_rec_t *)nrecord = *(const H5HF_huge_bt2_indir_rec_t *)udata;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_indir_store() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_indir_retrieve
 *
 * Purpose:	Retrieve native information from record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_indir_retrieve(void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_indir_retrieve)

    *(H5HF_huge_bt2_indir_rec_t *)udata = *(const H5HF_huge_bt2_indir_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_indir_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_indir_compare
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
H5HF_huge_btree2_indir_compare(const void *_rec1, const void *_rec2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_indir_compare)

#ifdef QAK
{
const H5HF_huge_bt2_indir_rec_t *rec1 = (const H5HF_huge_bt2_indir_rec_t *)_rec1;
const H5HF_huge_bt2_indir_rec_t *rec2 = (const H5HF_huge_bt2_indir_rec_t *)_rec2;

HDfprintf(stderr, "%s: rec1 = {%a, %Hu, %Hu}\n", "H5HF_huge_btree2_indir_compare", rec1->addr, rec1->len, rec1->id);
HDfprintf(stderr, "%s: rec2 = {%a, %Hu, %Hu}\n", "H5HF_huge_btree2_indir_compare", rec2->addr, rec2->len, rec2->id);
}
#endif /* QAK */
    FUNC_LEAVE_NOAPI((herr_t)(((const H5HF_huge_bt2_indir_rec_t *)_rec1)->id - ((const H5HF_huge_bt2_indir_rec_t *)_rec2)->id))
} /* H5HF_huge_btree2_indir_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_indir_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_indir_encode(const H5F_t *f, uint8_t *raw, const void *_nrecord)
{
    const H5HF_huge_bt2_indir_rec_t *nrecord = (const H5HF_huge_bt2_indir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_indir_encode)

    /* Encode the record's fields */
    H5F_addr_encode(f, &raw, nrecord->addr);
    H5F_ENCODE_LENGTH(f, raw, nrecord->len);
    H5F_ENCODE_LENGTH(f, raw, nrecord->id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_indir_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_indir_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_indir_decode(const H5F_t *f, const uint8_t *raw, void *_nrecord)
{
    H5HF_huge_bt2_indir_rec_t *nrecord = (H5HF_huge_bt2_indir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_indir_decode)

    /* Decode the record's fields */
    H5F_addr_decode(f, &raw, &nrecord->addr);
    H5F_DECODE_LENGTH(f, raw, nrecord->len);
    H5F_DECODE_LENGTH(f, raw, nrecord->id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_indir_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_indir_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_indir_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *_nrecord,
    const void UNUSED *_udata)
{
    const H5HF_huge_bt2_indir_rec_t *nrecord = (const H5HF_huge_bt2_indir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_indir_debug)

    HDfprintf(stream, "%*s%-*s {%a, %Hu, %Hu}\n", indent, "", fwidth, "Record:",
        nrecord->addr, nrecord->len, nrecord->id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_indir_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_filt_indir_found
 *
 * Purpose:	Retrieve record for indirectly accessed, filtered 'huge' object,
 *              when it's found in the v2 B-tree
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
H5HF_huge_bt2_filt_indir_found(const void *nrecord, void *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_bt2_filt_indir_found)

#ifdef QAK
HDfprintf(stderr, "%s: nrecord = {%a, %Hu, %x, %Hu, %Hu}\n", "H5HF_huge_bt2_filt_indir_found",
        ((const H5HF_huge_bt2_filt_indir_rec_t *)nrecord)->addr,
        ((const H5HF_huge_bt2_filt_indir_rec_t *)nrecord)->len,
        ((const H5HF_huge_bt2_filt_indir_rec_t *)nrecord)->filter_mask,
        ((const H5HF_huge_bt2_filt_indir_rec_t *)nrecord)->obj_size,
        ((const H5HF_huge_bt2_filt_indir_rec_t *)nrecord)->id);
#endif /* QAK */
    *(H5HF_huge_bt2_filt_indir_rec_t *)op_data = *(const H5HF_huge_bt2_filt_indir_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_bt2_filt_indir_found() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_filt_indir_remove
 *
 * Purpose:	Free space for indirectly accessed, filtered 'huge' object, as
 *              v2 B-tree is being deleted or v2 B-tree node is removed
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
H5HF_huge_bt2_filt_indir_remove(const void *nrecord, void *_udata)
{
    H5HF_huge_remove_ud1_t *udata = (H5HF_huge_remove_ud1_t *)_udata;   /* User callback data */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_bt2_filt_indir_remove)

    /* Free the space in the file for the object being removed */
    if(H5MF_xfree(udata->hdr->f, H5FD_MEM_FHEAP_HUGE_OBJ, udata->dxpl_id, ((const H5HF_huge_bt2_filt_indir_rec_t *)nrecord)->addr, ((const H5HF_huge_bt2_filt_indir_rec_t *)nrecord)->len) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free space for huge object on disk")

    /* Set the length of the object removed */
    udata->obj_len = ((const H5HF_huge_bt2_filt_indir_rec_t *)nrecord)->obj_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_huge_bt2_filt_indir_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_indir_store
 *
 * Purpose:	Store native information into record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_indir_store(void *nrecord, const void *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_indir_store)

    *(H5HF_huge_bt2_filt_indir_rec_t *)nrecord = *(const H5HF_huge_bt2_filt_indir_rec_t *)udata;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_indir_store() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_indir_retrieve
 *
 * Purpose:	Retrieve native information from record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_indir_retrieve(void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_indir_retrieve)

    *(H5HF_huge_bt2_filt_indir_rec_t *)udata = *(const H5HF_huge_bt2_filt_indir_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_indir_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_indir_compare
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
H5HF_huge_btree2_filt_indir_compare(const void *_rec1, const void *_rec2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_indir_compare)

#ifdef QAK
{
const H5HF_huge_bt2_filt_indir_rec_t *rec1 = (const H5HF_huge_bt2_filt_indir_rec_t *)_rec1;
const H5HF_huge_bt2_filt_indir_rec_t *rec2 = (const H5HF_huge_bt2_filt_indir_rec_t *)_rec2;

HDfprintf(stderr, "%s: rec1 = {%a, %Hu, %x, %Hu, %Hu}\n", "H5HF_huge_btree2_filt_indir_compare", rec1->addr, rec1->len, rec1->filter_mask, rec1->obj_size, rec1->id);
HDfprintf(stderr, "%s: rec2 = {%a, %Hu, %x, %Hu, %Hu}\n", "H5HF_huge_btree2_filt_indir_compare", rec2->addr, rec2->len, rec2->filter_mask, rec2->obj_size, rec2->id);
}
#endif /* QAK */
    FUNC_LEAVE_NOAPI((herr_t)(((const H5HF_huge_bt2_filt_indir_rec_t *)_rec1)->id - ((const H5HF_huge_bt2_filt_indir_rec_t *)_rec2)->id))
} /* H5HF_huge_btree2_filt_indir_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_indir_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_indir_encode(const H5F_t *f, uint8_t *raw, const void *_nrecord)
{
    const H5HF_huge_bt2_filt_indir_rec_t *nrecord = (const H5HF_huge_bt2_filt_indir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_indir_encode)

    /* Encode the record's fields */
    H5F_addr_encode(f, &raw, nrecord->addr);
    H5F_ENCODE_LENGTH(f, raw, nrecord->len);
    UINT32ENCODE(raw, nrecord->filter_mask);
    H5F_ENCODE_LENGTH(f, raw, nrecord->obj_size);
    H5F_ENCODE_LENGTH(f, raw, nrecord->id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_indir_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_indir_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_indir_decode(const H5F_t *f, const uint8_t *raw, void *_nrecord)
{
    H5HF_huge_bt2_filt_indir_rec_t *nrecord = (H5HF_huge_bt2_filt_indir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_indir_decode)

    /* Decode the record's fields */
    H5F_addr_decode(f, &raw, &nrecord->addr);
    H5F_DECODE_LENGTH(f, raw, nrecord->len);
    UINT32DECODE(raw, nrecord->filter_mask);
    H5F_DECODE_LENGTH(f, raw, nrecord->obj_size);
    H5F_DECODE_LENGTH(f, raw, nrecord->id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_indir_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_indir_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_indir_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *_nrecord,
    const void UNUSED *_udata)
{
    const H5HF_huge_bt2_filt_indir_rec_t *nrecord = (const H5HF_huge_bt2_filt_indir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_indir_debug)

    HDfprintf(stream, "%*s%-*s {%a, %Hu, %x, %Hu, %Hu}\n", indent, "", fwidth, "Record:",
        nrecord->addr, nrecord->len, nrecord->filter_mask, nrecord->obj_size, nrecord->id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_indir_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_dir_remove
 *
 * Purpose:	Free space for directly accessed 'huge' object, as v2 B-tree
 *              is being deleted or v2 B-tree node is being removed
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
H5HF_huge_bt2_dir_remove(const void *nrecord, void *_udata)
{
    H5HF_huge_remove_ud1_t *udata = (H5HF_huge_remove_ud1_t *)_udata;   /* User callback data */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_bt2_dir_remove)

    /* Free the space in the file for the object being removed */
    if(H5MF_xfree(udata->hdr->f, H5FD_MEM_FHEAP_HUGE_OBJ, udata->dxpl_id, ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->addr, ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->len) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free space for huge object on disk")

    /* Set the length of the object removed */
    udata->obj_len = ((const H5HF_huge_bt2_indir_rec_t *)nrecord)->len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_huge_bt2_dir_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_dir_store
 *
 * Purpose:	Store native information into record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_dir_store(void *nrecord, const void *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_dir_store)

    *(H5HF_huge_bt2_dir_rec_t *)nrecord = *(const H5HF_huge_bt2_dir_rec_t *)udata;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_dir_store() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_dir_retrieve
 *
 * Purpose:	Retrieve native information from record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_dir_retrieve(void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_dir_retrieve)

    *(H5HF_huge_bt2_dir_rec_t *)udata = *(const H5HF_huge_bt2_dir_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_dir_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_dir_compare
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
H5HF_huge_btree2_dir_compare(const void *_rec1, const void *_rec2)
{
    const H5HF_huge_bt2_dir_rec_t *rec1 = (const H5HF_huge_bt2_dir_rec_t *)_rec1;
    const H5HF_huge_bt2_dir_rec_t *rec2 = (const H5HF_huge_bt2_dir_rec_t *)_rec2;
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_dir_compare)

#ifdef QAK
HDfprintf(stderr, "%s: rec1 = {%a, %Hu}\n", "H5HF_huge_btree2_dir_compare", rec1->addr, rec1->len);
HDfprintf(stderr, "%s: rec2 = {%a, %Hu}\n", "H5HF_huge_btree2_dir_compare", rec2->addr, rec2->len);
#endif /* QAK */
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

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_huge_btree2_dir_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_dir_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_dir_encode(const H5F_t *f, uint8_t *raw, const void *_nrecord)
{
    const H5HF_huge_bt2_dir_rec_t *nrecord = (const H5HF_huge_bt2_dir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_dir_encode)

    /* Encode the record's fields */
    H5F_addr_encode(f, &raw, nrecord->addr);
    H5F_ENCODE_LENGTH(f, raw, nrecord->len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_dir_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_dir_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_dir_decode(const H5F_t *f, const uint8_t *raw, void *_nrecord)
{
    H5HF_huge_bt2_dir_rec_t *nrecord = (H5HF_huge_bt2_dir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_dir_decode)

    /* Decode the record's fields */
    H5F_addr_decode(f, &raw, &nrecord->addr);
    H5F_DECODE_LENGTH(f, raw, nrecord->len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_dir_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_dir_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_dir_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *_nrecord,
    const void UNUSED *_udata)
{
    const H5HF_huge_bt2_dir_rec_t *nrecord = (const H5HF_huge_bt2_dir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_dir_debug)

    HDfprintf(stream, "%*s%-*s {%a, %Hu}\n", indent, "", fwidth, "Record:",
        nrecord->addr, nrecord->len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_dir_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_filt_dir_found
 *
 * Purpose:	Retrieve record for directly accessed, filtered 'huge' object,
 *              when it's found in the v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_bt2_filt_dir_found(const void *nrecord, void *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_bt2_filt_dir_found)

#ifdef QAK
HDfprintf(stderr, "%s: nrecord = {%a, %Hu, %x, %Hu}\n", "H5HF_huge_bt2_filt_dir_found",
        ((const H5HF_huge_bt2_filt_dir_rec_t *)nrecord)->addr,
        ((const H5HF_huge_bt2_filt_dir_rec_t *)nrecord)->len,
        ((const H5HF_huge_bt2_filt_dir_rec_t *)nrecord)->filter_mask,
        ((const H5HF_huge_bt2_filt_dir_rec_t *)nrecord)->obj_size);
#endif /* QAK */
    *(H5HF_huge_bt2_filt_dir_rec_t *)op_data = *(const H5HF_huge_bt2_filt_dir_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_bt2_filt_dir_found() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_filt_dir_remove
 *
 * Purpose:	Free space for directly accessed, filtered 'huge' object, as
 *              v2 B-tree is being deleted or v2 B-tree node is removed
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_bt2_filt_dir_remove(const void *nrecord, void *_udata)
{
    H5HF_huge_remove_ud1_t *udata = (H5HF_huge_remove_ud1_t *)_udata;   /* User callback data */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_bt2_filt_dir_remove)

    /* Free the space in the file for the object being removed */
    if(H5MF_xfree(udata->hdr->f, H5FD_MEM_FHEAP_HUGE_OBJ, udata->dxpl_id, ((const H5HF_huge_bt2_filt_dir_rec_t *)nrecord)->addr, ((const H5HF_huge_bt2_filt_dir_rec_t *)nrecord)->len) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free space for huge object on disk")

    /* Set the length of the object removed */
    udata->obj_len = ((const H5HF_huge_bt2_filt_dir_rec_t *)nrecord)->obj_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_huge_bt2_filt_dir_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_dir_store
 *
 * Purpose:	Store native information into record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_dir_store(void *nrecord, const void *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_dir_store)

    *(H5HF_huge_bt2_filt_dir_rec_t *)nrecord = *(const H5HF_huge_bt2_filt_dir_rec_t *)udata;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_dir_store() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_dir_retrieve
 *
 * Purpose:	Retrieve native information from record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_dir_retrieve(void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_dir_retrieve)

    *(H5HF_huge_bt2_filt_dir_rec_t *)udata = *(const H5HF_huge_bt2_filt_dir_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_dir_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_dir_compare
 *
 * Purpose:	Compare two native information records, according to some key
 *
 * Return:	<0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_dir_compare(const void *_rec1, const void *_rec2)
{
    const H5HF_huge_bt2_filt_dir_rec_t *rec1 = (const H5HF_huge_bt2_filt_dir_rec_t *)_rec1;
    const H5HF_huge_bt2_filt_dir_rec_t *rec2 = (const H5HF_huge_bt2_filt_dir_rec_t *)_rec2;
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_dir_compare)

#ifdef QAK
HDfprintf(stderr, "%s: rec1 = {%a, %Hu, %x, %Hu}\n", "H5HF_huge_btree2_filt_dir_compare", rec1->addr, rec1->len, rec1->filter_mask, rec1->obj_size);
HDfprintf(stderr, "%s: rec2 = {%a, %Hu, %x, %Hu}\n", "H5HF_huge_btree2_filt_dir_compare", rec2->addr, rec2->len, rec2->filter_mask, rec2->obj_size);
#endif /* QAK */
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

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_huge_btree2_filt_dir_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_dir_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_dir_encode(const H5F_t *f, uint8_t *raw, const void *_nrecord)
{
    const H5HF_huge_bt2_filt_dir_rec_t *nrecord = (const H5HF_huge_bt2_filt_dir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_dir_encode)

    /* Encode the record's fields */
    H5F_addr_encode(f, &raw, nrecord->addr);
    H5F_ENCODE_LENGTH(f, raw, nrecord->len);
    UINT32ENCODE(raw, nrecord->filter_mask);
    H5F_ENCODE_LENGTH(f, raw, nrecord->obj_size);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_dir_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_dir_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_dir_decode(const H5F_t *f, const uint8_t *raw, void *_nrecord)
{
    H5HF_huge_bt2_filt_dir_rec_t *nrecord = (H5HF_huge_bt2_filt_dir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_dir_decode)

    /* Decode the record's fields */
    H5F_addr_decode(f, &raw, &nrecord->addr);
    H5F_DECODE_LENGTH(f, raw, nrecord->len);
    UINT32DECODE(raw, nrecord->filter_mask);
    H5F_DECODE_LENGTH(f, raw, nrecord->obj_size);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_dir_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_btree2_filt_dir_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_btree2_filt_dir_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *_nrecord, const void UNUSED *_udata)
{
    const H5HF_huge_bt2_filt_dir_rec_t *nrecord = (const H5HF_huge_bt2_filt_dir_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_btree2_filt_dir_debug)

    HDfprintf(stream, "%*s%-*s {%a, %Hu, %x, %Hu}\n", indent, "", fwidth, "Record:",
        nrecord->addr, nrecord->len, nrecord->filter_mask, nrecord->obj_size);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_huge_btree2_filt_dir_debug() */

