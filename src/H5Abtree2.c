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
 * Created:		H5Abtree2.c
 *			Dec  4 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		v2 B-tree callbacks for indexing attributes on objects
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5A_PACKAGE		/*suppress error about including H5Apkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes	  			*/
#include "H5Eprivate.h"		/* Error handling		  	*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/*
 * Data exchange structure for dense attribute storage.  This structure is
 * passed through the fractal heap layer to compare attributes.
 */
typedef struct H5A_fh_ud_cmp_t {
    /* downward */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */
    const char  *name;                  /* Name of attribute to compare      */
    H5B2_found_t found_op;              /* Callback when correct attribute is found */
    void        *found_op_data;         /* Callback data when correct attribute is found */

    /* upward */
    int         cmp;                    /* Comparison of two attribute names */
} H5A_fh_ud_cmp_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* v2 B-tree function callbacks */

/* v2 B-tree driver callbacks for 'name' index */
static herr_t H5A_dense_btree2_name_store(void *native, const void *udata);
static herr_t H5A_dense_btree2_name_retrieve(void *udata, const void *native);
static herr_t H5A_dense_btree2_name_compare(const void *rec1, const void *rec2);
static herr_t H5A_dense_btree2_name_encode(const H5F_t *f, uint8_t *raw,
    const void *native);
static herr_t H5A_dense_btree2_name_decode(const H5F_t *f, const uint8_t *raw,
    void *native);
static herr_t H5A_dense_btree2_name_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);

/* Fractal heap function callbacks */
static herr_t H5A_dense_fh_name_cmp(const void *obj, size_t obj_len, void *op_data);


/*********************/
/* Package Variables */
/*********************/
/* v2 B-tree class for indexing 'name' field of links */
const H5B2_class_t H5A_BT2_NAME[1]={{  /* B-tree class information */
    H5B2_ATTR_DENSE_NAME_ID,                /* Type of B-tree */
    sizeof(H5A_dense_bt2_name_rec_t),  /* Size of native record */
    H5A_dense_btree2_name_store,       /* Record storage callback */
    H5A_dense_btree2_name_retrieve,    /* Record retrieval callback */
    H5A_dense_btree2_name_compare,     /* Record comparison callback */
    H5A_dense_btree2_name_encode,      /* Record encoding callback */
    H5A_dense_btree2_name_decode,      /* Record decoding callback */
    H5A_dense_btree2_name_debug        /* Record debugging callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5A_dense_fh_name_cmp
 *
 * Purpose:	Compares the name of a attribute in a fractal heap to another
 *              name
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  4 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_fh_name_cmp(const void *obj, size_t UNUSED obj_len, void *_udata)
{
    H5A_fh_ud_cmp_t *udata = (H5A_fh_ud_cmp_t *)_udata;         /* User data for 'op' callback */
    H5A_t *attr = NULL;                 /* Pointer to attribute created from heap object */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_fh_name_cmp)

    /* Decode link information */
    if(NULL == (attr = H5O_msg_decode(udata->f, udata->dxpl_id, H5O_ATTR_ID, obj)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "can't decode attribute")

    /* Compare the string values */
    udata->cmp = HDstrcmp(udata->name, attr->name);

    /* Check for correct attribute & callback to make */
    if(udata->cmp == 0 && udata->found_op) {
        if((udata->found_op)(attr, udata->found_op_data) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTOPERATE, FAIL, "attribute found callback failed")
    } /* end if */

done:
    /* Release the space allocated for the attrbute */
    if(attr)
        H5O_msg_free(H5O_ATTR_ID, attr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_fh_name_cmp() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_btree2_name_store
 *
 * Purpose:	Store user information into native record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, December  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_btree2_name_store(void *_nrecord, const void *_udata)
{
    const H5A_bt2_ud_ins_t *udata = (const H5A_bt2_ud_ins_t *)_udata;
    H5A_dense_bt2_name_rec_t *nrecord = (H5A_dense_bt2_name_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_dense_btree2_name_store)

    /* Copy user information info native record */
    nrecord->hash = udata->common.name_hash;
    nrecord->flags = udata->common.flags;
    HDmemcpy(nrecord->id, udata->id, (size_t)H5A_DENSE_FHEAP_ID_LEN);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5A_dense_btree2_name_store() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_btree2_name_retrieve
 *
 * Purpose:	Retrieve native information from record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, December  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_btree2_name_retrieve(void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_dense_btree2_name_retrieve)

    *(H5A_dense_bt2_name_rec_t *)udata = *(const H5A_dense_bt2_name_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5A_dense_btree2_name_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_btree2_name_compare
 *
 * Purpose:	Compare two native information records, according to some key
 *
 * Return:	<0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:	Quincey Koziol
 *              Monday, December  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_btree2_name_compare(const void *_bt2_udata, const void *_bt2_rec)
{
    const H5A_bt2_ud_common_t *bt2_udata = (const H5A_bt2_ud_common_t *)_bt2_udata;
    const H5A_dense_bt2_name_rec_t *bt2_rec = (const H5A_dense_bt2_name_rec_t *)_bt2_rec;
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_dense_btree2_name_compare)

    /* Sanity check */
    HDassert(bt2_udata);
    HDassert(bt2_rec);

    /* Check hash value */
    if(bt2_udata->name_hash < bt2_rec->hash)
        HGOTO_DONE(-1)
    else if(bt2_udata->name_hash > bt2_rec->hash)
        HGOTO_DONE(1)
    else {
        H5A_fh_ud_cmp_t fh_udata;       /* User data for fractal heap 'op' callback */
        herr_t status;                  /* Status from fractal heap 'op' routine */

        /* Sanity check */
        HDassert(bt2_udata->name_hash == bt2_rec->hash);

        /* Prepare user data for callback */
        /* down */
        fh_udata.f = bt2_udata->f;
        fh_udata.dxpl_id = bt2_udata->dxpl_id;
        fh_udata.name = bt2_udata->name;
        fh_udata.found_op = bt2_udata->found_op;
        fh_udata.found_op_data = bt2_udata->found_op_data;

        /* up */
        fh_udata.cmp = 0;

        /* Check for attribute in shared storage */
        if(bt2_rec->flags) {
HDfprintf(stderr, "%s: Shared dense storage for attributes not supported yet!\n", "H5A_dense_btree2_name_compare");
HDassert(0 && "Shared dense storage for attributes not supported yet!");
        } /* end if */

        /* Check if the user's link and the B-tree's link have the same name */
        status = H5HF_op(bt2_udata->fheap, bt2_udata->dxpl_id, bt2_rec->id,
                H5A_dense_fh_name_cmp, &fh_udata);
        HDassert(status >= 0);

        /* Callback will set comparison value */
        HGOTO_DONE(fh_udata.cmp)
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A_dense_btree2_name_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_btree2_name_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, December  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_btree2_name_encode(const H5F_t UNUSED *f, uint8_t *raw, const void *_nrecord)
{
    const H5A_dense_bt2_name_rec_t *nrecord = (const H5A_dense_bt2_name_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_dense_btree2_name_encode)

    /* Encode the record's fields */
    UINT32ENCODE(raw, nrecord->hash)
    HDmemcpy(raw, nrecord->id, (size_t)H5A_DENSE_FHEAP_ID_LEN);
    *raw++ = nrecord->flags;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5A_dense_btree2_name_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_btree2_name_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, December  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_btree2_name_decode(const H5F_t UNUSED *f, const uint8_t *raw, void *_nrecord)
{
    H5A_dense_bt2_name_rec_t *nrecord = (H5A_dense_bt2_name_rec_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_dense_btree2_name_decode)

    /* Decode the record's fields */
    UINT32DECODE(raw, nrecord->hash)
    HDmemcpy(nrecord->id, raw, (size_t)H5A_DENSE_FHEAP_ID_LEN);
    nrecord->flags = *raw++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5A_dense_btree2_name_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_btree2_name_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, December  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_btree2_name_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *_nrecord, const void UNUSED *_udata)
{
    const H5A_dense_bt2_name_rec_t *nrecord = (const H5A_dense_bt2_name_rec_t *)_nrecord;
    unsigned u;                 /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_dense_btree2_name_debug)

    HDfprintf(stream, "%*s%-*s {%lx, ", indent, "", fwidth, "Record:",
        nrecord->hash);
    for(u = 0; u < H5A_DENSE_FHEAP_ID_LEN; u++)
        HDfprintf(stderr, "%02x%s", nrecord->id[u], (u < (H5A_DENSE_FHEAP_ID_LEN - 1) ? " " : ", "));
    HDfprintf(stderr, "%02x}\n", nrecord->flags);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5A_dense_btree2_name_debug() */

