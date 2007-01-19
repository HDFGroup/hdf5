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

/****************/
/* Module Setup */
/****************/

#define H5SM_PACKAGE		/*suppress error about including H5SMpkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5SMpkg.h"            /* Shared object header messages        */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Udata struct for call to H5SM_btree_compare_cb */
typedef struct H5SM_compare_udata_t {
    H5SM_mesg_key_t *key;   /* Key; compare this against record in heap */
    herr_t ret;            /* Return value; set this to result of memcmp */
} H5SM_compare_udata_t;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5SM_btree_compare_cb(const void *obj, size_t obj_len, void *_udata);
static herr_t H5SM_btree_store(void *native, const void *udata);
static herr_t H5SM_btree_retrieve(void *udata, const void *native);
static herr_t H5SM_btree_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);


/*****************************/
/* Library Private Variables */
/*****************************/
/* v2 B-tree class for SOHM indexes*/
const H5B2_class_t H5SM_INDEX[1]={{   /* B-tree class information */
    H5B2_SOHM_INDEX_ID,               /* Type of B-tree */
    sizeof(H5SM_sohm_t),              /* Size of native record */
    H5SM_btree_store,                 /* Record storage callback */
    H5SM_btree_retrieve,              /* Record retrieval callback */
    H5SM_message_compare,             /* Record comparison callback */
    H5SM_message_encode,              /* Record encoding callback */
    H5SM_message_decode,              /* Record decoding callback */
    H5SM_btree_debug                  /* Record debugging callback */
}};

/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5SM_btree_compare_cb
 *
 * Purpose:	Callback for H5HF_op, used in H5SM_btree_compare_cb below.
 *              Determines whether the search key passed in in _UDATA is
 *              equal to OBJ or not.
 *
 *              Passes back the result in _UDATA->RET
 *
 * Return:	Negative on error, non-negative on success
 *
 * Programmer:	James Laird
 *              Monday, January 8, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_btree_compare_cb(const void *obj, size_t obj_len, void *_udata)
{
    H5SM_compare_udata_t *udata = (H5SM_compare_udata_t *)_udata;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_btree_compare_cb)

    /* If the encoding sizes are different, it's not the same object */
    if(udata->key->encoding_size != obj_len) {
        if(udata->key->encoding_size > obj_len)
            udata->ret = 1;
        else
            udata->ret = -1;
    } else {
        /* Sizes are the same.  Return result of memcmp */
        udata->ret = HDmemcmp(udata->key->encoding, obj, obj_len);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_btree_compare_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_message_compare
 *
 * Purpose:	Determine whether the search key rec1 represents a shared
 *              message that is equal to rec2 or not, and if not, whether
 *              rec1 is "greater than" or "less than" rec2.
 *
 * Return:	0 if rec1 == rec2
 *              Negative if rec1 < rec2
 *              Positive if rec1 > rec2
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_message_compare(const void *rec1, const void *rec2)
{
    const H5SM_mesg_key_t *key = (const H5SM_mesg_key_t *) rec1;
    const H5SM_sohm_t *mesg = (const H5SM_sohm_t *) rec2;
    int64_t hash_diff;  /* Has to be able to hold two 32-bit values */
    herr_t ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_compare)

    /* JAMES: might be able to spare a sentinel byte instead of worrying about
     * refcounts.  Here, we need to find a deleted message in a B-tree to
     * actually delete it.
     */
    /* JAMES HDassert(mesg->ref_count > 0); */

    hash_diff = key->message.hash;
    hash_diff -= mesg->hash;

    /* If the hash values match, make sure the messages are really the same */
    if(0 == hash_diff) {
        /* Compare either the heap_ids directly (if the key has one)
         * or the encoded buffers
         */
        if(key->encoding_size == 0)
        {
            HDassert(key->encoding == NULL);
            ret_value = (herr_t) (key->message.fheap_id - mesg->fheap_id);
        } /* end if */
        else
        {
            /* Hash values match, but we don't have a heap ID for the key.
             * Compare the encoded message with the one in the heap.
             */
            H5SM_compare_udata_t udata;
            herr_t ret;

            /* Casting away const OK.  -JML */
            udata.key = key;

            /* Call heap op routine with comparison callback */
            ret = H5HF_op(key->fheap, H5AC_dxpl_id, &(mesg->fheap_id), H5SM_btree_compare_cb, &udata);
            HDassert(ret >= 0);

            ret_value = udata.ret;
        } /* end else */
    } /* end if */
    else {
        /* Compress 64-bit hash_diff to fit in an herr_t */
        if(hash_diff > 0)
            ret_value = 1;
        else
            ret_value = -1;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_message_compare */


/*-------------------------------------------------------------------------
 * Function:	H5SM_btree_store
 *
 * Purpose:	Store a H5SM_sohm_t SOHM message in the B-tree.  The message
 *              comes in UDATA as a H5SM_mesg_key_t* and is copied to
 *              NATIVE as a H5SM_sohm_t.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_btree_store(void *native, const void *udata)
{
    const H5SM_mesg_key_t *key = (const H5SM_mesg_key_t *)udata;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_btree_store)

    /* Copy the source message to the B-tree */
    *(H5SM_sohm_t *)native = key->message;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_btree_store */


/*-------------------------------------------------------------------------
 * Function:	H5SM_btree_retrieve
 *
 * Purpose:	Retrieve a H5SM_sohm_t SOHM message from the B-tree by
 *              copying it from NATIVE to UDATA.
 *
 *              Quincey said this function may no longer be used.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_btree_retrieve(void *udata, const void *native)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_btree_retrieve)

    /* Copy the B-tree's native message to the udata buffer */
    *(H5SM_sohm_t *)udata = *(const H5SM_sohm_t *)native;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_btree_retrieve */


/*-------------------------------------------------------------------------
 * Function:	H5SM_btree_debug
 *
 * Purpose:	Print debugging information for a H5SM_sohm_t.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_btree_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *record, const void UNUSED *_udata)
{
    const H5SM_sohm_t *sohm = (const H5SM_sohm_t *)record;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_btree_debug)

    HDfprintf(stream, "%*s%-*s {%a, %lo, %Hx}\n", indent, "", fwidth, "Record:",
        sohm->fheap_id, sohm->hash, sohm->ref_count);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_btree_debug */


/*-------------------------------------------------------------------------
 * Function:	H5SM_incr_ref
 *
 * Purpose:	Increment the reference count for a SOHM message and return
 *              the message's heap ID.
 *
 *              The message pointer is actually returned via op_data, which
 *              should be a pointer to a H5SM_fheap_id_t.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_incr_ref(void *record, void *op_data, hbool_t *changed)
{
    H5SM_sohm_t *message = (H5SM_sohm_t *) record;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_incr_ref)

    HDassert(record);
    HDassert(op_data);
    HDassert(changed);

    ++message->ref_count;
    *changed = TRUE;

    if(op_data)
       *(H5O_fheap_id_t *)op_data = message->fheap_id;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_incr_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_decr_ref
 *
 * Purpose:	Decrement the reference count for a SOHM message.  Doesn't
 *              remove the record from the B-tree even if the refcount
 *              reaches zero.
 *
 *              The new message is returned through op_data.  If its
 *              reference count is zero, the calling function should
 *              remove this record from the B-tree.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_decr_ref(void *record, void *op_data, hbool_t *changed)
{
    H5SM_sohm_t *message = (H5SM_sohm_t *) record;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_decr_ref)

    HDassert(record);
    HDassert(op_data);
    HDassert(changed);

    --message->ref_count;
    *changed = TRUE;

    if(op_data)
       *(H5SM_sohm_t *)op_data = *message;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_decr_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_btree_convert_to_list_op
 *
 * Purpose:	An H5B2_remove_t callback function to convert a SOHM
 *              B-tree index to a list.
 *
 *              Inserts this record into the list passed through op_data.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_btree_convert_to_list_op(const void * record, void *op_data)
{
    const H5SM_sohm_t *message = (const H5SM_sohm_t *) record;
    const H5SM_list_t *list = (const H5SM_list_t *) op_data;
    hsize_t      x;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_btree_convert_to_list_op)

    HDassert(record);
    HDassert(op_data);

    /* Insert this message into the list */
    for(x=0; x<list->header->list_max; x++)
    {
        if(list->messages[x].ref_count == 0)
        {
            HDmemcpy(&(list->messages[x]), message, sizeof(H5SM_sohm_t));
            HDassert(list->messages[x].ref_count > 0);
            break;
        }
    }

    /* Increment the number of messages in the list */
    ++list->header->num_messages;

    FUNC_LEAVE_NOAPI(SUCCEED)
}

