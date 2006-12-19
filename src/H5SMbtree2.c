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


/********************/
/* Local Prototypes */
/********************/

/* JAMES: name these as "H5SM_btree_store", etc? */
static herr_t H5SM_message_store(void *native, const void *udata);
static herr_t H5SM_message_retrieve(void *udata, const void *native);
static herr_t H5SM_message_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);


/*****************************/
/* Library Private Variables */
/*****************************/
/* v2 B-tree class for SOHM indexes*/
const H5B2_class_t H5SM_INDEX[1]={{     /* B-tree class information */
    H5B2_SOHM_INDEX_ID,                 /* Type of B-tree */
    sizeof(H5SM_sohm_t),                /* Size of native record */
    H5SM_message_store,                 /* Record storage callback */
    H5SM_message_retrieve,              /* Record retrieval callback */
    H5SM_message_compare,               /* Record comparison callback */
    H5SM_message_encode,                /* Record encoding callback */
    H5SM_message_decode,                /* Record decoding callback */
    H5SM_message_debug                  /* Record debugging callback */
}};

/*******************/
/* Local Variables */
/*******************/


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

    hash_diff = key->hash;
    hash_diff -= mesg->hash;

    /* If the hash values match, make sure the messages are really the same */
    if(0 == hash_diff)     {
        /* Compare either the heap_ids directly (if the key has one)
         * or the encoded buffers
         */
        /* JAMES: not a great test.  Use a flag instead? */
        if(key->encoding_size == 0)
        {
            ret_value = (herr_t) (key->mesg_heap_id - mesg->fheap_id);
        }
        else
        {
            unsigned char buf2[H5O_MESG_MAX_SIZE];
            herr_t ret;

            /* We need to see if this message is in fact the message stored
             * in the heap.  Read it from the heap and compare the two.
             */
            HDmemset(buf2, 0, (size_t)H5O_MESG_MAX_SIZE);

            ret = H5HF_read(key->fheap, H5AC_dxpl_id, &(mesg->fheap_id), &buf2);
            HDassert(ret >= 0);

            /* JAMES: I think I want to use in-heap callback here. */
            ret_value = HDmemcmp(key->encoding, buf2, key->encoding_size);
        }
    }
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
 * Function:	H5SM_message_store
 *
 * Purpose:	Store a H5SM_sohm_t SOHM message in the B-tree by copying it
 *              from UDATA to NATIVE.
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
H5SM_message_store(void *native, const void *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_store)

    /* Copy the source message to the B-tree */
    *(H5SM_sohm_t *)native = *(const H5SM_sohm_t *)udata;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_message_store */


/*-------------------------------------------------------------------------
 * Function:	H5SM_message_retrieve
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
H5SM_message_retrieve(void *udata, const void *native)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_retrieve)

    /* Copy the B-tree's native message to the udata buffer */
    *(H5SM_sohm_t *)udata = *(const H5SM_sohm_t *)native;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_message_retrieve */


/*-------------------------------------------------------------------------
 * Function:	H5SM_message_encode
 *
 * Purpose:	Serialize the SOHM message.
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
H5SM_message_encode(const H5F_t UNUSED *f, uint8_t *raw, const void *_nrecord)
{
    const H5SM_sohm_t *message = (const H5SM_sohm_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_encode)

    /* Encode the SOHM's fields */
    UINT32ENCODE(raw, message->hash);
    UINT32ENCODE(raw, message->ref_count);
    UINT64ENCODE(raw, message->fheap_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_message_encode */


/*-------------------------------------------------------------------------
 * Function:	H5SM_message_decode
 *
 * Purpose:	Read an encoded SOHM message into an H5SM_sohm_t struct.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
/* JAMES: can we combine this with H5SMcache functions? */
herr_t
H5SM_message_decode(const H5F_t UNUSED *f, const uint8_t *raw, void *_nrecord)
{
    H5SM_sohm_t *message = (H5SM_sohm_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_decode)

    /* Encode the SOHM's fields */
    UINT32DECODE(raw, message->hash);
    UINT32DECODE(raw, message->ref_count);
    UINT64DECODE(raw, message->fheap_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_message_decode */


/*-------------------------------------------------------------------------
 * Function:	H5SM_message_debug
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
H5SM_message_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *record, const void UNUSED *_udata)
{
    const H5SM_sohm_t *sohm = (const H5SM_sohm_t *)record;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_debug)

    HDfprintf(stream, "%*s%-*s {%a, %lo, %Hu}\n", indent, "", fwidth, "Record:",
        sohm->fheap_id, sohm->hash, sohm->ref_count);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_message_debug */


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
       *(H5SM_fheap_id_t *)op_data = message->fheap_id;

    FUNC_LEAVE_NOAPI(SUCCEED)
}


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
}


/*-------------------------------------------------------------------------
 * Function:	H5SM_convert_to_list_op
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
H5SM_convert_to_list_op(const void * record, void *op_data)
{
    const H5SM_sohm_t *message = (const H5SM_sohm_t *) record;
    const H5SM_list_t *list = (const H5SM_list_t *) op_data;
    hsize_t      x;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_convert_to_list_op)

    HDassert(record);
    HDassert(op_data);

    /* Insert this message into the list */
    for(x=0; x<list->header->list_to_btree; x++)
    {
        if(list->messages[x].hash == H5O_HASH_UNDEF) /* JAMES: is this a valid test? */
        {
            HDmemcpy(&(list->messages[x]), message, sizeof(H5SM_sohm_t));
            break;
        }
    }

    /* Increment the number of messages in the list */
    ++list->header->num_messages;

    FUNC_LEAVE_NOAPI(SUCCEED)
}

