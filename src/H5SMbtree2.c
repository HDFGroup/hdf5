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

/****************/
/* Module Setup */
/****************/

#define H5O_PACKAGE		/*suppress error about including H5Opkg 	  */
#define H5SM_PACKAGE		/*suppress error about including H5SMpkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Opkg.h"             /* Object Headers                       */
#include "H5SMpkg.h"            /* Shared object header messages        */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Udata struct for calls to H5SM_btree_compare_cb and H5SM_compare_iter_op*/
typedef struct H5SM_compare_udata_t {
    const H5SM_mesg_key_t *key; /* Key; compare this against stored message */
    H5O_msg_crt_idx_t idx;      /* Index of the message in the OH, if applicable */
    herr_t ret;                 /* Return value; set this to result of memcmp */
} H5SM_compare_udata_t;


/********************/
/* Local Prototypes */
/********************/

/* v2 B-tree callbacks */
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
 * Purpose:	Callback for H5HF_op, used in H5SM_message_compare below.
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

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_btree_compare_cb)

    /* If the encoding sizes are different, it's not the same object */
    if(udata->key->encoding_size > obj_len)
        udata->ret = 1;
    else if(udata->key->encoding_size < obj_len)
        udata->ret = -1;
    else
        /* Sizes are the same.  Return result of memcmp */
        udata->ret = HDmemcmp(udata->key->encoding, obj, obj_len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_btree_compare_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_compare_iter_op
 *
 * Purpose:	OH iteration callback to compare a key against a message in
 *              an OH
 *
 * Return:	0 if this is not the message we're searching for
 *              1 if this is the message we're searching for (with memcmp
 *                      result returned in udata)
 *              negative on error
 *
 * Programmer:	James Laird
 *              Wednesday, February 7, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_compare_iter_op(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/, unsigned sequence,
    hbool_t UNUSED *oh_modified, void *_udata/*in,out*/)
{
    H5SM_compare_udata_t *udata = (H5SM_compare_udata_t *) _udata;
    herr_t ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI_NOINIT(H5SM_compare_iter_op)

    /*
     * Check arguments.
     */
    HDassert(oh);
    HDassert(mesg);
    HDassert(udata && udata->key);

    /* Check the creation index for this message */
    if(sequence == udata->idx) {
        size_t aligned_encoded_size = H5O_ALIGN_OH(oh, udata->key->encoding_size);

        /* Sanity check the message's length */
        HDassert(mesg->raw_size > 0);

        if(aligned_encoded_size > mesg->raw_size)
            udata->ret = 1;
        else if(aligned_encoded_size < mesg->raw_size)
            udata->ret = -1;
        else {
            /* Check if the message is dirty & flush it to the object header if so */
            if(mesg->dirty)
                if(H5O_msg_flush(udata->key->file, oh, mesg) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, H5_ITER_ERROR, "unable to encode object header message")

            HDassert(udata->key->encoding_size <= mesg->raw_size);
            udata->ret = HDmemcmp(udata->key->encoding, mesg->raw, udata->key->encoding_size);
        } /* end else */

        /* Indicate that we found the message we were looking for */
        ret_value = H5_ITER_STOP;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_compare_iter_op() */


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
    herr_t ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_compare)

    /* If the key has an fheap ID, we're looking for a message that's
     * already in the index; if the fheap ID matches, we've found the message
     * and can stop immediately.
     * Likewise, if the message has an OH location that is matched by the
     * message in the index, we've found the message.
     */
    if(mesg->location == H5SM_IN_HEAP && key->message.location == H5SM_IN_HEAP) {
        if(key->message.u.heap_loc.fheap_id == mesg->u.heap_loc.fheap_id)
            HGOTO_DONE(0);
    } /* end if */
    else if(mesg->location == H5SM_IN_OH && key->message.location == H5SM_IN_OH) {
        if(key->message.u.mesg_loc.oh_addr == mesg->u.mesg_loc.oh_addr &&
                key->message.u.mesg_loc.index == mesg->u.mesg_loc.index &&
                key->message.msg_type_id == mesg->msg_type_id)
            HGOTO_DONE(0);
    } /* end if */

    /* Compare hash values */
    if(key->message.hash > mesg->hash)
        ret_value = 1;
    else if(key->message.hash < mesg->hash)
        ret_value = -1;
    /* If the hash values match, make sure the messages are really the same */
    else {
        /* Hash values match; compare the encoded message with the one in
         * the index.
         */
        H5SM_compare_udata_t udata;
        herr_t status;

        HDassert(key->message.hash == mesg->hash);
        HDassert(key->encoding_size > 0 && key->encoding);

        /* Set up user data for callback */
        udata.key = key;

        /* Compare the encoded message with either the message in the heap or
         * the message in an object header.
         */
        if(mesg->location == H5SM_IN_HEAP) {
            /* Call heap op routine with comparison callback */
            status = H5HF_op(key->fheap, key->dxpl_id, &(mesg->u.heap_loc.fheap_id), H5SM_btree_compare_cb, &udata);
            HDassert(status >= 0);
        } /* end if */
        else {
            H5O_loc_t oloc;             /* Object owning the message */
            H5O_mesg_operator_t op;             /* Message operator */

            /* Sanity checks */
            HDassert(key->file);
            HDassert(mesg->location == H5SM_IN_OH);

            /* Reset the object location */
            status = H5O_loc_reset(&oloc);
            HDassert(status >= 0);

            /* Set up object location */
            oloc.file = key->file;
            oloc.addr = mesg->u.mesg_loc.oh_addr;

            /* Finish setting up user data for iterator */
            udata.idx = mesg->u.mesg_loc.index;

            /* Locate the right message and compare with it */
            op.op_type = H5O_MESG_OP_LIB;
            op.u.lib_op = H5SM_compare_iter_op;
            status = H5O_msg_iterate(&oloc, mesg->msg_type_id, &op, &udata, key->dxpl_id);
            HDassert(status >= 0);
        } /* end else */

        ret_value = udata.ret;
    } /* end if */

done:
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

    if(sohm->location == H5SM_IN_HEAP)
        HDfprintf(stream, "%*s%-*s {%a, %lo, %Hx}\n", indent, "", fwidth,
            "Shared Message in heap:",
            sohm->u.heap_loc.fheap_id, sohm->hash, sohm->u.heap_loc.ref_count);
    else {
        HDassert(sohm->location == H5SM_IN_OH);
        HDfprintf(stream, "%*s%-*s {%a, %lo, %Hx, %Hx}\n", indent, "", fwidth,
            "Shared Message in OH:",
            sohm->u.mesg_loc.oh_addr, sohm->hash, sohm->msg_type_id, sohm->u.mesg_loc.index);
    } /* end else */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_btree_debug */


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
    const H5SM_sohm_t *message = (const H5SM_sohm_t *)record;
    const H5SM_list_t *list = (const H5SM_list_t *)op_data;
    size_t mesg_idx;            /* Index of message to modify */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_btree_convert_to_list_op)

    /* Sanity checks */
    HDassert(record);
    HDassert(op_data);

    /* Get the message index, and increment the # of messages in list */
    mesg_idx = list->header->num_messages++;
    HDassert(list->header->num_messages <= list->header->list_max);

    /* Insert this message at the end of the list */
    HDassert(list->messages[mesg_idx].location == H5SM_NO_LOC);
    HDassert(message->location != H5SM_NO_LOC);
    HDmemcpy(&(list->messages[mesg_idx]), message, sizeof(H5SM_sohm_t));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_btree_convert_to_list_op() */

