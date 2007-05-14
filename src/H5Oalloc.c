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
 * Created:		H5Oalloc.c
 *			Nov 17 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Object header allocation routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Opkg.h"             /* Object headers			*/

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

static herr_t H5O_add_gap(H5O_t *oh, unsigned chunkno, unsigned idx,
    uint8_t *new_gap_loc, size_t new_gap_size);
static herr_t H5O_eliminate_gap(H5O_t *oh, H5O_mesg_t *mesg,
    uint8_t *new_gap_loc, size_t new_gap_size);
static herr_t H5O_alloc_null(H5O_t *oh, unsigned null_idx,
    const H5O_msg_class_t *new_type, void *new_native, size_t new_size);
static htri_t H5O_alloc_extend_chunk(H5F_t *f, H5O_t *oh, unsigned chunkno,
    size_t size, unsigned * msg_idx);
static unsigned H5O_alloc_new_chunk(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    size_t size);
static htri_t H5O_move_msgs_forward(H5O_t *oh);
static htri_t H5O_merge_null(H5O_t *oh);
static htri_t H5O_remove_empty_chunks(H5F_t *f, H5O_t *oh, hid_t dxpl_id);


/*********************/
/* Package Variables */
/*********************/

/* Declare extern the free list for H5O_cont_t's */
H5FL_EXTERN(H5O_cont_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5O_add_gap
 *
 * Purpose:     Add a gap to a chunk
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Oct 17 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_add_gap(H5O_t *oh, unsigned chunkno, unsigned idx,
    uint8_t *new_gap_loc, size_t new_gap_size)
{
    hbool_t merged_with_null;           /* Whether the gap was merged with a null message */
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_add_gap)

    /* check args */
    HDassert(oh);
    HDassert(oh->version > H5O_VERSION_1);
    HDassert(new_gap_loc);
    HDassert(new_gap_size);

    /* Check for existing null message in chunk */
    merged_with_null = FALSE;
    for(u = 0; u < oh->nmesgs && !merged_with_null; u++) {
        /* Find a null message in the chunk with the new gap */
        /* (a null message that's not the one we are eliminating) */
        if(H5O_NULL_ID == oh->mesg[u].type->id && oh->mesg[u].chunkno == chunkno
                && u != idx) {
            /* Sanity check - chunks with null messages shouldn't have a gap */
            HDassert(oh->chunk[chunkno].gap == 0);

            /* Eliminate the gap in the chunk */
            if(H5O_eliminate_gap(oh, &oh->mesg[u], new_gap_loc, new_gap_size) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, FAIL, "can't eliminate gap in chunk")

            /* Set flag to indicate that the gap was handled */
            merged_with_null = TRUE;
        } /* end if */
    } /* end for */

    /* If we couldn't find a null message in the chunk, move the gap to the end */
    if(!merged_with_null) {
        /* Adjust message offsets after new gap forward in chunk */
        for(u = 0; u < oh->nmesgs; u++)
            if(oh->mesg[u].chunkno == chunkno && oh->mesg[u].raw > new_gap_loc)
                oh->mesg[u].raw -= new_gap_size;

        /* Slide raw message info forward in chunk image */
        HDmemmove(new_gap_loc, new_gap_loc + new_gap_size, 
                (size_t)((oh->chunk[chunkno].image + (oh->chunk[chunkno].size - H5O_SIZEOF_CHKSUM_OH(oh))) - (new_gap_loc + new_gap_size)));

        /* Add existing gap size to new gap size */
        new_gap_size += oh->chunk[chunkno].gap;

        /* Merging with existing gap will allow for a new null message */
        if(new_gap_size >= (size_t)H5O_SIZEOF_MSGHDR_OH(oh)) {
            H5O_mesg_t *null_msg;       /* Pointer to new null message */

            /* Check if we need to extend message table to hold the new null message */
            if(oh->nmesgs >= oh->alloc_nmesgs)
                if(H5O_alloc_msgs(oh, (size_t)1) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "can't allocate more space for messages")

            /* Increment new gap size */
            oh->chunk[chunkno].gap += new_gap_size;

            /* Create new null message, with the tail of the previous null message */
            null_msg = &(oh->mesg[oh->nmesgs++]);
            null_msg->type = H5O_MSG_NULL;
            null_msg->dirty = TRUE;
            null_msg->native = NULL;
            null_msg->raw_size = new_gap_size - H5O_SIZEOF_MSGHDR_OH(oh);
            null_msg->raw = (oh->chunk[chunkno].image + oh->chunk[chunkno].size)
                    - (H5O_SIZEOF_CHKSUM_OH(oh) + null_msg->raw_size);
            null_msg->chunkno = chunkno;

            /* Zero out new null message's raw data */
            if(null_msg->raw_size)
                HDmemset(null_msg->raw, 0, null_msg->raw_size);

            /* Reset size of gap in chunk */
            oh->chunk[chunkno].gap = 0;
        } /* end if */
        else
            oh->chunk[chunkno].gap = new_gap_size;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_add_gap() */


/*-------------------------------------------------------------------------
 * Function:    H5O_eliminate_gap
 *
 * Purpose:     Eliminate a gap in a chunk with a null message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Oct 17 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_eliminate_gap(H5O_t *oh, H5O_mesg_t *mesg, uint8_t *gap_loc, size_t gap_size)
{
    uint8_t *move_start, *move_end;     /* Pointers to area of messages to move */
    hbool_t null_before_gap;    /* Flag whether the null message is before the gap or not */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_eliminate_gap)

    /* check args */
    HDassert(oh);
    HDassert(oh->version > H5O_VERSION_1);
    HDassert(mesg);
    HDassert(gap_loc);
    HDassert(gap_size);

    /* Check if the null message is before or after the gap produced */
    null_before_gap = (hbool_t)(mesg->raw < gap_loc);

    /* Set up information about region of messages to move */
    if(null_before_gap) {
        move_start = mesg->raw + mesg->raw_size;
        move_end = gap_loc;
    } /* end if */
    else {
        move_start = gap_loc + gap_size;
        move_end = mesg->raw - H5O_SIZEOF_MSGHDR_OH(oh);
    } /* end else */

    /* Check for messages between null message and gap */
    if(move_end > move_start) {
        unsigned u;                 /* Local index variable */

        /* Look for messages that need to move, to adjust raw pointers in chunk */
        for(u = 0; u < oh->nmesgs; u++) {
            uint8_t *msg_start;     /* Start of encoded message in chunk */

            msg_start = oh->mesg[u].raw - H5O_SIZEOF_MSGHDR_OH(oh);
            if(oh->mesg[u].chunkno == mesg->chunkno
                    && (msg_start >= move_start && msg_start < move_end)) {
                /* Move message's raw pointer in appropriate direction */
                if(null_before_gap)
                    oh->mesg[u].raw += gap_size;
                else
                    oh->mesg[u].raw -= gap_size;
            } /* end if */
        } /* end for */

        /* Slide raw message info in chunk image */
        if(null_before_gap)
            /* Slide messages down */
            HDmemmove(move_start + gap_size, move_start, (size_t)(move_end - move_start));
        else {
            /* Slide messages up */
            HDmemmove(move_start - gap_size, move_start, (size_t)(move_end - move_start));

            /* Adjust start of null message */
            mesg->raw -= gap_size;
        } /* end else */
    }
    else if(move_end == move_start && !null_before_gap) {
	/* Slide null message up */
	HDmemmove(move_start - gap_size, move_start, mesg->raw_size + H5O_SIZEOF_MSGHDR_OH(oh));

	/* Adjust start of null message */
	mesg->raw -= gap_size;
    } /* end if */

    /* Zero out addition to null message */
    HDmemset(mesg->raw + mesg->raw_size, 0, gap_size);

    /* Adjust size of null message */
    mesg->raw_size += gap_size;

    /* Mark null message as dirty */
    mesg->dirty = TRUE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5O_eliminate_gap() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_alloc_null
 *
 * Purpose:     Allocate room for a new message from a null message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 22 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_alloc_null(H5O_t *oh, unsigned null_idx, const H5O_msg_class_t *new_type,
    void *new_native, size_t new_size)
{
    H5O_mesg_t *alloc_msg;              /* Pointer to null message to allocate out of */
    herr_t ret_value = SUCCEED; 	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_alloc_null)

    /* check args */
    HDassert(oh);
    HDassert(new_type);
    HDassert(new_size);

    /* Point to null message to allocate out of */
    alloc_msg = &oh->mesg[null_idx];

    /* Check if there's a need to split the null message */
    if(alloc_msg->raw_size > new_size) {
        /* Check for producing a gap in the chunk */
        if((alloc_msg->raw_size - new_size) < (size_t)H5O_SIZEOF_MSGHDR_OH(oh)) {
            size_t gap_size = alloc_msg->raw_size - new_size;     /* Size of gap produced */

            /* Adjust the size of the null message being eliminated */
            alloc_msg->raw_size = new_size;

            /* Add the gap to the chunk */
            if(H5O_add_gap(oh, alloc_msg->chunkno, null_idx, alloc_msg->raw + alloc_msg->raw_size, gap_size) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, UFAIL, "can't insert gap in chunk")
        } /* end if */
        else {
            size_t  new_mesg_size = new_size + H5O_SIZEOF_MSGHDR_OH(oh); /* Total size of newly allocated message */
            H5O_mesg_t *null_msg;       /* Pointer to new null message */

            /* Check if we need to extend message table to hold the new null message */
            if(oh->nmesgs >= oh->alloc_nmesgs) {
                if(H5O_alloc_msgs(oh, (size_t)1) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "can't allocate more space for messages")

                /* "Retarget" 'alloc_msg' pointer into newly re-allocated array of messages */
                alloc_msg = &oh->mesg[null_idx];
            } /* end if */

            /* Create new null message, with the tail of the previous null message */
            null_msg = &(oh->mesg[oh->nmesgs++]);
            null_msg->type = H5O_MSG_NULL;
            null_msg->dirty = TRUE;
            null_msg->native = NULL;
            null_msg->raw = alloc_msg->raw + new_mesg_size;
            null_msg->raw_size = alloc_msg->raw_size - new_mesg_size;
            null_msg->chunkno = alloc_msg->chunkno;

            /* Check for gap in new null message's chunk */
            if(oh->chunk[null_msg->chunkno].gap > 0) {
                unsigned null_chunkno = null_msg->chunkno;   /* Chunk w/gap */

                /* Eliminate the gap in the chunk */
                if(H5O_eliminate_gap(oh, null_msg,
                        ((oh->chunk[null_chunkno].image + oh->chunk[null_chunkno].size) - (H5O_SIZEOF_CHKSUM_OH(oh) + oh->chunk[null_chunkno].gap)),
                        oh->chunk[null_chunkno].gap) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTREMOVE, UFAIL, "can't eliminate gap in chunk")

                /* Set the gap size to zero for the chunk */
                oh->chunk[null_chunkno].gap = 0;
            } /* end if */

            /* Set the size of the new "real" message */
            alloc_msg->raw_size = new_size;
        } /* end else */
    } /* end if */

    /* Initialize the new message */
    alloc_msg->type = new_type;
    alloc_msg->dirty = TRUE;
    alloc_msg->native = new_native;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc_null() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_alloc_msgs
 *
 * Purpose:     Allocate more messages for a header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Nov 21 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_alloc_msgs(H5O_t *oh, size_t min_alloc)
{
    size_t old_alloc;                   /* Old number of messages allocated */
    size_t na;                          /* New number of messages allocated */
    H5O_mesg_t *new_mesg;               /* Pointer to new message array */
    herr_t ret_value = SUCCEED; 	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_alloc_msgs)

    /* check args */
    HDassert(oh);

    /* Initialize number of messages information */
    old_alloc = oh->alloc_nmesgs;
    na = oh->alloc_nmesgs + MAX(oh->alloc_nmesgs, min_alloc);   /* At least double */

    /* Attempt to allocate more memory */
    if(NULL == (new_mesg = H5FL_SEQ_REALLOC(H5O_mesg_t, oh->mesg, na)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Update ohdr information */
    oh->alloc_nmesgs = na;
    oh->mesg = new_mesg;

    /* Set new object header info to zeros */
    HDmemset(&oh->mesg[old_alloc], 0, (oh->alloc_nmesgs - old_alloc) * sizeof(H5O_mesg_t));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc_msgs() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_alloc_extend_chunk
 *
 * Purpose:     Attempt to extend a chunk that is allocated on disk.
 *
 *              If the extension is successful, and if the last message
 *		of the chunk is the null message, then that message will
 *		be extended with the chunk.  Otherwise a new null message
 *		is created.
 *
 *              f is the file in which the chunk will be written.  It is
 *              included to ensure that there is enough space to extend
 *              this chunk.
 *
 * Return:      TRUE:		The chunk has been extended, and *msg_idx
 *				contains the message index for null message
 *				which is large enough to hold size bytes.
 *
 *		FALSE:		The chunk cannot be extended, and *msg_idx
 *				is undefined.
 *
 *		FAIL:		Some internal error has been detected.
 *
 * Programmer:  John Mainzer -- 8/16/05
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_alloc_extend_chunk(H5F_t *f,
                       H5O_t *oh,
                       unsigned chunkno,
                       size_t size,
                       unsigned * msg_idx)
{
    size_t      delta;          /* Change in chunk's size */
    size_t      aligned_size = H5O_ALIGN_OH(oh, size);
    uint8_t     *old_image;     /* Old address of chunk's image in memory */
    size_t      old_size;       /* Old size of chunk */
    htri_t      tri_result;     /* Result from checking if chunk can be extended */
    int         extend_msg = -1;/* Index of null message to extend */
    uint8_t     new_size_flags = 0;     /* New chunk #0 size flags */
    hbool_t     adjust_size_flags = FALSE;      /* Whether to adjust the chunk #0 size flags */
    size_t      extra_prfx_size = 0; /* Extra bytes added to object header prefix */
    unsigned    u;              /* Local index variable */
    htri_t      ret_value = TRUE; 	/* return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_alloc_extend_chunk)

    /* check args */
    HDassert(f != NULL);
    HDassert(oh != NULL);
    HDassert(chunkno < oh->nchunks);
    HDassert(size > 0);
    HDassert(msg_idx != NULL);
    HDassert(H5F_addr_defined(oh->chunk[chunkno].addr));

    /* Test to see if the specified chunk ends with a null messages.
     * If successful, set the index of the the null message in extend_msg.
     */
    for(u = 0; u < oh->nmesgs; u++) {
        /* Check for null message at end of proper chunk */
        /* (account for possible checksum at end of chunk) */
        if(oh->mesg[u].chunkno == chunkno && H5O_NULL_ID == oh->mesg[u].type->id &&
                ((oh->mesg[u].raw + oh->mesg[u].raw_size)
                == ((oh->chunk[chunkno].image + oh->chunk[chunkno].size) -
                    (oh->chunk[chunkno].gap + H5O_SIZEOF_CHKSUM_OH(oh))))) {

            extend_msg = u;
            break;
        } /* end if */
    } /* end for */

    /* If we can extend an existing null message, adjust the delta appropriately */
    if(extend_msg >= 0) {
        HDassert(oh->chunk[chunkno].gap == 0);
        delta = aligned_size - oh->mesg[extend_msg].raw_size;
    } /* end if */
    else
        delta = (aligned_size + H5O_SIZEOF_MSGHDR_OH(oh)) - oh->chunk[chunkno].gap;
    delta = H5O_ALIGN_OH(oh, delta);

    /* Check for changing the chunk #0 data size enough to need adjusting the flags */
    if(oh->version > H5O_VERSION_1 && chunkno == 0) {
        uint64_t chunk0_size = oh->chunk[0].size - H5O_SIZEOF_HDR(oh);  /* Size of chunk 0's data */

        /* Check for moving from a 1-byte to a 2-byte size encoding */
        if(chunk0_size <= 255 && (chunk0_size + delta) > 255) {
            extra_prfx_size = 1;
            new_size_flags = H5O_HDR_CHUNK0_2;
            adjust_size_flags = TRUE;
        } /* end if */
        /* Check for moving from a 2-byte to a 4-byte size encoding */
        else if(chunk0_size <= 65535 && (chunk0_size + delta) > 65535) {
            extra_prfx_size = 2;
            new_size_flags = H5O_HDR_CHUNK0_4;
            adjust_size_flags = TRUE;
        } /* end if */
        /* Check for moving from a 4-byte to a 8-byte size encoding */
        else if(chunk0_size <= 4294967295 && (chunk0_size + delta) > 4294967295) {
            extra_prfx_size = 4;
            new_size_flags = H5O_HDR_CHUNK0_8;
            adjust_size_flags = TRUE;
        } /* end if */
    } /* end if */

    /* Determine whether the chunk can be extended */
    tri_result = H5MF_can_extend(f, H5FD_MEM_OHDR, oh->chunk[chunkno].addr,
                                 (hsize_t)(oh->chunk[chunkno].size), (hsize_t)(delta + extra_prfx_size));
    if(tri_result == FALSE)     /* can't extend -- we are done */
        HGOTO_DONE(FALSE)
    else if(tri_result < 0) /* error */
        HGOTO_ERROR(H5E_RESOURCE, H5E_SYSTEM, FAIL, "can't tell if we can extend chunk")

    /* If we get this far, we should be able to extend the chunk */
    if(H5MF_extend(f, H5FD_MEM_OHDR, oh->chunk[chunkno].addr, (hsize_t)(oh->chunk[chunkno].size), (hsize_t)(delta + extra_prfx_size)) < 0 )
        HGOTO_ERROR(H5E_RESOURCE, H5E_SYSTEM, FAIL, "can't extend chunk")

    /* Adjust object header prefix flags */
    if(adjust_size_flags) {
        oh->flags &= ~H5O_HDR_CHUNK0_SIZE;
        oh->flags |= new_size_flags;
    } /* end if */

    /* If we can extend an existing null message, take care of that */
    if(extend_msg >= 0) {
        /* Adjust message size of existing null message */
        oh->mesg[extend_msg].dirty = TRUE;
        oh->mesg[extend_msg].raw_size += delta;
    } /* end if */
    /* Create new null message for end of chunk */
    else {
        /* Create a new null message */
        if(oh->nmesgs >= oh->alloc_nmesgs)
            if(H5O_alloc_msgs(oh, (size_t)1) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate more space for messages")

        /* Set extension message */
        extend_msg = oh->nmesgs++;

        /* Initialize new null message */
        oh->mesg[extend_msg].type = H5O_MSG_NULL;
        oh->mesg[extend_msg].dirty = TRUE;
        oh->mesg[extend_msg].native = NULL;
        oh->mesg[extend_msg].raw = ((oh->chunk[chunkno].image + oh->chunk[chunkno].size)
                - (H5O_SIZEOF_CHKSUM_OH(oh) + oh->chunk[chunkno].gap))
                + H5O_SIZEOF_MSGHDR_OH(oh);
        oh->mesg[extend_msg].raw_size = (delta + oh->chunk[chunkno].gap) - H5O_SIZEOF_MSGHDR_OH(oh);
        oh->mesg[extend_msg].chunkno = chunkno;
    } /* end else */

    /* Allocate more memory space for chunk's image */
    old_image = oh->chunk[chunkno].image;
    old_size = oh->chunk[chunkno].size;
    oh->chunk[chunkno].size += delta + extra_prfx_size;
    oh->chunk[chunkno].image = H5FL_BLK_REALLOC(chunk_image, old_image, oh->chunk[chunkno].size);
    oh->chunk[chunkno].gap = 0;
    oh->chunk[chunkno].dirty = TRUE;
    if(NULL == oh->chunk[chunkno].image)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Wipe new space for chunk */
    HDmemset(oh->chunk[chunkno].image + old_size, 0, oh->chunk[chunkno].size - old_size);

    /* Spin through existing messages, adjusting them */
    for(u = 0; u < oh->nmesgs; u++) {
        /* Adjust raw addresses for messages in this chunk to reflect new 'image' address */
        if(oh->mesg[u].chunkno == chunkno)
            oh->mesg[u].raw = oh->chunk[chunkno].image + extra_prfx_size + (oh->mesg[u].raw - old_image);

        /* Find continuation message which points to this chunk and adjust chunk's size */
        /* (Chunk 0 doesn't have a continuation message that points to it and
         * it's size is directly encoded in the object header) */
        if(chunkno > 0 && (H5O_CONT_ID == oh->mesg[u].type->id) &&
                (((H5O_cont_t *)(oh->mesg[u].native))->chunkno == chunkno)) {
            /* Adjust size of continuation message */
            HDassert(((H5O_cont_t *)(oh->mesg[u].native))->size == old_size);
            ((H5O_cont_t *)(oh->mesg[u].native))->size = oh->chunk[chunkno].size;

            /* Flag continuation message as dirty */
            oh->mesg[u].dirty = TRUE;
        } /* end if */
    } /* end for */

    /* Set return value */
    *msg_idx = extend_msg;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc_extend_chunk() */


/*-------------------------------------------------------------------------
 * Function:    H5O_alloc_new_chunk
 *
 * Purpose:     Allocates a new chunk for the object header, including
 *		file space.
 *
 *              One of the other chunks will get an object continuation
 *		message.  If there isn't room in any other chunk for the
 *		object continuation message, then some message from
 *		another chunk is moved into this chunk to make room.
 *
 *              SIZE need not be aligned.
 *
 * Note:	The algorithm for finding a message to replace with a
 *		continuation message is still fairly limited.  It's possible
 *		that two (or more) messages smaller than a continuation message
 *		might occupy a chunk and need to be moved in order to make
 *		room for the continuation message.
 *
 *		Also, we aren't checking for NULL messages in front of another
 *		message right now...
 *
 * Return:      Success:        Index number of the null message for the
 *                              new chunk.  The null message will be at
 *                              least SIZE bytes not counting the message
 *                              ID or size fields.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  7 1997
 *
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_alloc_new_chunk(H5F_t *f,
                    hid_t dxpl_id,
                    H5O_t *oh,
                    size_t size)
{
    /* Struct for storing information about "best" messages to allocate from */
    typedef struct {
        int msgno;                      /* Index in message array */
        size_t gap_size;                /* Size of any "gap" in the chunk immediately after message */
        size_t null_size;               /* Size of any null message in the chunk immediately after message */
        size_t total_size;              /* Total size of "available" space around message */
        unsigned null_msgno;            /* Message index of null message immediately after message */
    } alloc_info;

    H5O_mesg_t *curr_msg;               /* Pointer to current message to operate on */
    size_t      cont_size;              /*continuation message size     */
    int         found_null = (-1);      /* Best fit null message         */
    alloc_info  found_attr = {-1, 0, 0, 0, 0};      /* Best fit attribute message    */
    alloc_info  found_other = {-1, 0, 0, 0, 0};     /* Best fit other message        */
    unsigned    idx;                    /*message number */
    uint8_t     *p = NULL;              /*ptr into new chunk            */
    H5O_cont_t  *cont = NULL;           /*native continuation message   */
    unsigned    chunkno;                /* Chunk allocated */
    haddr_t	new_chunk_addr;
    unsigned    u;                      /* Local index variable */
    unsigned    ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_alloc_new_chunk)

    /* check args */
    HDassert(oh);
    HDassert(size > 0);
    size = H5O_ALIGN_OH(oh, size);

    /*
     * Find the smallest null message that will hold an object
     * continuation message.  Failing that, find the smallest message
     * that could be moved to make room for the continuation message.
     *
     * Don't ever move continuation message from one chunk to another.
     *
     * Avoid moving attributes when possible to preserve their
     * ordering (although ordering is *not* guaranteed!).
     *
     */
    cont_size = H5O_ALIGN_OH(oh, H5F_SIZEOF_ADDR(f) + H5F_SIZEOF_SIZE(f));
    for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
        if(curr_msg->type->id == H5O_NULL_ID) {
            if(cont_size == curr_msg->raw_size) {
                found_null = u;
                break;
            }  /* end if */
            else if(curr_msg->raw_size > cont_size &&
                    (found_null < 0 || curr_msg->raw_size < oh->mesg[found_null].raw_size))
                found_null = u;
        } /* end if */
        else if(curr_msg->type->id == H5O_CONT_ID) {
            /* Don't consider continuation messages (for now) */
        } /* end if */
        else {
            unsigned msg_chunkno = curr_msg->chunkno;         /* Chunk that the message is in */
            uint8_t *end_chunk_data = (oh->chunk[msg_chunkno].image + oh->chunk[msg_chunkno].size) - (H5O_SIZEOF_CHKSUM_OH(oh) + oh->chunk[msg_chunkno].gap);     /* End of message data in chunk */
            uint8_t *end_msg = curr_msg->raw + curr_msg->raw_size;  /* End of current message */
            size_t gap_size = 0;            /* Size of gap after current message */
            size_t null_size = 0;           /* Size of NULL message after current message */
            unsigned null_msgno;            /* Index of NULL message after current message */
            size_t total_size;              /* Total size of available space "around" current message */

            /* Check if the message is the last one in the chunk */
            if(end_msg == end_chunk_data)
                gap_size = oh->chunk[msg_chunkno].gap;
            else {
                H5O_mesg_t *tmp_msg;    /* Temp. pointer to message to operate on */
                unsigned v;             /* Local index variable */

                /* Check for null message after this message, in same chunk */
                for(v = 0, tmp_msg = &oh->mesg[0]; v < oh->nmesgs; v++, tmp_msg++) {
                    if(tmp_msg->type->id == H5O_NULL_ID && (tmp_msg->raw - H5O_SIZEOF_MSGHDR_OH(oh)) == end_msg) {
                        null_msgno = v;
                        null_size = H5O_SIZEOF_MSGHDR_OH(oh) + tmp_msg->raw_size;
                        break;
                    } /* end if */

                    /* XXX: Should also check for NULL message in front of current message... */

                } /* end for */
            } /* end else */

            /* Add up current message's total available space */
            total_size = curr_msg->raw_size + gap_size + null_size;

            /* Check if message is large enough to hold continuation info */
            if(total_size >= cont_size) {
                if(curr_msg->type->id == H5O_ATTR_ID) {
                    if(found_attr.msgno < 0 || total_size < found_attr.total_size) {
                        found_attr.msgno = u;
                        found_attr.gap_size = gap_size;
                        found_attr.null_size = null_size;
                        found_attr.total_size = total_size;
                        found_attr.null_msgno = null_msgno;
                    } /* end if */
                } /* end if */
                else {
                    if(found_other.msgno < 0 || total_size < found_other.total_size) {
                        found_other.msgno = u;
                        found_other.gap_size = gap_size;
                        found_other.null_size = null_size;
                        found_other.total_size = total_size;
                        found_other.null_msgno = null_msgno;
                    } /* end if */
                } /* end else */
            } /* end if */
        } /* end else */
    } /* end for */
    if(found_null < 0 && found_attr.msgno < 0 && found_other.msgno < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "unable to locate message to move")

    /*
     * If we must move some other message to make room for the null
     * message, then make sure the new chunk has enough room for that
     * other message.
     *
     * Move other messages first, and attributes only as a last resort.
     *
     */
    if(found_null < 0) {
        if(found_other.msgno < 0)
            found_other = found_attr;

        HDassert(found_other.msgno >= 0);
        size += H5O_SIZEOF_MSGHDR_OH(oh) + oh->mesg[found_other.msgno].raw_size;
    } /* end if */

    /*
     * The total chunk size must include the requested space plus enough
     * for the message header.  This must be at least some minimum and
     * aligned propertly.
     */
    size = MAX(H5O_MIN_SIZE, size + H5O_SIZEOF_MSGHDR_OH(oh));
    HDassert(size == H5O_ALIGN_OH(oh, size));

    /*
     * The total chunk size must include enough space for the checksum
     * on the chunk and the continuation chunk magic #. (which are only present
     * in later versions of the object header)
     */
    size +=  H5O_SIZEOF_CHKHDR_OH(oh);

    /* allocate space in file to hold the new chunk */
    new_chunk_addr = H5MF_alloc(f, H5FD_MEM_OHDR, dxpl_id, (hsize_t)size);
    if(HADDR_UNDEF == new_chunk_addr)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "unable to allocate space for new chunk")

    /*
     * Create the new chunk giving it a file address.
     */
    if(oh->nchunks >= oh->alloc_nchunks) {
        unsigned na = MAX(H5O_NCHUNKS, oh->alloc_nchunks * 2);        /* Double # of chunks allocated */
        H5O_chunk_t *x = H5FL_SEQ_REALLOC(H5O_chunk_t, oh->chunk, (size_t)na);

        if(!x)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed")
        oh->alloc_nchunks = na;
        oh->chunk = x;
    } /* end if */

    chunkno = oh->nchunks++;
    oh->chunk[chunkno].dirty = TRUE;
    oh->chunk[chunkno].addr = new_chunk_addr;
    oh->chunk[chunkno].size = size;
    oh->chunk[chunkno].gap = 0;
    if(NULL == (oh->chunk[chunkno].image = p = H5FL_BLK_CALLOC(chunk_image, size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed")

    /* If this is a later version of the object header format, put the magic
     *  # at the beginning of the chunk image.
     */
    if(oh->version > H5O_VERSION_1) {
        HDmemcpy(p, H5O_CHK_MAGIC, (size_t)H5O_SIZEOF_MAGIC);
        p += H5O_SIZEOF_MAGIC;
    } /* end if */

    /*
     * Make sure we have enough space for all possible new messages
     * that could be generated below.
     */
    if(oh->nmesgs + 3 > oh->alloc_nmesgs)
        if(H5O_alloc_msgs(oh, (size_t)3) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "can't allocate more space for messages")

    /* Move message (that will be replaced with continuation message) 
     *  to new chunk, if necessary.
     */
    if(found_null < 0) {
        H5O_mesg_t *null_msg;       /* Pointer to new null message */

        /* Create null message for space that message to copy currently occupies */
        found_null = oh->nmesgs++;
        null_msg = &(oh->mesg[found_null]);
        null_msg->type = H5O_MSG_NULL;
        null_msg->dirty = TRUE;
        null_msg->native = NULL;
        null_msg->raw = oh->mesg[found_other.msgno].raw;
        null_msg->raw_size = oh->mesg[found_other.msgno].raw_size;
        null_msg->chunkno = oh->mesg[found_other.msgno].chunkno;

        /* Copy the message to move (& its prefix) to its new location */
        /* (Chunk is already dirty, no need to mark it) */
        HDmemcpy(p, oh->mesg[found_other.msgno].raw - H5O_SIZEOF_MSGHDR_OH(oh),
                 oh->mesg[found_other.msgno].raw_size + H5O_SIZEOF_MSGHDR_OH(oh));

        /* Switch moved message to point to new location */
        oh->mesg[found_other.msgno].raw = p + H5O_SIZEOF_MSGHDR_OH(oh);
        oh->mesg[found_other.msgno].chunkno = chunkno;

        /* Account for copied message in new chunk */
        p += H5O_SIZEOF_MSGHDR_OH(oh) + oh->mesg[found_other.msgno].raw_size;
        size -= H5O_SIZEOF_MSGHDR_OH(oh) + oh->mesg[found_other.msgno].raw_size;

        /* Add any available space after the message to move to the new null message */
        if(found_other.gap_size > 0) {
            /* Absorb a gap after the moved message */
            HDassert(oh->chunk[null_msg->chunkno].gap == found_other.gap_size);
            null_msg->raw_size += found_other.gap_size;
            oh->chunk[null_msg->chunkno].gap = 0;
        } /* end if */
        else if(found_other.null_size > 0) {
            H5O_mesg_t *old_null_msg = &oh->mesg[found_other.null_msgno]; /* Pointer to NULL message to eliminate */

            /* Absorb a null message after the moved message */
            HDassert((null_msg->raw + null_msg->raw_size) == (old_null_msg->raw - H5O_SIZEOF_MSGHDR_OH(oh)));
            null_msg->raw_size += found_other.null_size;

            /* Release any information/memory for message */
            H5O_msg_free_mesg(old_null_msg);

            /* Remove null message from list of messages */
            if(found_other.null_msgno < (oh->nmesgs - 1))
                HDmemmove(old_null_msg, old_null_msg + 1, ((oh->nmesgs - 1) - found_other.null_msgno) * sizeof(H5O_mesg_t));

            /* Decrement # of messages */
            /* (Don't bother reducing size of message array for now -QAK) */
            oh->nmesgs--;

            /* Adjust message index for new NULL message */
            found_null--;
        } /* end if */
    } /* end if */
    HDassert(found_null >= 0);

    /* Create null message for [rest of] space in new chunk */
    /* (account for chunk's magic # & checksum) */
    idx = oh->nmesgs++;
    oh->mesg[idx].type = H5O_MSG_NULL;
    oh->mesg[idx].dirty = TRUE;
    oh->mesg[idx].native = NULL;
    oh->mesg[idx].raw = p + H5O_SIZEOF_MSGHDR_OH(oh);
    oh->mesg[idx].raw_size = size - (H5O_SIZEOF_CHKHDR_OH(oh) + H5O_SIZEOF_MSGHDR_OH(oh));
    oh->mesg[idx].chunkno = chunkno;

    /* Initialize the continuation information */
    if(NULL == (cont = H5FL_MALLOC(H5O_cont_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed")
    cont->addr = oh->chunk[chunkno].addr;
    cont->size = oh->chunk[chunkno].size;
    cont->chunkno = chunkno;

    /* Split the null message and point at continuation message */
    if(H5O_alloc_null(oh, (unsigned)found_null, H5O_MSG_CONT, cont, cont_size) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, UFAIL, "can't split null message")

    /* Set return value */
    ret_value = idx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc_new_chunk() */


/*-------------------------------------------------------------------------
 * Function:    H5O_alloc
 *
 * Purpose:     Allocate enough space in the object header for this message.
 *
 * Return:      Success:        Index of message
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5O_alloc(H5F_t *f, hid_t dxpl_id, H5O_t *oh, const H5O_msg_class_t *type,
    const void *mesg)
{
    size_t      raw_size;       /* Raw size of message */
    size_t      aligned_size;   /* Size of message including alignment */
    unsigned    idx;            /* Index of message which fits allocation */
    unsigned    ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_alloc, UFAIL)

    /* check args */
    HDassert(oh);
    HDassert(type);
    HDassert(mesg);

    /* Compute the size needed to store the message in the object header */
    if((raw_size = (type->raw_size)(f, FALSE, mesg)) >= H5O_MESG_MAX_SIZE)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, UFAIL, "object header message is too large")
    aligned_size = H5O_ALIGN_OH(oh, raw_size);

    /* look for a null message which is large enough */
    for(idx = 0; idx < oh->nmesgs; idx++)
        if(H5O_NULL_ID == oh->mesg[idx].type->id && oh->mesg[idx].raw_size >= aligned_size)
            break;

    /* if we didn't find one, then allocate more header space */
    if(idx >= oh->nmesgs) {
        unsigned        chunkno;

        /* check to see if we can extend one of the chunks.  If we can,
         * do so.  Otherwise, we will have to allocate a new chunk.
         *
         * Note that in this new version of this function, all chunks
         * must have file space allocated to them.
         */
        for(chunkno = 0; chunkno < oh->nchunks; chunkno++) {
            htri_t	tri_result;

            HDassert(H5F_addr_defined(oh->chunk[chunkno].addr));

            tri_result = H5O_alloc_extend_chunk(f, oh, chunkno, raw_size, &idx);
            if(tri_result == TRUE)
		break;
            else if(tri_result == FALSE)
                idx = UFAIL;
            else
                HGOTO_ERROR(H5E_OHDR, H5E_SYSTEM, UFAIL, "H5O_alloc_extend_chunk failed unexpectedly")
        } /* end for */

        /* If idx is still UFAIL, we were not able to extend a chunk,
         *      create a new one.
         */
        if(idx == UFAIL)
            if((idx = H5O_alloc_new_chunk(f, dxpl_id, oh, raw_size)) == UFAIL)
                HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, UFAIL, "unable to create a new object header data chunk")
    } /* end if */

    /* Split the null message and point at continuation message */
    if(H5O_alloc_null(oh, idx, type, NULL, aligned_size) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, UFAIL, "can't split null message")

    /* Mark object header as dirty in cache */
    if(H5AC_mark_pinned_or_protected_entry_dirty(f, oh) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTMARKDIRTY, UFAIL, "unable to mark object header as dirty")

    /* Set return value */
    ret_value = idx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_release_mesg
 *
 * Purpose:     Convert a message into a null message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 22 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_release_mesg(H5F_t *f, hid_t dxpl_id, H5O_t *oh, H5O_mesg_t *mesg,
    hbool_t adj_link)
{
    herr_t ret_value = SUCCEED; 	                /* Return value */

    FUNC_ENTER_NOAPI(H5O_release_mesg, FAIL)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(mesg);

    /* Check if we should operate on the message */
    if(adj_link) {
        /* Free any space referred to in the file from this message */
        if(H5O_delete_mesg(f, dxpl_id, oh, mesg) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to delete file space for object header message")
    } /* end if */

    /* Free any native information */
    H5O_msg_free_mesg(mesg);

    /* Change message type to nil and zero it */
    mesg->type = H5O_MSG_NULL;
    HDassert(mesg->raw + mesg->raw_size <= (oh->chunk[mesg->chunkno].image + oh->chunk[mesg->chunkno].size) - (H5O_SIZEOF_CHKSUM_OH(oh) + oh->chunk[mesg->chunkno].gap));
    HDmemset(mesg->raw, 0, mesg->raw_size);

    /* Clear message flags */
    mesg->flags = 0;

    /* Indicate that the message was modified */
    mesg->dirty = TRUE;

    /* Check if chunk has a gap currently */
    if(oh->chunk[mesg->chunkno].gap) {
        /* Eliminate the gap in the chunk */
        if(H5O_eliminate_gap(oh, mesg, 
                ((oh->chunk[mesg->chunkno].image + oh->chunk[mesg->chunkno].size) - (H5O_SIZEOF_CHKSUM_OH(oh) + oh->chunk[mesg->chunkno].gap)),
                oh->chunk[mesg->chunkno].gap) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTREMOVE, FAIL, "can't eliminate gap in chunk")

        /* Set the gap size to zero for the chunk */
        oh->chunk[mesg->chunkno].gap = 0;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_release_mesg() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_move_msgs_forward
 *
 * Purpose:     Move messages toward first chunk
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct 17 2005
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_move_msgs_forward(H5O_t *oh)
{
    hbool_t packed_msg;                 /* Flag to indicate that messages were packed */
    hbool_t did_packing = FALSE;        /* Whether any messages were packed */
    htri_t ret_value; 	                /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_move_msgs_forward)

    /* check args */
    HDassert(oh);

    /* Loop until no messages packed */
    /* (Double loop is not very efficient, but it would be some extra work to
     *      add a list of messages to each chunk -QAK)
     */
    do {
        H5O_mesg_t *curr_msg;       /* Pointer to current message to operate on */
        unsigned	u;              /* Local index variable */

        /* Reset packed messages flag */
        packed_msg = FALSE;

        /* Scan through messages for messages that can be moved earlier in chunks */
        for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
            if(H5O_NULL_ID == curr_msg->type->id) {
                H5O_chunk_t *chunk;     /* Pointer to chunk that null message is in */

                /* Check if null message is not last in chunk */
                chunk = &(oh->chunk[curr_msg->chunkno]);
                if((curr_msg->raw + curr_msg->raw_size) 
                        != ((chunk->image + chunk->size) - (H5O_SIZEOF_CHKSUM_OH(oh) + chunk->gap))) {
                    H5O_mesg_t *nonnull_msg;       /* Pointer to current message to operate on */
                    unsigned	v;              /* Local index variable */

                    /* Loop over messages again, looking for the message in the chunk after the null message */
                    for(v = 0, nonnull_msg = &oh->mesg[0]; v < oh->nmesgs; v++, nonnull_msg++) {
                        /* Locate message that is immediately after the null message */
                        if((curr_msg->chunkno == nonnull_msg->chunkno) &&
                                ((curr_msg->raw + curr_msg->raw_size) == (nonnull_msg->raw - H5O_SIZEOF_MSGHDR_OH(oh)))) {
                            /* Don't swap messages if the second message is also a null message */
                            /* (We'll merge them together later, in another routine) */
                            if(H5O_NULL_ID != nonnull_msg->type->id) {
                                /* Copy raw data for non-null message to new location */
                                HDmemmove(curr_msg->raw - H5O_SIZEOF_MSGHDR_OH(oh),
                                    nonnull_msg->raw - H5O_SIZEOF_MSGHDR_OH(oh), nonnull_msg->raw_size + H5O_SIZEOF_MSGHDR_OH(oh));

                                /* Adjust non-null message's offset in chunk */
                                nonnull_msg->raw = curr_msg->raw;

                                /* Adjust null message's offset in chunk */
                                curr_msg->raw = nonnull_msg->raw +
                                        nonnull_msg->raw_size + H5O_SIZEOF_MSGHDR_OH(oh);

                                /* Mark null message dirty */
                                /* (since we need to re-encode its message header) */
                                /* (also, marking this message dirty means we
                                 *  don't have to mark chunk as dirty)
                                 */
                                curr_msg->dirty = TRUE;

                                /* Set the flag to indicate that the null message
                                 * was packed - if its not at the end its chunk,
                                 * we'll move it again on the next pass.
                                 */
                                packed_msg = TRUE;
                            } /* end if */

                            /* Break out of loop */
                            break;
                        } /* end if */
                    } /* end for */
                    /* Should have been message after null message */
                    HDassert(v < oh->nmesgs);
                } /* end if */
            } /* end if */
            else {
                H5O_mesg_t *null_msg;       /* Pointer to current message to operate on */
                unsigned	v;              /* Local index variable */

                /* Loop over messages again, looking for large enough null message in earlier chunk */
                for(v = 0, null_msg = &oh->mesg[0]; v < oh->nmesgs; v++, null_msg++) {
                    if(H5O_NULL_ID == null_msg->type->id && curr_msg->chunkno > null_msg->chunkno
                            && curr_msg->raw_size <= null_msg->raw_size) {
                        unsigned old_chunkno;   /* Old message information */
                        uint8_t *old_raw;

                        /* Keep old information about non-null message */
                        old_chunkno = curr_msg->chunkno;
                        old_raw = curr_msg->raw;

                        /* Copy raw data for non-null message to new chunk */
                        HDmemcpy(null_msg->raw - H5O_SIZEOF_MSGHDR_OH(oh), curr_msg->raw - H5O_SIZEOF_MSGHDR_OH(oh), curr_msg->raw_size + H5O_SIZEOF_MSGHDR_OH(oh));

                        /* Mark null message's chunk as dirty, since the raw data image changed */
                        oh->chunk[null_msg->chunkno].dirty = TRUE;

                        /* Point non-null message at null message's space */
                        curr_msg->chunkno = null_msg->chunkno;
                        curr_msg->raw = null_msg->raw;

                        /* Change information for null message */
                        if(curr_msg->raw_size == null_msg->raw_size) {
                            /* Point null message at old non-null space */
                            /* (Instead of freeing it and allocating new message) */
                            null_msg->chunkno = old_chunkno;
                            null_msg->raw = old_raw;

                            /* Mark null message dirty */
                            null_msg->dirty = TRUE;
                        } /* end if */
                        else {
                            unsigned new_null_msg;          /* Message index for new null message */

                            /* Check if null message is large enough to still exist */
                            if((null_msg->raw_size - curr_msg->raw_size) < (size_t)H5O_SIZEOF_MSGHDR_OH(oh)) {
                                size_t gap_size = null_msg->raw_size - curr_msg->raw_size;     /* Size of gap produced */

                                /* Adjust the size of the null message being eliminated */
                                null_msg->raw_size = curr_msg->raw_size;

                                /* Add the gap to the chunk */
                                if(H5O_add_gap(oh, null_msg->chunkno, v, null_msg->raw + null_msg->raw_size, gap_size) < 0)
                                    HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, FAIL, "can't insert gap in chunk")

                                /* Re-use message # for new null message taking place of non-null message */
                                new_null_msg = v;
                            } /* end if */
                            else {
                                /* Adjust null message's size & offset */
                                null_msg->raw += curr_msg->raw_size + H5O_SIZEOF_MSGHDR_OH(oh);
                                null_msg->raw_size -= curr_msg->raw_size + H5O_SIZEOF_MSGHDR_OH(oh);

                                /* Mark null message dirty */
                                null_msg->dirty = TRUE;

                                /* Create new null message for previous location of non-null message */
                                if(oh->nmesgs >= oh->alloc_nmesgs) {
                                    if(H5O_alloc_msgs(oh, (size_t)1) < 0)
                                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate more space for messages")

                                    /* "Retarget" 'curr_msg' pointer into newly re-allocated array of messages */
                                    curr_msg = &oh->mesg[u];
                                } /* end if */

                                /* Get message # for new null message */
                                new_null_msg = oh->nmesgs++;
                            } /* end else */

                            /* Initialize new null message to take over non-null message's location */
                            oh->mesg[new_null_msg].type = H5O_MSG_NULL;
                            oh->mesg[new_null_msg].dirty = TRUE;
                            oh->mesg[new_null_msg].native = NULL;
                            oh->mesg[new_null_msg].raw = old_raw;
                            oh->mesg[new_null_msg].raw_size = curr_msg->raw_size;
                            oh->mesg[new_null_msg].chunkno = old_chunkno;

                            /* Check for gap in new null message's chunk */
                            if(oh->chunk[old_chunkno].gap > 0) {
                                /* Eliminate the gap in the chunk */
                                if(H5O_eliminate_gap(oh, &oh->mesg[new_null_msg], 
                                        ((oh->chunk[old_chunkno].image + oh->chunk[old_chunkno].size) - (H5O_SIZEOF_CHKSUM_OH(oh) + oh->chunk[old_chunkno].gap)),
                                        oh->chunk[old_chunkno].gap) < 0)
                                    HGOTO_ERROR(H5E_OHDR, H5E_CANTREMOVE, FAIL, "can't eliminate gap in chunk")

                                /* Set the gap size to zero for the chunk */
                                oh->chunk[old_chunkno].gap = 0;
                            } /* end if */
                        } /* end else */

                        /* Indicate that we packed messages */
                        packed_msg = TRUE;

                        /* Break out of loop */
                        /* (If it's possible to move message to even earlier chunk
                         *      we'll get it on the next pass - QAK)
                         */
                        break;
                    } /* end if */
                } /* end for */

                /* If we packed messages, get out of loop and start over */
                /* (Don't know if this has any benefit one way or the other -QAK) */
                if(packed_msg)
                    break;
            } /* end else */
        } /* end for */

        /* If we did any packing, remember that */
        if(packed_msg)
            did_packing = TRUE;
    } while(packed_msg);

    /* Set return value */
    ret_value = did_packing;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_move_msgs_forward() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_merge_null
 *
 * Purpose:     Merge neighboring null messages in an object header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct 10 2005
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_merge_null(H5O_t *oh)
{
    hbool_t merged_msg;                 /* Flag to indicate that messages were merged */
    hbool_t did_merging = FALSE;        /* Whether any messages were merged */
    htri_t ret_value; 	                /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_merge_null)

    /* check args */
    HDassert(oh != NULL);

    /* Loop until no messages merged */
    /* (Double loop is not very efficient, but it would be some extra work to add
     *      a list of messages to each chunk -QAK)
     */
    do {
        H5O_mesg_t *curr_msg;       /* Pointer to current message to operate on */
        unsigned	u;              /* Local index variable */

        /* Reset merged messages flag */
        merged_msg = FALSE;

        /* Scan messages for adjacent null messages & merge them */
        for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
            if(H5O_NULL_ID == curr_msg->type->id) {
                H5O_mesg_t *curr_msg2;       /* Pointer to current message to operate on */
                unsigned	v;              /* Local index variable */

                /* Should be no gaps in chunk with null message */
                HDassert(oh->chunk[curr_msg->chunkno].gap == 0);

                /* Loop over messages again, looking for null message in same chunk */
                for(v = 0, curr_msg2 = &oh->mesg[0]; v < oh->nmesgs; v++, curr_msg2++) {
                    if(u != v && H5O_NULL_ID == curr_msg2->type->id && curr_msg->chunkno == curr_msg2->chunkno) {

                        /* Check for second message after first message */
                        if((curr_msg->raw + curr_msg->raw_size) == (curr_msg2->raw - H5O_SIZEOF_MSGHDR_OH(oh))) {
                            /* Extend first null message length to cover second null message */
                            curr_msg->raw_size += (H5O_SIZEOF_MSGHDR_OH(oh) + curr_msg2->raw_size);

                            /* Message has been merged */
                            merged_msg = TRUE;
                        } /* end if */
                        /* Check for second message before first message */
                        else if((curr_msg->raw - H5O_SIZEOF_MSGHDR_OH(oh)) == (curr_msg2->raw + curr_msg2->raw_size)) {
                            /* Adjust first message address and extend length to cover second message */
                            curr_msg->raw -= (H5O_SIZEOF_MSGHDR_OH(oh) + curr_msg2->raw_size);
                            curr_msg->raw_size += (H5O_SIZEOF_MSGHDR_OH(oh) + curr_msg2->raw_size);

                            /* Message has been merged */
                            merged_msg = TRUE;
                        } /* end if */

                        /* Second message has been merged, delete it */
                        if(merged_msg) {
                            /* Release any information/memory for second message */
                            H5O_msg_free_mesg(curr_msg2);

                            /* Mark first message as dirty */
                            curr_msg->dirty = TRUE;

                            /* Remove second message from list of messages */
                            if(v < (oh->nmesgs - 1))
                                HDmemmove(&oh->mesg[v], &oh->mesg[v + 1], ((oh->nmesgs - 1) - v) * sizeof(H5O_mesg_t));

                            /* Decrement # of messages */
                            /* (Don't bother reducing size of message array for now -QAK) */
                            oh->nmesgs--;

                            /* Get out of loop */
                            break;
                        } /* end if */
                    } /* end if */
                } /* end for */

                /* Get out of loop if we merged messages */
                if(merged_msg)
                    break;
            } /* end if */
        } /* end for */

        /* If we did any merging, remember that */
        if(merged_msg)
            did_merging = TRUE;
    } while(merged_msg);

    /* Set return value */
    ret_value = did_merging;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_merge_null() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_remove_empty_chunks
 *
 * Purpose:     Attempt to eliminate empty chunks from object header.
 *
 *              This examines a chunk to see if it's empty
 *              and removes it (and the continuation message that points to it)
 *              from the object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct 17 2005
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_remove_empty_chunks(H5F_t *f, H5O_t *oh, hid_t dxpl_id)
{
    hbool_t deleted_chunk;              /* Whether to a chunk was deleted */
    hbool_t did_deleting = FALSE;       /* Whether any chunks were deleted */
    htri_t ret_value; 	                /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_remove_empty_chunks)

    /* check args */
    HDassert(oh != NULL);

    /* Loop until no chunks are freed */
    do {
        H5O_mesg_t *null_msg;       /* Pointer to null message found */
        H5O_mesg_t *cont_msg;       /* Pointer to continuation message found */
        unsigned	u, v;       /* Local index variables */

        /* Reset 'chunk deleted' flag */
        deleted_chunk = FALSE;

        /* Scan messages for null messages that fill an entire chunk */
        for(u = 0, null_msg = &oh->mesg[0]; u < oh->nmesgs; u++, null_msg++) {
            /* If a null message takes up an entire object header chunk (and
             * its not the "base" chunk), delete that chunk from object header
             */
            if(H5O_NULL_ID == null_msg->type->id && null_msg->chunkno > 0 &&
                    (H5O_SIZEOF_MSGHDR_OH(oh) + null_msg->raw_size)
                         == (oh->chunk[null_msg->chunkno].size - H5O_SIZEOF_CHKHDR_OH(oh))) {
                H5O_mesg_t *curr_msg;           /* Pointer to current message to operate on */
                unsigned null_msg_no;           /* Message # for null message */
                unsigned deleted_chunkno;       /* Chunk # to delete */

                /* Locate continuation message that points to chunk */
                for(v = 0, cont_msg = &oh->mesg[0]; v < oh->nmesgs; v++, cont_msg++) {
                    if(H5O_CONT_ID == cont_msg->type->id) {
                        /* Decode current continuation message if necessary */
                        if(NULL == cont_msg->native) {
                            HDassert(H5O_MSG_CONT->decode);
                            cont_msg->native = (H5O_MSG_CONT->decode)(f, dxpl_id, 0, cont_msg->raw);
                            if(NULL == cont_msg->native)
                                HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "unable to decode message")
                        } /* end if */

                        /* Check for correct chunk to delete */
                        if(oh->chunk[null_msg->chunkno].addr == ((H5O_cont_t *)(cont_msg->native))->addr)
                            break;
                    } /* end if */
                } /* end for */
                /* Must be a continuation message that points to chunk containing null message */
                HDassert(v < oh->nmesgs);
                HDassert(cont_msg);

                /* Initialize information about null message */
                null_msg_no = u;
                deleted_chunkno = null_msg->chunkno;

                /* Convert continuation message into a null message */
                if(H5O_release_mesg(f, dxpl_id, oh, cont_msg, TRUE) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to convert into null message")

                /*
                 * Remove chunk from object header's data structure
                 */

                /* Free memory for chunk image */
                H5FL_BLK_FREE(chunk_image, oh->chunk[null_msg->chunkno].image);

                /* Remove chunk from list of chunks */
                if(null_msg->chunkno < (oh->nchunks - 1))
                    HDmemmove(&oh->chunk[null_msg->chunkno], &oh->chunk[null_msg->chunkno + 1], ((oh->nchunks - 1) - null_msg->chunkno) * sizeof(H5O_chunk_t));

                /* Decrement # of chunks */
                /* (Don't bother reducing size of chunk array for now -QAK) */
                oh->nchunks--;

                /*
                 * Delete null message (in empty chunk that was be freed) from list of messages
                 */

                /* Release any information/memory for message */
                H5O_msg_free_mesg(null_msg);

                /* Remove null message from list of messages */
                if(null_msg_no < (oh->nmesgs - 1))
                    HDmemmove(&oh->mesg[null_msg_no], &oh->mesg[null_msg_no + 1], ((oh->nmesgs - 1) - null_msg_no) * sizeof(H5O_mesg_t));

                /* Decrement # of messages */
                /* (Don't bother reducing size of message array for now -QAK) */
                oh->nmesgs--;

                /* Adjust chunk # for messages in chunks after deleted chunk */
                for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
                    HDassert(curr_msg->chunkno != deleted_chunkno);
                    if(curr_msg->chunkno > deleted_chunkno)
                        curr_msg->chunkno--;

                    /* Check for continuation message */
                    if(H5O_CONT_ID == curr_msg->type->id) {
                        /* Decode current continuation message if necessary */
                        if(NULL == curr_msg->native) {
                            HDassert(H5O_MSG_CONT->decode);
                            curr_msg->native = (H5O_MSG_CONT->decode)(f, dxpl_id, 0, curr_msg->raw);
                            if(NULL == curr_msg->native)
                                HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "unable to decode message")
                        } /* end if */

                        /* Check for pointer to chunk after deleted chunk */
                        if(((H5O_cont_t *)(curr_msg->native))->chunkno > deleted_chunkno)
                            ((H5O_cont_t *)(curr_msg->native))->chunkno--;
                    } /* end if */
                } /* end for */

                /* Found chunk to delete */
                deleted_chunk = TRUE;
                break;
            } /* end if */
        } /* end for */

        /* If we deleted any chunks, remember that */
        if(deleted_chunk)
            did_deleting = TRUE;
    } while(deleted_chunk);

    /* Set return value */
    ret_value = did_deleting;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_remove_empty_chunks() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_condense_header
 *
 * Purpose:     Attempt to eliminate empty chunks from object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct  4 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_condense_header(H5F_t *f, H5O_t *oh, hid_t dxpl_id)
{
    hbool_t rescan_header;              /* Whether to rescan header */
    htri_t result;                      /* Result from packing/merging/etc */
    herr_t ret_value = SUCCEED; 	/* return value */

    FUNC_ENTER_NOAPI(H5O_condense_header, FAIL)

    /* check args */
    HDassert(oh != NULL);

    /* Loop until no changed to the object header messages & chunks */
    do {
        /* Reset 'rescan chunks' flag */
        rescan_header = FALSE;

        /* Scan for messages that can be moved earlier in chunks */
        result = H5O_move_msgs_forward(oh);
        if(result < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTPACK, FAIL, "can't move header messages forward")
        if(result > 0)
            rescan_header = TRUE;

        /* Scan for adjacent null messages & merge them */
        result = H5O_merge_null(oh);
        if(result < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTPACK, FAIL, "can't pack null header messages")
        if(result > 0)
            rescan_header = TRUE;

        /* Scan for empty chunks to remove */
        result = H5O_remove_empty_chunks(f, oh, dxpl_id);
        if(result < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTPACK, FAIL, "can't remove empty chunk")
        if(result > 0)
            rescan_header = TRUE;
    } while(rescan_header);
#ifdef H5O_DEBUG
H5O_assert(oh);
#endif /* H5O_DEBUG */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_condense_header() */

