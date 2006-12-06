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
 * Created:		H5Omessage.c
 *			Dec  3 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Object header message routines.
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
#include "H5Aprivate.h"		/* Attributes	  			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5SMprivate.h"        /* Shared object header messages        */


/****************/
/* Local Macros */
/****************/

/* Load native information for a message, if it's not already present */
/* (Only works for messages with decode callback) */
#define H5O_LOAD_NATIVE(F, DXPL, MSG, ERR)                                        \
    if(NULL == (MSG)->native) {                                               \
        const H5O_msg_class_t	*decode_type;                                 \
                                                                              \
        /* Check for shared message */                                        \
        if((MSG)->flags & H5O_MSG_FLAG_SHARED)                                \
            decode_type = H5O_MSG_SHARED;                                     \
        else                                                                  \
            decode_type = (MSG)->type;                                        \
                                                                              \
        /* Decode the message */                                              \
        HDassert(decode_type->decode);                                        \
        if(NULL == ((MSG)->native = (decode_type->decode)((F), (DXPL), (MSG)->raw))) \
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, ERR, "unable to decode message") \
    } /* end if */


/******************/
/* Local Typedefs */
/******************/

/* User data for iteration while removing a message */
typedef struct {
    H5F_t      *f;              /* Pointer to file for insertion */
    hid_t dxpl_id;              /* DXPL during iteration */
    int sequence;               /* Sequence # to search for */
    unsigned nfailed;           /* # of failed message removals */
    H5O_operator_t op;          /* Callback routine for removal operations */
    void *op_data;              /* Callback data for removal operations */
    hbool_t adj_link;           /* Whether to adjust links when removing messages */
} H5O_iter_rm_t;

/* User data for iteration when converting attributes to dense storage */
typedef struct {
    H5F_t      *f;              /* Pointer to file for insertion */
    hid_t dxpl_id;              /* DXPL during iteration */
} H5O_iter_to_dense_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5O_msg_write_real(H5F_t *f, H5O_t *oh, const H5O_msg_class_t *type,
    unsigned overwrite, unsigned mesg_flags, unsigned update_flags,
    const void *mesg, hid_t dxpl_id, unsigned *oh_flags_ptr);
static herr_t H5O_msg_reset_real(const H5O_msg_class_t *type, void *native);
static void *H5O_msg_copy_real(const H5O_msg_class_t *type, const void *mesg,
        void *dst);
static herr_t H5O_msg_remove_real(const H5O_loc_t *loc, const H5O_msg_class_t *type,
    int sequence, H5O_operator_t op, void *op_data, hbool_t adj_link, hid_t dxpl_id);
static herr_t H5O_msg_remove_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/, 
    unsigned sequence, unsigned *oh_flags_ptr, void *_udata/*in,out*/);
static unsigned H5O_new_mesg(H5F_t *f, H5O_t *oh, unsigned *flags,
    const H5O_msg_class_t *orig_type, const void *orig_mesg, H5O_shared_t *sh_mesg,
    const H5O_msg_class_t **new_type, const void **new_mesg, hid_t dxpl_id,
    unsigned *oh_flags_ptr);
static herr_t H5O_write_mesg(H5F_t *f, hid_t dxpl_id, H5O_t *oh, unsigned idx,
    const H5O_msg_class_t *type, const void *mesg, unsigned flags,
    unsigned update_flags, unsigned *oh_flags_ptr);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5O_msg_create
 *
 * Purpose:	Create a new object header message
 *
 * Return:	Success:	The sequence number of the message that
 *				was created.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  1 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_create(H5O_loc_t *loc, unsigned type_id, unsigned mesg_flags,
    unsigned update_flags, void *mesg, hid_t dxpl_id)
{
    H5O_t *oh = NULL;                   /* Object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;     /* Metadata cache flags for object header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_create, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    HDassert(0 == (mesg_flags & ~H5O_MSG_FLAG_BITS));
    HDassert(mesg);

    /* Check for write access on the file */
    if(0 == (H5F_INTENT(loc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Protect the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Go append message to object header */
    if(H5O_msg_append(loc->file, dxpl_id, oh, type_id, mesg_flags, update_flags, mesg, &oh_flags) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to append to object header")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_create() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_attr_to_dense_cb
 *
 * Purpose:	Object header iterator callback routine to convert compact
 *              attributes to dense attributes
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  4 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_msg_attr_to_dense_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned UNUSED sequence, unsigned *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_to_dense_t *udata = (H5O_iter_to_dense_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_attr_to_dense_cb)

    /* check args */
    HDassert(oh);
    HDassert(mesg);

    /* Insert attribute into dense storage */
    if(H5A_dense_insert(udata->f, udata->dxpl_id, oh, mesg->flags, mesg->native) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, H5_ITER_ERROR, "unable to add to dense storage")

    /* Convert message into a null message */
    if(H5O_release_mesg(udata->f, udata->dxpl_id, oh, mesg, TRUE, FALSE) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, H5_ITER_ERROR, "unable to convert into null message")

    /* Indicate that the object header was modified */
    *oh_flags_ptr |= H5AC__DIRTIED_FLAG;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_attr_to_dense_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_append
 *
 * Purpose:	Simplified version of H5O_msg_create, used when creating a new
 *              object header message (usually during object creation) and
 *              several messages will be added to the object header at once.
 *
 * Return:	Success:	The sequence number of the message that
 *				was created.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_append(H5F_t *f, hid_t dxpl_id, H5O_t *oh, unsigned type_id,
    unsigned mesg_flags, unsigned update_flags, void *mesg,
    unsigned *oh_flags_ptr)
{
    const H5O_msg_class_t *new_type;    /* Actual H5O class type for the ID */
    const void *new_mesg;               /* Actual message to write */
    const H5O_msg_class_t *type;        /* Original H5O class type for the ID */
    H5O_shared_t sh_mesg;               /* Shared object header info */
    unsigned idx;                       /* Index of message to modify */
    hbool_t new_format_attr;            /* Whether this message is an attribute in a new-format header */
    htri_t shared_mesg;                 /* Should this message be stored in the Shared Message table? */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_append, FAIL)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(0 == (mesg_flags & ~H5O_MSG_FLAG_BITS));
    HDassert(mesg);
    HDassert(oh_flags_ptr);

    /* Should this message be written as a SOHM? */
    if((shared_mesg = H5SM_try_share(f, dxpl_id, type_id, mesg)) > 0)
        /* Mark the message as shared */
        mesg_flags |= H5O_MSG_FLAG_SHARED;
    else if(shared_mesg < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "error determining if message should be shared")

    /* Set the flag for an attribute in a new format header */
    if(H5O_ATTR_ID == type_id && oh->version > H5O_VERSION_1)
        new_format_attr = TRUE;
    else
        new_format_attr = FALSE;

    /* If the message added is an attribute, check for dense storage */
    if(new_format_attr) {
#ifdef QAK
HDfprintf(stderr, "%s: adding attribute to new-style object header\n", FUNC);
HDfprintf(stderr, "%s: oh->nattrs = %Hu\n", FUNC, oh->nattrs);
HDfprintf(stderr, "%s: oh->max_compact = %u\n", FUNC, oh->max_compact);
HDfprintf(stderr, "%s: oh->min_dense = %u\n", FUNC, oh->min_dense);
#endif /* QAK */
        /* Check for switching to "dense" attribute storage */
        if(oh->nattrs == oh->max_compact && !H5F_addr_defined(oh->attr_fheap_addr)) {
            H5O_iter_to_dense_t udata;          /* User data for callback */
            H5O_mesg_operator_t op;             /* Wrapper for operator */
#ifdef QAK
HDfprintf(stderr, "%s: converting attributes to dense storage\n", FUNC);
#endif /* QAK */

            /* Create dense storage for attributes */
            if(H5A_dense_create(f, dxpl_id, oh) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to create dense storage for attributes")

            /* Set up user data for callback */
            udata.f = f;
            udata.dxpl_id = dxpl_id;

            /* Iterate over existing attributes, moving them to dense storage */
            op.lib_op = H5O_msg_attr_to_dense_cb;
            if(H5O_msg_iterate_real(f, oh, H5O_MSG_ATTR, TRUE, op, &udata, dxpl_id, oh_flags_ptr) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTCONVERT, FAIL, "error converting attributes to dense storage")
        } /* end if */
    } /* end if */

    /* Check for storing attribute with dense storage */
    if(new_format_attr && H5F_addr_defined(oh->attr_fheap_addr)) {
        /* Insert attribute into dense storage */
#ifdef QAK
HDfprintf(stderr, "%s: inserting attribute to dense storage\n", FUNC);
#endif /* QAK */
        if(H5A_dense_insert(f, dxpl_id, oh, mesg_flags, mesg) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, FAIL, "unable to add to dense storage")
    } /* end if */
    else {
        /* Create a new message */
        if((idx = H5O_new_mesg(f, oh, &mesg_flags, type, mesg, &sh_mesg, &new_type, &new_mesg, dxpl_id, oh_flags_ptr)) == UFAIL)
            HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, FAIL, "unable to create new message")

        /* Write the information to the message */
        if(H5O_write_mesg(f, dxpl_id, oh, idx, new_type, new_mesg, mesg_flags, update_flags, oh_flags_ptr) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to write message")
    } /* end else */

    /* If the message added is an attribute, increment count */
    if(new_format_attr)
        oh->nattrs++;
#ifdef H5O_DEBUG
H5O_assert(oh);
#endif /* H5O_DEBUG */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_append() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_write
 *
 * Purpose:	Modifies an existing message or creates a new message.
 *
 *		The OVERWRITE argument is a sequence number of a
 *		message to overwrite (usually zero).
 *		If the message to overwrite doesn't exist then this routine
 *              fails.
 *
 *              The UPDATE_FLAGS argument are flags that allow the caller
 *              to skip updating the modification time or reseting the message
 *              data.  This is useful when several calls to H5O_msg_write will be
 *              made in a sequence.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
/* JAMES: this will probably get put through its paces when extending shared
 * dataspaces */
herr_t
H5O_msg_write(H5O_loc_t *loc, unsigned type_id, unsigned overwrite,
   unsigned mesg_flags, unsigned update_flags, void *mesg, hid_t dxpl_id)
{
    H5O_t *oh = NULL;                   /* Object header to use */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;     /* Flags to unprotect object header */
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    htri_t shared_mesg;                 /* Whether the message should be shared */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_write, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(H5O_ATTR_ID != type_id);   /* Attributes are modified in another routine */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(mesg);
    HDassert(0 == (mesg_flags & ~H5O_MSG_FLAG_BITS));

    /* Check for write access on the file */
    if(0 == (H5F_INTENT(loc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Should this message be written as a SOHM? */
    if((shared_mesg = H5SM_try_share(loc->file, dxpl_id, type_id, mesg)) > 0)
        /* Mark the message as shared */
        mesg_flags |= H5O_MSG_FLAG_SHARED;
    else if(shared_mesg < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "error while trying to share message")

    /* Protect the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Call the "real" modify routine */
    if(H5O_msg_write_real(loc->file, oh, type, overwrite, mesg_flags, update_flags, mesg, dxpl_id, &oh_flags) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header message")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_write() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_write_real
 *
 * Purpose:	Modifies an existing message or creates a new message.
 *
 *		The OVERWRITE argument is a sequence number of a
 *		message to overwrite (usually zero).
 *		If the message to overwrite doesn't exist then this routine
 *              fails.
 *
 *              The UPDATE_FLAGS argument are flags that allow the caller
 *              to skip updating the modification time or reseting the message
 *              data.  This is useful when several calls to H5O_msg_write will be
 *              made in a sequence.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_msg_write_real(H5F_t *f, H5O_t *oh, const H5O_msg_class_t *type,
    unsigned overwrite, unsigned mesg_flags, unsigned update_flags,
    const void *mesg, hid_t dxpl_id, unsigned *oh_flags_ptr)
{
    int	        	sequence;       /* Sequence # of message type to modify */
    unsigned		idx;            /* Index of message to modify */
    H5O_mesg_t         *idx_msg;        /* Pointer to message to modify */
    H5O_shared_t	sh_mesg;        /* Shared message to store */
    const H5O_msg_class_t *write_type = type;   /* Type of message to be written */
    const void          *write_mesg = mesg;     /* Actual message being written */
    herr_t	        ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_write_real)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(type);
    HDassert(mesg);
    HDassert(0 == (mesg_flags & ~H5O_MSG_FLAG_BITS));
    HDassert(oh_flags_ptr);

    /* Count similar messages */
    for(idx = 0, sequence = -1, idx_msg = &oh->mesg[0]; idx < oh->nmesgs; idx++, idx_msg++) {
	if(type->id != idx_msg->type->id)
            continue;
	if(++sequence == (int)overwrite)
            break;
    } /* end for */

    /* Was the right message found? */
    if(sequence != (int)overwrite)
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "message not found")

    /* Check for modifying a constant message */
    if(oh->mesg[idx].flags & H5O_MSG_FLAG_CONSTANT) {
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to modify constant message")
    } else if(oh->mesg[idx].flags & H5O_MSG_FLAG_SHARED) {
        /* This message is shared, but it's being modified.  This is valid if
         * it's shared in the heap .
         * First, make sure it's not a committed message; these can't ever
         * be modified.
         */
        HDassert(!(((H5O_shared_t *)oh->mesg[idx].native)->flags & H5O_COMMITTED_FLAG));

        /* Remove the old message from the SOHM index */
        if(H5SM_try_delete(f, dxpl_id, oh->mesg[idx].type->id, oh->mesg[idx].native) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to delete message from SOHM table")

        /* Now this message is no longer shared and we can safely overwrite it.
         * We need to make sure that the message we're writing is shared,
         * though, and that the library doesn't try to reset the current
         * message like it would in a normal overwrite (this message is
         * realy a shared pointer, not a real message).
         * JAMES: will this break if a shared message is overwritten with a larger
         * non-shared message?
         */
        HDassert(H5O_msg_is_shared(type->id, mesg) > 0); /* JAMES: this should work with
                                                      * replacement messages that aren't shared, too. */

        /* Extract shared message info from current message */
        if(H5O_msg_get_share(type->id, mesg, &sh_mesg) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't get shared message")

        /* Instead of writing the original message, write a shared message */
        write_type = H5O_MSG_SHARED;
        write_mesg = &sh_mesg;
    } /* end if */

    /* Write the information to the message */
    if(H5O_write_mesg(f, dxpl_id, oh, idx, write_type, write_mesg, mesg_flags, update_flags, oh_flags_ptr) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to write message")
#ifdef H5O_DEBUG
H5O_assert(oh);
#endif /* H5O_DEBUG */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_write_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_read
 *
 * Purpose:	Reads a message from an object header and returns a pointer
 *		to it.	The caller will usually supply the memory through
 *		MESG and the return value will be MESG.	 But if MESG is
 *		the null pointer, then this function will malloc() memory
 *		to hold the result and return its pointer instead.
 *
 * Return:	Success:	Ptr to message in native format.  The message
 *				should be freed by calling H5O_msg_reset().  If
 *				MESG is a null pointer then the caller should
 *				also call H5MM_xfree() on the return value
 *				after calling H5O_msg_reset().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_msg_read(const H5O_loc_t *loc, unsigned type_id, int sequence, void *mesg,
    hid_t dxpl_id)
{
    H5O_t *oh = NULL;                   /* Object header to use */
    void *ret_value;                    /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_read, NULL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    HDassert(sequence >= 0);

    /* Get the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "unable to load object header")

    /* Call the "real" read routine */
    if((ret_value = H5O_msg_read_real(loc->file, oh, type_id, sequence, mesg, dxpl_id)) == NULL)
	HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to load object header")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, NULL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_read() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_read_real
 *
 * Purpose:	Reads a message from an object header and returns a pointer
 *		to it.	The caller will usually supply the memory through
 *		MESG and the return value will be MESG.	 But if MESG is
 *		the null pointer, then this function will malloc() memory
 *		to hold the result and return its pointer instead.
 *
 * Return:	Success:	Ptr to message in native format.  The message
 *				should be freed by calling H5O_msg_reset().  If
 *				MESG is a null pointer then the caller should
 *				also call H5MM_xfree() on the return value
 *				after calling H5O_msg_reset().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_msg_read_real(H5F_t *f, H5O_t *oh, unsigned type_id, int sequence,
    void *mesg, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    unsigned       idx;                 /* Message's index in object header */
    void           *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_read_real)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(sequence >= 0);

    /* Scan through the messages looking for the right one */
    for(idx = 0; idx < oh->nmesgs; idx++) {
	if(type_id != oh->mesg[idx].type->id)
            continue;
	if(--sequence < 0)
            break;
    } /* end for */
    if(sequence >= 0)
	HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, NULL, "unable to find object header message")

    /*
     * Decode the message if necessary.  If the message is shared then decode
     * a shared message, ignoring the message type.
     */
    H5O_LOAD_NATIVE(f, dxpl_id, &(oh->mesg[idx]), NULL)

    /* Read the message in */
    if(oh->mesg[idx].flags & H5O_MSG_FLAG_SHARED) {
	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
        if(NULL == (ret_value = H5O_shared_read(f, dxpl_id, oh->mesg[idx].native, type, mesg)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy message to user space")
    } else {
	/*
	 * The message is not shared, but rather exists in the object
	 * header.  The object header caches the native message (along with
	 * the raw message) so we must copy the native message before
	 * returning.
	 */
	if(NULL == (ret_value = (type->copy)(oh->mesg[idx].native, mesg)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy message to user space")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_read_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_reset
 *
 * Purpose:	Some message data structures have internal fields that
 *		need to be freed.  This function does that if appropriate
 *		but doesn't free NATIVE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 12 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_reset(unsigned type_id, void *native)
{
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_reset, FAIL)

    /* check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" reset routine */
    if(H5O_msg_reset_real(type, native) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTRESET, FAIL, "unable to reset object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_reset_real
 *
 * Purpose:	Some message data structures have internal fields that
 *		need to be freed.  This function does that if appropriate
 *		but doesn't free NATIVE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 12 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_msg_reset_real(const H5O_msg_class_t *type, void *native)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_reset_real)

    /* check args */
    HDassert(type);

    if(native) {
	if(type->reset) {
	    if((type->reset)(native) < 0)
		HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "reset method failed")
	} else
	    HDmemset(native, 0, type->native_size);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_reset_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_free
 *
 * Purpose:	Similar to H5O_msg_reset() except it also frees the message
 *		pointer.
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_msg_free(unsigned type_id, void *mesg)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    void * ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI_NOFUNC(H5O_msg_free)

    /* check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" free routine */
    ret_value = H5O_msg_free_real(type, mesg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_free() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_free_mesg
 *
 * Purpose:	Call H5O_msg_free_real() on a message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, Sep  6, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_free_mesg(H5O_mesg_t *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_msg_free_mesg)

    /* check args */
    HDassert(mesg);

    /* Free any native information */
    if(mesg->flags & H5O_MSG_FLAG_SHARED)
        mesg->native = H5O_msg_free_real(H5O_MSG_SHARED, mesg->native);
    else
        mesg->native = H5O_msg_free_real(mesg->type, mesg->native);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_msg_free_mesg() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_free_real
 *
 * Purpose:	Similar to H5O_msg_reset() except it also frees the message
 *		pointer.
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_msg_free_real(const H5O_msg_class_t *type, void *msg_native)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_msg_free_real)

    /* check args */
    HDassert(type);

    if(msg_native) {
        H5O_msg_reset_real(type, msg_native);
        if (NULL!=(type->free))
            (type->free)(msg_native);
        else
            H5MM_xfree(msg_native);
    } /* end if */

    FUNC_LEAVE_NOAPI(NULL)
} /* end H5O_msg_free_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_copy
 *
 * Purpose:	Copies a message.  If MESG is is the null pointer then a null
 *		pointer is returned with no error.
 *
 * Return:	Success:	Ptr to the new message
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_msg_copy(unsigned type_id, const void *mesg, void *dst)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    void	*ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_copy, NULL)

    /* check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" copy routine */
    if((ret_value = H5O_msg_copy_real(type, mesg, dst)) == NULL)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_copy_real
 *
 * Purpose:	Copies a message.  If MESG is is the null pointer then a null
 *		pointer is returned with no error.
 *
 * Return:	Success:	Ptr to the new message
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_msg_copy_real(const H5O_msg_class_t *type, const void *mesg, void *dst)
{
    void	*ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_copy_real)

    /* check args */
    HDassert(type);
    HDassert(type->copy);

    if(mesg)
	if(NULL == (ret_value = (type->copy)(mesg, dst)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_copy_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_count
 *
 * Purpose:	Counts the number of messages in an object header which are a
 *		certain type.
 *
 * Return:	Success:	Number of messages of specified type.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 *-------------------------------------------------------------------------
 */
int
H5O_msg_count(const H5O_loc_t *loc, unsigned type_id, hid_t dxpl_id)
{
    H5O_t *oh = NULL;           /* Object header to operate on */
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    int	acc;                    /* Count of the message type found */
    unsigned u;                 /* Local index variable */
    int	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_count, FAIL)

    /* Check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Load the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Loop over all messages, counting the ones of the type looked for */
    for(u = acc = 0; u < oh->nmesgs; u++)
	if(oh->mesg[u].type == type)
            acc++;

    /* Set return value */
    ret_value = acc;

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_count() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_exists
 *
 * Purpose:	Determines if a particular message exists in an object
 *		header without trying to decode the message.
 *
 * Return:	Success:	FALSE if the message does not exist; TRUE if
 *				th message exists.
 *
 *		Failure:	FAIL if the existence of the message could
 *				not be determined due to some error such as
 *				not being able to read the object header.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_msg_exists(H5O_loc_t *loc, unsigned type_id, int sequence, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;             /* Object header for location */
    htri_t      ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_exists, FAIL)

    HDassert(loc);
    HDassert(loc->file);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    HDassert(sequence >= 0);

    /* Load the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Call the "real" exists routine */
    if((ret_value = H5O_msg_exists_oh(oh, type_id, sequence)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_READERROR, FAIL, "unable to verify object header message")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) != SUCCEED)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_exists() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_exists_oh
 *
 * Purpose:	Determines if a particular message exists in an object
 *		header without trying to decode the message.
 *
 * Return:	Success:	FALSE if the message does not exist; TRUE if
 *				th message exists.
 *
 *		Failure:	FAIL if the existence of the message could
 *				not be determined due to some error such as
 *				not being able to read the object header.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_msg_exists_oh(H5O_t *oh, unsigned type_id, int sequence)
{
    const H5O_msg_class_t *type;    /* Actual H5O class type for the ID */
    unsigned	u;              /* Local index variable */
    htri_t      ret_value;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_msg_exists_oh)

    HDassert(oh);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(sequence >= 0);

    /* Scan through the messages looking for the right one */
    for(u = 0; u < oh->nmesgs; u++) {
	if(type->id != oh->mesg[u].type->id)
            continue;
	if(--sequence < 0)
            break;
    } /* end for */

    /* Set return value */
    ret_value = (sequence < 0);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_exists_oh() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_remove
 *
 * Purpose:	Removes the specified message from the object header.
 *		If sequence is H5O_ALL (-1) then all messages of the
 *		specified type are removed.  Removing a message causes
 *		the sequence numbers to change for subsequent messages of
 *		the same type.
 *
 *		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 28 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_remove(H5O_loc_t *loc, unsigned type_id, int sequence, hbool_t adj_link,
    hid_t dxpl_id)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_remove, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" remove routine */
    if((ret_value = H5O_msg_remove_real(loc, type, sequence, NULL, NULL, adj_link, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to remove object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_remove_op
 *
 * Purpose:	Removes messages from the object header that a callback
 *              routine indicates should be removed.
 *
 *		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_remove_op(const H5O_loc_t *loc, unsigned type_id, int sequence,
    H5O_operator_t op, void *op_data, hbool_t adj_link, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_remove_op, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" remove routine */
    if((ret_value = H5O_msg_remove_real(loc, type, sequence, op, op_data, adj_link, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to remove object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_remove_op() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_remove_cb
 *
 * Purpose:	Object header iterator callback routine to remove messages
 *              of a particular type that match a particular sequence number,
 *              or all messages if the sequence number is H5O_ALL (-1).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_msg_remove_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/, unsigned sequence,
    unsigned *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_rm_t *udata = (H5O_iter_rm_t *)_udata;   /* Operator user data */
    htri_t try_remove = FALSE;         /* Whether to try removing a message */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_remove_cb)

    /* check args */
    HDassert(mesg);

    /* Check for callback routine */
    if(udata->op) {
        /* Call the iterator callback */
        if((try_remove = (udata->op)(mesg->native, sequence, udata->op_data)) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, H5_ITER_ERROR, "object header message deletion callback failed")
    } /* end if */
    else {
        /* If there's no callback routine, does the sequence # match? */
        if((int)sequence == udata->sequence || H5O_ALL == udata->sequence)
            try_remove = TRUE;
    } /* end else */

    /* Try removing the message, if indicated */
    if(try_remove) {
        /*
         * Keep track of how many times we failed trying to remove constant
         * messages.
         */
        if(mesg->flags & H5O_MSG_FLAG_CONSTANT)
            udata->nfailed++;
        else {
            /* If the message removed is an attribute, decrement count */
            if(H5O_ATTR_ID == mesg->type->id && oh->version > H5O_VERSION_1)
                oh->nattrs--;

            /* Convert message into a null message */
            if(H5O_release_mesg(udata->f, udata->dxpl_id, oh, mesg, TRUE, udata->adj_link) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, H5_ITER_ERROR, "unable to convert into null message")

            /* Indicate that the object header was modified */
            *oh_flags_ptr |= H5AC__DIRTIED_FLAG;
        } /* end else */

        /* Break out now, if we've found the correct message */
        if(udata->sequence == H5O_FIRST || udata->sequence != H5O_ALL)
            HGOTO_DONE(H5_ITER_STOP)
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_remove_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_remove_real
 *
 * Purpose:	Removes the specified message from the object header.
 *		If sequence is H5O_ALL (-1) then all messages of the
 *		specified type are removed.  Removing a message causes
 *		the sequence numbers to change for subsequent messages of
 *		the same type.
 *
 *		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 28 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_msg_remove_real(const H5O_loc_t *loc, const H5O_msg_class_t *type, int sequence,
    H5O_operator_t app_op, void *op_data, hbool_t adj_link, hid_t dxpl_id)
{
    H5O_t *oh = NULL;                   /* Pointer to actual object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;  /* Start iteration with no flags set on object header */
    H5O_iter_rm_t udata;                /* User data for iterator */
    H5O_mesg_operator_t op;             /* Wrapper for operator */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_remove_real)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(type);

    /* Make certain we are allowed to modify the file */
    if(0 == (H5F_INTENT(loc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Set up iterator operator data */
    udata.f = loc->file;
    udata.dxpl_id = dxpl_id;
    udata.sequence = sequence;
    udata.nfailed = 0;
    udata.op = app_op;
    udata.op_data = op_data;
    udata.adj_link = adj_link;

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Iterate over the messages, deleting appropriate one(s) */
    op.lib_op = H5O_msg_remove_cb;
    if(H5O_msg_iterate_real(loc->file, oh, type, TRUE, op, &udata, dxpl_id, &oh_flags) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "error iterating over messages")

    /* Fail if we tried to remove any constant messages */
    if(udata.nfailed)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to remove constant message(s)")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
        HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_remove_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_iterate
 *
 * Purpose:	Iterate through object headers of a certain type.
 *
 * Return:	Returns a negative value if something is wrong, the return
 *      value of the last operator if it was non-zero, or zero if all
 *      object headers were processed.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Nov 19 2004
 *
 * Description:
 *      This function interates over the object headers of an object
 *  specified with 'loc' of type 'type_id'.  For each object header of the
 *  object, the 'op_data' and some additional information (specified below) are
 *  passed to the 'op' function.
 *      The operation receives a pointer to the object header message for the
 *  object being iterated over ('mesg'), and the pointer to the operator data
 *  passed in to H5O_msg_iterate ('op_data').  The return values from an operator
 *  are:
 *      A. Zero causes the iterator to continue, returning zero when all
 *          object headers of that type have been processed.
 *      B. Positive causes the iterator to immediately return that positive
 *          value, indicating short-circuit success.
 *      C. Negative causes the iterator to immediately return that value,
 *          indicating failure.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_iterate(const H5O_loc_t *loc, unsigned type_id, H5O_operator_t app_op,
    void *op_data, hid_t dxpl_id)
{
    H5O_t *oh = NULL;               /* Pointer to actual object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;  /* Start iteration with no flags set on object header */
    const H5O_msg_class_t *type;    /* Actual H5O class type for the ID */
    H5O_mesg_operator_t op;         /* Wrapper for operator */
    herr_t ret_value;               /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_iterate, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Call the "real" iterate routine */
    op.app_op = app_op;
    if((ret_value = H5O_msg_iterate_real(loc->file, oh, type, FALSE, op, op_data, dxpl_id, &oh_flags)) < 0)
        HERROR(H5E_OHDR, H5E_BADITER, "unable to iterate over object header messages");

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
        HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_iterate_real
 *
 * Purpose:	Iterate through object headers of a certain type.
 *
 * Return:	Returns a negative value if something is wrong, the return
 *      value of the last operator if it was non-zero, or zero if all
 *      object headers were processed.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 * Description:
 *      This function interates over the object headers of an object
 *  specified with 'ent' of type 'type_id'.  For each object header of the
 *  object, the 'op_data' and some additional information (specified below) are
 *  passed to the 'op' function.
 *      The operation receives a pointer to the object header message for the
 *  object being iterated over ('mesg'), and the pointer to the operator data
 *  passed in to H5O_msg_iterate ('op_data').  The return values from an operator
 *  are:
 *      A. Zero causes the iterator to continue, returning zero when all
 *          object headers of that type have been processed.
 *      B. Positive causes the iterator to immediately return that positive
 *          value, indicating short-circuit success.
 *      C. Negative causes the iterator to immediately return that value,
 *          indicating failure.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_iterate_real(H5F_t *f, H5O_t *oh, const H5O_msg_class_t *type,
    hbool_t internal, H5O_mesg_operator_t op, void *op_data, hid_t dxpl_id,
    unsigned *oh_flags_ptr)
{
    unsigned		idx;            /* Absolute index of current message in all messages */
    unsigned		sequence;       /* Relative index of current message for messages of type */
    H5O_mesg_t         *idx_msg;        /* Pointer to current message */
    void               *native_mesg;    /* Native, readable message */
    hbool_t             native_mesg_alloc = FALSE;  /* True if native_mesg needs to be freed */
    herr_t              ret_value = H5_ITER_CONT;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_iterate_real)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(type);
    HDassert(op.app_op);
    HDassert(oh_flags_ptr);

    /* Iterate over messages */
    for(sequence = 0, idx = 0, idx_msg = &oh->mesg[0]; idx < oh->nmesgs && !ret_value; idx++, idx_msg++) {
	if(type->id == idx_msg->type->id) {
            /* Decode the message if necessary.  */
            H5O_LOAD_NATIVE(f, dxpl_id, idx_msg, FAIL)

            /* Check for making an "internal" (i.e. within the H5O package) callback */
            if(internal) {
                /* Call the "internal" iterator callback */
                if((ret_value = (op.lib_op)(oh, idx_msg, sequence, oh_flags_ptr, op_data)) != 0)
                    break;
            } /* end if */
            else {
                /* If the message is shared, get the real message it points to */
                /* JAMES: test */
                if(idx_msg->flags & H5O_MSG_FLAG_SHARED) {
                    if(NULL == (native_mesg = H5O_shared_read(f, dxpl_id,
                                idx_msg->native, idx_msg->type, NULL)))
                        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "unable to read shared message")
                    native_mesg_alloc = TRUE;
                } /* end if */
                else
                    native_mesg = idx_msg->native;

                /* Call the iterator callback */
                if((ret_value = (op.app_op)(native_mesg, sequence, op_data)) != 0)
                    break;

                /* Free the "real" message if it was allocated */
                if(native_mesg_alloc) {
                    H5O_msg_free(idx_msg->type->id, native_mesg);
                    native_mesg_alloc = FALSE;
                } /* end if */
            } /* end else */

            /* Increment sequence value for message type */
            sequence++;
        } /* end if */
    } /* end for */

    /* Check for error from iterator */
    if(ret_value < 0)
        HERROR(H5E_OHDR, H5E_CANTLIST, "iterator function failed");

done:
    /* Free the native message if it was allocated */
    if(native_mesg_alloc)
        H5O_msg_free(idx_msg->type->id, native_mesg);

    /* Check if object message was modified */
    if(*oh_flags_ptr & H5AC__DIRTIED_FLAG) {
        /* Try to condense object header info */
        /* (Since this routine is used to remove messages from an
         *  object header, the header will be condensed after each
         *  message removal)
         */
        if(H5O_condense_header(f, oh, dxpl_id) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_CANTPACK, FAIL, "can't pack object header")

        if(H5O_touch_oh(f, dxpl_id, oh, FALSE, oh_flags_ptr) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_CANTUPDATE, FAIL, "unable to update time on object")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_iterate_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_raw_size
 *
 * Purpose:	Call the 'raw_size' method for a
 *              particular class of object header.
 *
 * Return:	Size of message on success, 0 on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 13 2003
 *
 *-------------------------------------------------------------------------
 */
size_t
H5O_msg_raw_size(const H5F_t *f, unsigned type_id, const void *mesg)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    size_t      ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_raw_size, 0)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->raw_size);
    HDassert(f);
    HDassert(mesg);

    /* Compute the raw data size for the mesg */
    if((ret_value = (type->raw_size)(f, mesg)) == 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOUNT, 0, "unable to determine size of message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_raw_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_mesg_size
 *
 * Purpose:	Calculate the final size of an encoded message in an object
 *              header.
 *
 * Note:	This routine assumes that the message size will be used in the
 *              creation of a new object header.
 *
 * Return:	Size of message on success, 0 on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
size_t
H5O_msg_mesg_size(const H5F_t *f, unsigned type_id, const void *mesg, size_t extra_raw)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    size_t      ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_mesg_size, 0)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->raw_size);
    HDassert(f);
    HDassert(mesg);

    /* Compute the raw data size for the mesg */
    if((ret_value = (type->raw_size)(f, mesg)) == 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOUNT, 0, "unable to determine size of message")

    /* Add in "extra" raw space */
    ret_value += extra_raw;

    /* Adjust size for alignment, if necessary */
    ret_value = H5O_ALIGN_F(f, ret_value);

    /* Add space for message header */
    ret_value += H5O_SIZEOF_MSGHDR_F(f);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_mesg_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_get_share
 *
 * Purpose:	Call the 'get_share' method for a
 *              particular class of object header.
 *
 * Return:	Success:	Non-negative, and SHARE describes the shared
 *				object.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct  2 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_get_share(unsigned type_id, const void *mesg, H5O_shared_t *share)
{
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    herr_t ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_get_share, FAIL)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->get_share);
    HDassert(mesg);
    HDassert(share);

    /* Get shared data for the mesg */
    if((ret_value = (type->get_share)(mesg, share)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to retrieve shared message information")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_get_share() */


/*-------------------------------------------------------------------------
 * Function:    H5O_msg_is_shared
 *
 * Purpose:     Call the 'is_shared' method for a
 *              particular class of object header.
 *
 * Return:      Object is shared:        TRUE
 *              Object is not shared:    FALSE
 *
 * Programmer:  James Laird
 *              jlaird@ncsa.uiuc.edu
 *              April 5 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_msg_is_shared(unsigned type_id, const void *mesg)
{
    const H5O_msg_class_t *type;    /* Actual H5O class type for the ID */
    htri_t ret_value;

    FUNC_ENTER_NOAPI_NOFUNC(H5O_msg_is_shared)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(mesg);

    HDassert(type_id != H5O_SHARED_ID); /* JAMES: check for this mistake elsewhere, too */

    /* If there is no is_shared function, then obviously it's not a shared message! */
    if(!(type->is_shared))
        ret_value = FALSE;
    else
        ret_value = (type->is_shared)(mesg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_is_shared() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_set_share
 *
 * Purpose:	Set the shared information for an object header message.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		jlaird@hdfgroup.org
 *		November 1 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_set_share(unsigned type_id, H5O_shared_t *share, void *mesg)
{
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    herr_t ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_set_share, FAIL)

    /* Check args */
    HDassert(share);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->set_share);
    HDassert(mesg);
    HDassert(share->flags != H5O_NOT_SHARED);

    /* Set this message as the shared message for the message, wiping out
     * any information that was there before
     */
    if((ret_value = (type->set_share)(mesg, share)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTSET, FAIL, "unable to set shared message information")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_set_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_reset_share
 *
 * Purpose:	Reset the shared information for an object header message.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		jlaird@hdfgroup.org
 *		Oct 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_reset_share(unsigned type_id, void *mesg)
{
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    H5O_shared_t sh_mesg;               /* Shared message */
    herr_t ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_reset_share, FAIL)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->set_share);
    HDassert(mesg);

    /* Initialize the shared message to zero. */
    HDmemset(&sh_mesg, 0, sizeof(H5O_shared_t));

    /* Set this message as the shared message for the message, wiping out
     * any information that was there before
     */
    if((ret_value = (type->set_share)(mesg, &sh_mesg)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTSET, FAIL, "unable to reset shared message information")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_reset_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_encode
 *
 * Purpose:	Encode an object(data type and simple data space only)
 *              description into a buffer.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *		slu@ncsa.uiuc.edu
 *		July 13, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_msg_encode(H5F_t *f, unsigned type_id, unsigned char *buf, const void *mesg)
{
    const H5O_msg_class_t   *type;      /* Actual H5O class type for the ID */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_encode,FAIL)

    /* check args */
    HDassert(f);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Encode */
    if((type->encode)(f, buf, mesg) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_decode
 *
 * Purpose:	Decode a binary object description and return a new
 *              object handle.
 *
 * Note:	This routine is not guaranteed to work with all possible
 *              header messages, use with care.
 *
 * Return:	Success:        Pointer to object(data type or space)
 *
 *		Failure:	NULL
 *
 * Programmer:	Raymond Lu
 *		slu@ncsa.uiuc.edu
 *		July 14, 2004
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_msg_decode(H5F_t *f, hid_t dxpl_id, unsigned type_id, const unsigned char *buf)
{
    const H5O_msg_class_t   *type;      /* Actual H5O class type for the ID */
    void *ret_value;                    /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_decode, NULL)

    /* check args */
    HDassert(f);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* decode */
    if((ret_value = (type->decode)(f, dxpl_id, buf)) == NULL)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, NULL, "unable to decode message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_msg_copy_file
 *
 * Purpose:     Copies a message to file.  If MESG is is the null pointer then a null
 *              pointer is returned with no error.
 *
 * Return:      Success:        Ptr to the new message
 *
 *              Failure:        NULL
 *
 * Programmer:  Peter Cao
 *              June 4, 2005
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_msg_copy_file(const H5O_msg_class_t *copy_type, const H5O_msg_class_t *mesg_type, H5F_t *file_src, void *native_src,
    H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info, void *udata)
{
    void        *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_copy_file)

    /* check args */
    HDassert(copy_type);
    HDassert(mesg_type);
    HDassert(copy_type->copy_file);
    HDassert(file_src);
    HDassert(native_src);
    HDassert(file_dst);
    HDassert(cpy_info);

    if(NULL == (ret_value = (copy_type->copy_file)(file_src, mesg_type, native_src, file_dst, dxpl_id, cpy_info, udata)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy object header message to file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_copy_file() */


/*-------------------------------------------------------------------------
 * Function:	H5O_new_mesg
 *
 * Purpose:	Create a new message in an object header
 *
 * Return:	Success:	Index of message
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, September  3, 2003
 *
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_new_mesg(H5F_t *f, H5O_t *oh, unsigned *mesg_flags, const H5O_msg_class_t *orig_type,
    const void *orig_mesg, H5O_shared_t *sh_mesg, const H5O_msg_class_t **new_type,
    const void **new_mesg, hid_t dxpl_id, unsigned *oh_flags_ptr)
{
    size_t	size;                   /* Size of space allocated for object header */
    htri_t      is_shared;              /* Is this a shared message? */
    unsigned    ret_value = UFAIL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_new_mesg)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(mesg_flags);
    HDassert(orig_type);
    HDassert(orig_mesg);
    HDassert(sh_mesg);
    HDassert(new_type);
    HDassert(new_mesg);
    HDassert(oh_flags_ptr);

    /* Check for shared message */
    if(*mesg_flags & H5O_MSG_FLAG_SHARED) {
        if((NULL == orig_type->is_shared) || (NULL == orig_type->get_share))
            HGOTO_ERROR(H5E_OHDR, H5E_UNSUPPORTED, UFAIL, "message class is not sharable")
        if((is_shared = (orig_type->is_shared)(orig_mesg)) == FALSE) {
            /*
             * If the message isn't shared then turn off the shared bit
             * and treat it as an unshared message.
             */
            *mesg_flags &= ~H5O_MSG_FLAG_SHARED;
            *new_type = orig_type;
            *new_mesg = orig_mesg;
        } else if(is_shared > 0) {
            /* Message is shared. Get shared message, change message type,
             * and use shared information */
            HDmemset(sh_mesg, 0, sizeof(H5O_shared_t));
            if((orig_type->get_share)(orig_mesg, sh_mesg/*out*/) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, UFAIL, "can't get shared message")

            *new_type = H5O_MSG_SHARED;
            *new_mesg = sh_mesg;
        } else {
            HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, UFAIL, "can't determine if message is shared")
        }/* end else */
    } /* end if */
    else {
        *new_type = orig_type;
        *new_mesg = orig_mesg;
    } /* end else */

    /* Compute the size needed to store the message on disk */
    if((size = ((*new_type)->raw_size)(f, *new_mesg)) >= H5O_MESG_MAX_SIZE)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, UFAIL, "object header message is too large")

    /* Allocate space in the object header for the message */
    if((ret_value = H5O_alloc(f, dxpl_id, oh, orig_type, size, oh_flags_ptr)) == UFAIL)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, UFAIL, "unable to allocate space for message")

    /* Increment any links in message */
    if((*new_type)->link && ((*new_type)->link)(f, dxpl_id, (*new_mesg)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, UFAIL, "unable to adjust shared object link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_new_mesg() */


/*-------------------------------------------------------------------------
 * Function:	H5O_write_mesg
 *
 * Purpose:	Write message to object header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, September  3, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_write_mesg(H5F_t *f, hid_t dxpl_id, H5O_t *oh, unsigned idx,
    const H5O_msg_class_t *type, const void *mesg, unsigned mesg_flags,
    unsigned update_flags, unsigned *oh_flags_ptr)
{
    H5O_mesg_t         *idx_msg;        /* Pointer to message to modify */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_write_mesg)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(type);
    HDassert(mesg);
    HDassert(oh_flags_ptr);

    /* Set pointer to the correct message */
    idx_msg = &oh->mesg[idx];

    /* Reset existing native information */
    H5O_msg_reset_real(type, idx_msg->native);

    /* Copy the native value for the message */
    if(NULL == (idx_msg->native = (type->copy)(mesg, idx_msg->native)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to copy message to object header")

    /* Update the message flags and mark the message as modified */
    idx_msg->flags = mesg_flags;
    idx_msg->dirty = TRUE;

    /* Update the modification time, if requested */
    if(update_flags & H5O_UPDATE_TIME)
        if(H5O_touch_oh(f, dxpl_id, oh, FALSE, oh_flags_ptr) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTUPDATE, FAIL, "unable to update time on object")

    /* Mark the object header as modified */
    *oh_flags_ptr |= H5AC__DIRTIED_FLAG;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_write_mesg() */


/*-------------------------------------------------------------------------
 * Function:	H5O_delete_mesg
 *
 * Purpose:	Internal function to:
 *              Delete an object header message from a file.  This frees the file
 *              space used for anything referred to in the object header message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		September 26 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_delete_mesg(H5F_t *f, hid_t dxpl_id, H5O_mesg_t *mesg, hbool_t adj_link)
{
    const H5O_msg_class_t *type;  /* Type of object to free */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5O_delete_mesg, FAIL)

    /* Check args */
    HDassert(f);
    HDassert(mesg);

    /* Get the message to free's type */
    if(mesg->flags & H5O_MSG_FLAG_SHARED)
        type = H5O_MSG_SHARED;
    else
        type = mesg->type;

    /* Check if there is a file space deletion callback for this type of message */
    if(type->del) {
        /* Decode the message if necessary. */
        if(NULL == mesg->native) {
            HDassert(type->decode);
            if(NULL == (mesg->native = (type->decode)(f, dxpl_id, mesg->raw)))
                HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "unable to decode message")
        } /* end if */

        /* Check if this message needs to be removed from the SOHM table */
        /* JAMES: there should be a callback, maybe in H5O_shared_delete, to fiddle w/ the ref. count.
        * We shouldn't need to do a search in the SOHM table on delete. */
        if(type == H5O_MSG_SHARED)
        {
            /* The native message here is actually a shared message.    */
            if(H5SM_try_delete(f, dxpl_id, mesg->type->id, mesg->native) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to delete message from SOHM table")
        }

        if((type->del)(f, dxpl_id, mesg->native, adj_link) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to delete file space for object header message")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_delete_msg() */

