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
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5SMprivate.h"        /* Shared object header messages        */


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

static herr_t H5O_msg_append_real(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    const H5O_msg_class_t *type, unsigned mesg_flags, unsigned update_flags,
    const void *mesg, unsigned * oh_flags_ptr);
static herr_t H5O_msg_write_real(H5O_loc_t *loc, const H5O_msg_class_t *type,
    unsigned overwrite, unsigned flags, unsigned update_flags, const void *mesg,
    hid_t dxpl_id);
static herr_t H5O_msg_reset_real(const H5O_msg_class_t *type, void *native);
static void *H5O_msg_copy_real(const H5O_msg_class_t *type, const void *mesg,
        void *dst);
static unsigned H5O_find_in_ohdr(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    const H5O_msg_class_t **type_p, int sequence);
static unsigned H5O_new_mesg(H5F_t *f, H5O_t *oh, unsigned *flags,
    const H5O_msg_class_t *orig_type, const void *orig_mesg, H5O_shared_t *sh_mesg,
    const H5O_msg_class_t **new_type, const void **new_mesg, hid_t dxpl_id,
    unsigned * oh_flags_ptr);
static herr_t H5O_write_mesg(H5F_t *f, hid_t dxpl_id, H5O_t *oh, unsigned idx,
    const H5O_msg_class_t *type, const void *mesg, unsigned flags,
    unsigned update_flags, unsigned * oh_flags_ptr);


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
H5O_msg_append(H5F_t *f, hid_t dxpl_id, H5O_t *oh, unsigned type_id, unsigned mesg_flags,
    unsigned update_flags, void *mesg, unsigned * oh_flags_ptr)
{
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
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
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "error determining if message should be shared");

    if(H5O_msg_append_real(f, dxpl_id, oh, type, mesg_flags, update_flags, mesg, oh_flags_ptr) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to append to object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_append() */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_append_real
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
static herr_t
H5O_msg_append_real(H5F_t *f, hid_t dxpl_id, H5O_t *oh, const H5O_msg_class_t *type,
    unsigned mesg_flags, unsigned update_flags, const void *mesg, unsigned * oh_flags_ptr)
{
    unsigned		idx;            /* Index of message to modify */
    H5O_shared_t	sh_mesg;
    herr_t	        ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_append_real)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(type);
    HDassert(0 == (mesg_flags & ~H5O_MSG_FLAG_BITS));
    HDassert(mesg);
    HDassert(oh_flags_ptr);

    /* Create a new message */
    if((idx = H5O_new_mesg(f, oh, &mesg_flags, type, mesg, &sh_mesg, &type, &mesg, dxpl_id, oh_flags_ptr)) == UFAIL)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to create new message")

    /* Write the information to the message */
    if(H5O_write_mesg(f, dxpl_id, oh, idx, type, mesg, mesg_flags, update_flags, oh_flags_ptr) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to write message")
#ifdef H5O_DEBUG
H5O_assert(oh);
#endif /* H5O_DEBUG */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_append_real () */


/*-------------------------------------------------------------------------
 * Function:	H5O_msg_hash
 *
 * Purpose:	Returns a hash value for an object header message.
 *
 * Return:	Non-H5O_HASH_UNDEF hash value on success
 *              H5O_HASH_UNDEF on failure
 *
 * Programmer:	James Laird
 *		April 13 2006
 *
 *-------------------------------------------------------------------------
 */
uint32_t
H5O_msg_hash(unsigned type_id, H5F_t *f, const void *mesg)
{
    size_t buf_size;
    unsigned char * buf = NULL;    /* Buffer to be hashed */
    uint32_t hash;
    uint32_t ret_value;

    FUNC_ENTER_NOAPI(H5O_msg_hash, H5O_HASH_UNDEF)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    HDassert(mesg);
    HDassert(f);

    /* Find out the size of buffer needed */
    if((buf_size = H5O_raw_size(type_id, f, mesg)) <= 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADSIZE, H5O_HASH_UNDEF, "can't find message size");

    /* JAMES: revisit this!  Some messages don't use as much space as they say
     * they need.  Quincey may have fixed this.
     */
    if((buf = H5MM_calloc(buf_size)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5O_HASH_UNDEF, "can't allocate buffer for message");

    /* Encode message into temporary buffer */
    if(H5O_encode(f, buf, mesg, type_id) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, H5O_HASH_UNDEF, "can't encode OH message");

    /* 
     * Compute the hash value for this message.  type_id is used here to
     * initialize the hash algorithm, and affects the resulting value.
     */
    hash = H5_checksum_lookup3(buf, buf_size, type_id);

    /* JAMES: this is a pretty good hash function. Do we need to version it?
     * If so, we'd do so here. */

    /* A hash value of H5O_HASH_UNDEF indicates failure. If we naturally
     * generated this value, reset it to some valid value. */
    if(hash == H5O_HASH_UNDEF)
        hash = (uint32_t) 1;

    /* Set return value */
    ret_value = hash;

done:
    if(buf)
        HDfree(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_hash() */


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
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    htri_t shared_mesg;                 /* Whether the message should be shared */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5O_msg_write, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(mesg);
    HDassert(0 == (mesg_flags & ~H5O_MSG_FLAG_BITS));

    /* Should this message be written as a SOHM? */
    if((shared_mesg = H5SM_try_share(loc->file, dxpl_id, type_id, mesg)) > 0)
        /* Mark the message as shared */
        mesg_flags |= H5O_MSG_FLAG_SHARED;
    else if(shared_mesg < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "error while trying to share message");

    /* Call the "real" modify routine */
    if(H5O_msg_write_real(loc, type, overwrite, mesg_flags, update_flags, mesg, dxpl_id) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header message")

done:
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
H5O_msg_write_real(H5O_loc_t *loc, const H5O_msg_class_t *type, unsigned overwrite,
   unsigned mesg_flags, unsigned update_flags, const void *mesg, hid_t dxpl_id)
{
    H5O_t		*oh = NULL;
    unsigned		oh_flags = H5AC__NO_FLAGS_SET;
    int	        	sequence;       /* Sequence # of message type to modify */
    unsigned		idx;            /* Index of message to modify */
    H5O_mesg_t         *idx_msg;        /* Pointer to message to modify */
    H5O_shared_t	sh_mesg;
    const H5O_msg_class_t *write_type = type;   /* Type of message to be written */
    const void          *write_mesg = mesg;     /* Actual message being written */
    herr_t	        ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_write_real)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type);
    HDassert(mesg);
    HDassert(0 == (mesg_flags & ~H5O_MSG_FLAG_BITS));

    /* Check for write access on the file */
    if(0 == (H5F_INTENT(loc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Protect the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

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
        if(((H5O_shared_t*)oh->mesg[idx].native)->flags & H5O_COMMITTED_FLAG)
            HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to modify committed message")

        /* Remove the old message from the SOHM index */
        if(H5SM_try_delete(loc->file, dxpl_id, oh->mesg[idx].type->id, oh->mesg[idx].native) < 0)
            HGOTO_ERROR (H5E_OHDR, H5E_CANTFREE, FAIL, "unable to delete message from SOHM table")

        /* Now this message is no longer shared and we can safely overwrite it.
         * We need to make sure that the message we're writing is shared,
         * though, and that the library doesn't try to reset the current
         * message like it would in a normal overwrite (this message is
         * realy a shared pointer, not a real message).
         * JAMES: will this break if a shared message is overwritten with a larger
         * non-shared message?
         */
        HDassert(H5O_is_shared(type->id, mesg) > 0); /* JAMES: this should work with
                                                      * replacement messages that aren't shared, too. */

        if(H5O_get_share(type->id, loc->file, mesg, &sh_mesg) < 0)
              HGOTO_ERROR (H5E_OHDR, H5E_BADMESG, FAIL, "can't get shared message")

        /* Instead of writing the original message, write a shared message */
        write_type = H5O_MSG_SHARED;
        write_mesg = &sh_mesg;
    } /* end if */

    /* Write the information to the message */
    if(H5O_write_mesg(loc->file, dxpl_id, oh, idx, write_type, write_mesg, mesg_flags, update_flags, &oh_flags) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to write message")
#ifdef H5O_DEBUG
H5O_assert(oh);
#endif /* H5O_DEBUG */

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

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
H5O_msg_read(const H5O_loc_t *loc, unsigned type_id, int sequence, void *mesg, hid_t dxpl_id)
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
H5O_msg_read_real(H5F_t *f, H5O_t *oh, unsigned type_id, int sequence, void *mesg, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;        /* Actual H5O class type for the ID */
    int             idx;
    void           *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5O_msg_read_real)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(sequence >= 0);

    /* can we get it from the object header? */
    if((idx = H5O_find_in_ohdr(f, dxpl_id, oh, &type, sequence)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, NULL, "unable to find message in object header")

    if(oh->mesg[idx].flags & H5O_MSG_FLAG_SHARED) {
	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
	H5O_shared_t *shared;

	shared = (H5O_shared_t *)(oh->mesg[idx].native);
        ret_value = H5O_shared_read(f, dxpl_id, shared, type, mesg);
    } else {
	/*
	 * The message is not shared, but rather exists in the object
	 * header.  The object header caches the native message (along with
	 * the raw message) so we must copy the native message before
	 * returning.
	 */
	if(NULL == (ret_value = (type->copy)(oh->mesg[idx].native, mesg, 0)))
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
 * Modifications:
 *              Changed to use IDs for types, instead of type objects, then
 *              call "real" routine.
 *              Quincey Koziol
 *		Feb 14 2003
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
 * Modifications:
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
 * Modifications:
 *              Changed to use IDs for types, instead of type objects, then
 *              call "real" routine.
 *              Quincey Koziol
 *		Feb 14 2003
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
	if(NULL == (ret_value = (type->copy)(mesg, dst, 0)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_msg_copy_real() */



/*-------------------------------------------------------------------------
 * Function:	H5O_find_in_ohdr
 *
 * Purpose:     Find a message in the object header without consulting
 *              a symbol table entry.
 *
 * Return:      Success:    Index number of message.
 *              Failure:    Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  6 1997
 *
 * Modifications:
 *      Robb Matzke, 1999-07-28
 *      The ADDR argument is passed by value.
 *
 *      Bill Wendling, 2003-09-30
 *      Modified so that the object header needs to be AC_protected
 *      before calling this function.
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_find_in_ohdr(H5F_t *f, hid_t dxpl_id, H5O_t *oh, const H5O_msg_class_t **type_p, int sequence)
{
    unsigned		u;
    unsigned		ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_find_in_ohdr)

    /* Check args */
    HDassert(f);
    HDassert(oh);
    HDassert(type_p);

    /* Scan through the messages looking for the right one */
    for(u = 0; u < oh->nmesgs; u++) {
	if(*type_p && (*type_p)->id != oh->mesg[u].type->id)
            continue;
	if(--sequence < 0)
            break;
    } /* end for */

    if(sequence >= 0)
	HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, UFAIL, "unable to find object header message")

    /*
     * Decode the message if necessary.  If the message is shared then decode
     * a shared message, ignoring the message type.
     */
    H5O_LOAD_NATIVE(f, dxpl_id, &(oh->mesg[u]), UFAIL)

    /*
     * Return the message type. If this is a shared message then return the
     * pointed-to type.
     */
    *type_p = oh->mesg[u].type;

    /* Set return value */
    ret_value = u;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_find_in_ohdr() */


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
    const void **new_mesg, hid_t dxpl_id, unsigned * oh_flags_ptr)
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
    HDassert(new_mesg);
    HDassert(new_type);
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
            if((orig_type->get_share)(f, orig_mesg, sh_mesg/*out*/) < 0)
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
    if(!(update_flags & H5O_UPDATE_DATA_ONLY))
        H5O_msg_reset_real(type, idx_msg->native);

    /* Copy the native value for the message */
    if(NULL == (idx_msg->native = (type->copy)(mesg, idx_msg->native, update_flags)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to copy message to object header")

    /* Update the message flags and mark the message as modified */
    idx_msg->flags = mesg_flags;
    idx_msg->dirty = TRUE;

    /* Update the modification time message if any */
    if(update_flags & H5O_UPDATE_TIME)
        H5O_touch_oh(f, dxpl_id, oh, FALSE, oh_flags_ptr);

    /* Mark the object header as modified */
    *oh_flags_ptr |= H5AC__DIRTIED_FLAG;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_write_mesg() */

