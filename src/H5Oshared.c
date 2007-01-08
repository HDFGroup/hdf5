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

/*
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Wednesday, April  1, 1998
 *
 * Purpose:	Functions that operate on a shared message.  The shared
 *		message doesn't ever actually appear in the object header as
 *		a normal message.  Instead, if a message is shared, the
 *		H5O_FLAG_SHARED bit is set and the message body is that
 *		defined here for H5O_SHARED.  The message ID is the ID of the
 *		pointed-to message and the pointed-to message is stored in
 *		the global heap.
 */

#define H5F_PACKAGE	/*suppress error about including H5Fpkg	  */
#define H5O_PACKAGE	/*suppress error about including H5Opkg	  */


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Gprivate.h"		/* Groups				*/
#include "H5HFprivate.h"        /* Fractal heap				*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5SMprivate.h"        /* Shared object header messages        */

static void *H5O_shared_decode(H5F_t*, hid_t dxpl_id, const uint8_t*);
static herr_t H5O_shared_encode(H5F_t*, uint8_t*, const void*);
static void *H5O_shared_copy(const void *_mesg, void *_dest);
static size_t H5O_shared_size(const H5F_t*, const void *_mesg);
static herr_t H5O_shared_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg,
    hbool_t adj_link);
static herr_t H5O_shared_link(H5F_t *f, hid_t dxpl_id, const void *_mesg);
static herr_t H5O_shared_pre_copy_file(H5F_t *file_src, const H5O_msg_class_t *type,
    const void *mesg_src, hbool_t *deleted, const H5O_copy_t *cpy_info, void *_udata);
static void *H5O_shared_copy_file(H5F_t *file_src, const H5O_msg_class_t *mesg_type,
    void *native_src, H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info, void *udata);
static herr_t H5O_shared_debug(H5F_t*, hid_t dxpl_id, const void*, FILE*, int, int);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_SHARED[1] = {{
    H5O_SHARED_ID,	    	/*message id number			*/
    "shared",		    	/*message name for debugging		*/
    sizeof(H5O_shared_t), 	/*native message size			*/
    H5O_shared_decode,	    	/*decode method				*/
    H5O_shared_encode,	    	/*encode method				*/
    H5O_shared_copy,		/*copy the native value			*/
    H5O_shared_size,	    	/*size method				*/
    NULL,		    	/*no reset method			*/
    NULL,		        /*no free method			*/
    H5O_shared_delete,		/*file delete method			*/
    H5O_shared_link,		/*link method				*/
    NULL,			/*get share method			*/
    NULL, 			/*set share method			*/
    NULL, 			/*is shared method		*/
    H5O_shared_pre_copy_file,	/* pre copy native value to file */
    H5O_shared_copy_file,	/* copy native value to file    */
    NULL,			/* post copy native value to file    */
    H5O_shared_debug	    	/*debug method				*/
}};

/* First version, with full symbol table entry as link for object header sharing */
#define H5O_SHARED_VERSION_1	1

/* Older version, with just address of object as link for object header sharing */
#define H5O_SHARED_VERSION_2	2

/* Newest version, which recognizes messages that are stored in the heap */
#define H5O_SHARED_VERSION_3	3
#define H5O_SHARED_VERSION	H5O_SHARED_VERSION_3

/* Size of stack buffer for serialized messages */
#define H5O_MESG_BUF_SIZE               128

/* Declare a free list to manage the serialized message information */
H5FL_BLK_DEFINE(ser_mesg);


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_read
 *
 * Purpose:	Reads a message referred to by a shared message.
 *
 * Return:	Success:	Ptr to message in native format.  The message
 *				should be freed by calling H5O_msg_reset().  If
 *				MESG is a null pointer then the caller should
 *				also call H5MM_xfree() on the return value
 *				after calling H5O_msg_reset().
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 24 2003
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_shared_read(H5F_t *f, hid_t dxpl_id, const H5O_shared_t *shared,
    const H5O_msg_class_t *type, void *mesg)
{
    H5HF_t *fheap = NULL;
    uint8_t mesg_buf[H5O_MESG_BUF_SIZE]; /* Buffer for deserializing messages */
    uint8_t *buf = NULL;        /* Pointer to raw message in heap */
    void *native_mesg = NULL;   /* Used for messages shared in heap */
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_read)

    /* check args */
    HDassert(f);
    HDassert(shared);
    HDassert(type);
    HDassert(type->set_share);

    /* This message could have a heap ID (SOHM) or the address of an object
     * header on disk (named datatype)
     */
    HDassert(shared->flags != H5O_NOT_SHARED);

    /* Check for implicit shared object header message */
    if(shared->flags & H5O_SHARED_IN_HEAP_FLAG) {
        haddr_t fheap_addr;
        size_t buf_size;

        /* Retrieve the fractal heap address for shared messages */
        if(H5SM_get_fheap_addr(f, dxpl_id, type->id, &fheap_addr) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, NULL, "can't get fheap address for shared messages")

        /* Open the fractal heap */
        if(NULL == (fheap = H5HF_open(f, dxpl_id, fheap_addr)))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, NULL, "unable to open fractal heap")

        /* Get the size of the message in the heap */
        if(H5HF_get_obj_len(fheap, dxpl_id, &(shared->u.heap_id), &buf_size) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, NULL, "can't get message size from fractal heap.")

        /* Allocate space for serialized message, if necessary */
        if(buf_size > sizeof(mesg_buf)) {
            if(NULL == (buf = H5FL_BLK_MALLOC(ser_mesg, buf_size)))
                HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "memory allocation failed")
        } /* end if */
        else
            buf = mesg_buf;

        /* Retrieve the message from the heap */
        if(H5HF_read(fheap, dxpl_id, &(shared->u.heap_id), buf) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "can't read message from fractal heap.")

        /* Decode the message */
        if(NULL == (native_mesg = H5O_msg_decode(f, dxpl_id, type->id, buf)))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, NULL, "can't decode shared message.")

        /* Copy this message to the user's buffer */
        if(NULL == (ret_value = (type->copy)(native_mesg, mesg)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy message to user space")
    } /* end if */
    else {
        HDassert(shared->flags & H5O_COMMITTED_FLAG);

        /* Get the shared message from an object header */
        if(NULL == (ret_value = H5O_msg_read(&(shared->u.oloc), type->id, 0, mesg, dxpl_id)))
            HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to read message")
    } /* end else */

    /* Mark the message as shared */
    if(type->set_share && (type->set_share)(ret_value, shared) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to set sharing information")

done:
    /* Release resources */
    if(buf && buf != mesg_buf)
        H5FL_BLK_FREE(ser_mesg, buf);
    if(native_mesg)
        H5O_msg_free(type->id, native_mesg);
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_read() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_link_adj
 *
 * Purpose:	Changes the link count for the object referenced by a shared
 *              message.
 *
 * Return:	Success:	New link count
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 26 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5O_shared_link_adj(H5F_t *f, hid_t dxpl_id, const H5O_shared_t *shared, int adjust)
{
    int ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_link_adj)

    /* check args */
    HDassert(f);
    HDassert(shared);

    /*
     * The shared message is stored in some other object header.
     * The other object header must be in the same file as the
     * new object header. Adjust the reference count on that
     * object header.
     */
    if(shared->flags & H5O_COMMITTED_FLAG)
    {
        if(shared->u.oloc.file->shared != f->shared)
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "interfile hard links are not allowed")
        if((ret_value = H5O_link(&(shared->u.oloc), adjust, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared object link count")
    }
    else
    {
        HDassert(shared->flags & H5O_SHARED_IN_HEAP_FLAG);
        ret_value = 1; /* JAMES temp refcount*/
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_link_adj() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_decode
 *
 * Purpose:	Decodes a shared object message and returns it.
 *
 * Return:	Success:	Ptr to a new shared object message.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *	Robb Matzke, 1998-07-20
 *	Added a version number to the beginning of the message.
 *-------------------------------------------------------------------------
 */
static void *
H5O_shared_decode(H5F_t *f, hid_t UNUSED dxpl_id, const uint8_t *buf)
{
    H5O_shared_t	*mesg = NULL;
    unsigned		version;
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_decode)

    /* Check args */
    HDassert(f);
    HDassert(buf);

    /* Decode */
    if(NULL == (mesg = H5MM_calloc (sizeof(H5O_shared_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Version */
    version = *buf++;
    if(version < H5O_SHARED_VERSION_1 || version > H5O_SHARED_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for shared object message")

    /* Get the shared information flags
     * Flags are unused before version 3.
     */
    if(version >= H5O_SHARED_VERSION_2) {
        mesg->flags = *buf++;
    }
    else {
        mesg->flags = H5O_COMMITTED_FLAG;
        buf++;
    } /* end else */

    /* Skip reserved bytes (for version 1) */
    if(version == H5O_SHARED_VERSION_1)
        buf += 6;

    /* Body */
    if(version == H5O_SHARED_VERSION_1) {
        H5G_obj_ent_decode(f, &buf, &(mesg->u.oloc));
    }
    else if (version >= H5O_SHARED_VERSION_2) {
        /* If this message is in the heap, copy a heap ID.
         * Otherwise, it is a named datatype, so copy an H5O_loc_t.
         */
        if(mesg->flags & H5O_SHARED_IN_HEAP_FLAG) {
            HDassert(version >= H5O_SHARED_VERSION_3 );
            HDmemcpy(&(mesg->u.heap_id), buf, (size_t) H5SM_FHEAP_ID_LEN);
        }
        else {
            /* The H5O_COMMITTED_FLAG should be set if this message
             * is from an older version before the flag existed.
             */
            if(version < H5O_SHARED_VERSION_3)
                mesg->flags = H5O_COMMITTED_FLAG;

            HDassert(mesg->flags & H5O_COMMITTED_FLAG);

            H5F_addr_decode(f, &buf, &(mesg->u.oloc.addr));
            mesg->u.oloc.file = f;
        } /* end else */
    } /* end else if */

    /* Set return value */
    ret_value = mesg;

done:
    if(ret_value == NULL) {
        if(mesg != NULL)
            H5MM_xfree(mesg);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_encode
 *
 * Purpose:	Encodes message _MESG into buffer BUF.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *	Robb Matzke, 1998-07-20
 *	Added a version number to the beginning of the message.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shared_encode(H5F_t *f, uint8_t *buf/*out*/, const void *_mesg)
{
    const H5O_shared_t	*mesg = (const H5O_shared_t *)_mesg;
    unsigned    version;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shared_encode)

    /* Check args */
    HDassert(f);
    HDassert(buf);
    HDassert(mesg);

    /* If this message is shared in the heap, we need to use version 3 of the
     * encoding and encode the SHARED_IN_HEAP flag.
     */
    /* JAMES: also use "use latest version" flag here */
    if(mesg->flags & H5O_SHARED_IN_HEAP_FLAG) {
        version = H5O_SHARED_VERSION;
    }
    else {
        HDassert(mesg->flags & H5O_COMMITTED_FLAG);
        version = H5O_SHARED_VERSION_2; /* version 1 is no longer used */
    } /* end else */

    *buf++ = version;
    *buf++ = (unsigned)mesg->flags;

    /* Encode either the heap ID of the message or the address of the
     * object header that holds it.
     */
    if(mesg->flags & H5O_SHARED_IN_HEAP_FLAG) {
        HDmemcpy(buf, &(mesg->u.heap_id), (size_t) H5SM_FHEAP_ID_LEN);
    }
     else {
        H5F_addr_encode(f, &buf, mesg->u.oloc.addr);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_shared_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_copy
 *
 * Purpose:	Copies a message from _MESG to _DEST, allocating _DEST if
 *		necessary.
 *
 * Return:	Success:	Ptr to _DEST
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 26 2003
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_shared_copy(const void *_mesg, void *_dest)
{
    const H5O_shared_t  *mesg = (const H5O_shared_t *) _mesg;
    H5O_shared_t	*dest = (H5O_shared_t *) _dest;
    void        *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_copy)

    /* check args */
    HDassert(mesg);
    if(!dest && NULL == (dest = H5MM_malloc(sizeof(H5O_shared_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy */
    dest->flags = mesg->flags;
    if(mesg->flags & H5O_COMMITTED_FLAG)
        H5O_loc_copy(&(dest->u.oloc), &(mesg->u.oloc), H5_COPY_DEEP);
    else if(mesg->flags & H5O_SHARED_IN_HEAP_FLAG)
        dest->u.heap_id = mesg->u.heap_id;
    else
        /* This message's sharing information is being reset */
        HDassert(mesg->flags == H5O_NOT_SHARED);

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_size
 *
 * Purpose:	Returns the length of a shared object message.
 *
 * Return:	Success:	Length
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_shared_size(const H5F_t *f, const void *_mesg)
{
    const H5O_shared_t       *shared = (const H5O_shared_t *) _mesg;
    size_t	ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shared_size)

    if(shared->flags & H5O_COMMITTED_FLAG)
    {
        ret_value = 1 +			/*version			*/
                1 +				/*the flags field		*/
                H5F_SIZEOF_ADDR(f);		/*sharing by another obj hdr	*/
    }
    else
    {
        HDassert(shared->flags & H5O_SHARED_IN_HEAP_FLAG);
        ret_value = 1 +			/*version			*/
                1 +				/*the flags field	*/
                H5SM_FHEAP_ID_LEN;		/* Shared in the heap   */
    }

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5O_shared_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, September 26, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shared_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg, 
    hbool_t adj_link)
{
    const H5O_shared_t       *shared = (const H5O_shared_t *) _mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_delete)

    /* check args */
    HDassert(f);
    HDassert(shared);

    /* 
     * Committed datatypes increment the OH of the original message when they
     * are written (in H5O_shared_link) and decrement it here.
     * SOHMs in the heap behave differently; their refcount is incremented
     * during H5SM_share when they are going to be written (in H5O_msg_append
     * or H5O_msg_write). Their refcount in the SOHM indexes still needs to
     * be decremented when they're deleted (in H5O_shared_link_adj).
     */

    /* Decrement the reference count on the shared object, if requested */
    if(adj_link)
        if(H5O_shared_link_adj(f, dxpl_id, shared, -1) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared object link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5O_shared_link
 *
 * Purpose:     Increment reference count on any objects referenced by
 *              message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, September 26, 2003
 *
 *-------------------------------------------------------------------------
 */
/* JAMES: this is where shared messages increment their links */
static herr_t
H5O_shared_link(H5F_t *f, hid_t dxpl_id, const void *_mesg)
{
    const H5O_shared_t       *shared = (const H5O_shared_t *) _mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_link)

    /* check args */
    HDassert(f);
    HDassert(shared);

    /* Increment the reference count on the shared object */
    if(H5O_shared_link_adj(f, dxpl_id, shared, 1) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared object link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_link() */


/*-------------------------------------------------------------------------
 * Function:    H5O_shared_pre_copy_file
 *
 * Purpose:     Perform any necessary actions before copying message between
 *              files for shared messages.
 *
 * Return:      Success:        Non-negative
 *
 *              Failure:        Negative
 *
 * Programmer:  Peter Cao
 *              Saturday, February 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shared_pre_copy_file(H5F_t *file_src, const H5O_msg_class_t *type,
    const void *native_src, hbool_t *deleted, const H5O_copy_t *cpy_info,
    void *udata)
{
    const H5O_shared_t   *shared_src = (const H5O_shared_t *)native_src;
    void           *mesg_native = NULL;
    herr_t         ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_pre_copy_file)

    /* check args */
    HDassert(file_src);
    HDassert(type);

    if(type->pre_copy_file) {
        /* Go get the actual shared message */
        if(NULL == (mesg_native = H5O_shared_read(file_src, H5AC_dxpl_id, shared_src, type, NULL)))
            HGOTO_ERROR(H5E_OHDR, H5E_READERROR, FAIL, "unable to load object header")

        /* Perform "pre copy" operation on messge */
        if((type->pre_copy_file)(file_src, type, mesg_native, deleted, cpy_info, udata) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to perform 'pre copy' operation on message")
    } /* end of if */

done:
    if(mesg_native)
        H5O_msg_free_real(type, mesg_native);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shared_pre_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5O_shared_copy_file
 *
 * Purpose:     Copies a message from _MESG to _DEST in file
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              November 1, 2005
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_shared_copy_file(H5F_t *file_src, const H5O_msg_class_t *mesg_type,
    void *native_src, H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info,
    void *udata)
{
    H5O_shared_t        *shared_src = (H5O_shared_t *)native_src;
    H5O_shared_t        *shared_dst = NULL;     /* The destination message if
                                                 * it is a shared message */
    void                *dst_mesg = NULL;       /* The destination message if
                                                 * it's an unshared message */
    void                *ret_value;             /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_shared_copy_file)

    /* check args */
    HDassert(shared_src);
    HDassert(file_dst);
    HDassert(cpy_info);


    /* Committed shared messages create a shared message at the destination
     * and also copy the committed object that they point to.
     * SOHMs actually write a non-shared message at the destination.
     */
    if(shared_src->flags & H5O_COMMITTED_FLAG)
    {
        /* Allocate space for the destination message */
        if(NULL == (shared_dst = H5MM_malloc(sizeof(H5O_shared_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* Reset group entry for new object */
        H5O_loc_reset(&(shared_dst->u.oloc));
        shared_dst->u.oloc.file = file_dst;
    
        /* Set flags for new shared object */
        shared_dst->flags = shared_src->flags;

        /* Copy the shared object from source to destination */
        if(H5O_copy_header_map(&(shared_src->u.oloc), &(shared_dst->u.oloc), dxpl_id, cpy_info, FALSE) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy object")

        /* Set return value */
        ret_value = shared_dst;
    }
    else
    {
        HDassert(shared_src->flags & H5O_SHARED_IN_HEAP_FLAG);

        /* Read the shared message to get the original message */
        if(NULL == (dst_mesg = H5O_shared_read(file_src, dxpl_id, shared_src, mesg_type, NULL)))
            HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, NULL, "unable to read shared message")

        if(mesg_type->copy_file) {
            /* Copy the original, un-shared message and return it */
            ret_value = (mesg_type->copy_file)(file_src, mesg_type, dst_mesg, file_dst, dxpl_id, cpy_info, udata);
            H5O_msg_free(mesg_type->id, dst_mesg);
        }
        else {
            ret_value = dst_mesg;
        }
    }

done:
    if(!ret_value)
    {
        if(shared_dst)
            H5O_msg_free(H5O_SHARED_ID, shared_dst);
        if(dst_mesg)
            H5O_msg_free(mesg_type->id, dst_mesg);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_shared_copy_file() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_debug
 *
 * Purpose:	Prints debugging info for the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shared_debug (H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg,
		  FILE *stream, int indent, int fwidth)
{
    const H5O_shared_t	*mesg = (const H5O_shared_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shared_debug)

    /* JAMES_HEAP: this oughta change, too, of course. */

    /* Check args */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    if(mesg->flags & H5O_COMMITTED_FLAG)
    {
        HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Sharing method",
                "Obj Hdr");
        HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                "Object address:",
                mesg->u.oloc.addr);
    }
    else
    {
        HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Sharing method",
                "SOHM Heap");
        HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                "Heap ID:",
                mesg->u.heap_id);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_shared_debug() */

