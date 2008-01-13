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
 * Created:		H5Ocache.c
 *			Sep 28 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Object header metadata cache virtual functions.
 *
 *-------------------------------------------------------------------------
 */

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5O_cache_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Opkg.h"             /* Object headers			*/

/* Private typedefs */

/* PRIVATE PROTOTYPES */

/* Metadata cache callbacks */
static H5O_t *H5O_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_udata1,
		       void *_udata2);
static herr_t H5O_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5O_t *oh);
static herr_t H5O_clear(H5F_t *f, H5O_t *oh, hbool_t destroy);
static herr_t H5O_compute_size(const H5F_t *f, const H5O_t *oh, size_t *size_ptr);

/* H5O inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_OHDR[1] = {{
    H5AC_OHDR_ID,
    (H5AC_load_func_t)H5O_load,
    (H5AC_flush_func_t)H5O_flush,
    (H5AC_dest_func_t)H5O_dest,
    (H5AC_clear_func_t)H5O_clear,
    (H5AC_size_func_t)H5O_compute_size,
}};


/*-------------------------------------------------------------------------
 * Function:	H5O_cache_init_interface
 *
 * Purpose:	Initialize the H5O interface.  (Just calls
 *                  H5O_init_iterface currently).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, September 28, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_cache_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_cache_init_interface)

    FUNC_LEAVE_NOAPI(H5O_init())
} /* end H5O_cache_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5O_flush_msgs
 *
 * Purpose:	Flushes messages for object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Nov 21 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_flush_msgs(H5F_t *f, hid_t dxpl_id, H5O_t *oh)
{
    uint8_t	*p;             /* Temporary pointer to encode with */
    int	id;                     /* ID of message to encode */
    H5O_mesg_t *curr_msg;       /* Pointer to current message being operated on */
    herr_t	(*encode)(H5F_t*, uint8_t*, const void*) = NULL;
    unsigned	u;              /* Local index variable */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_flush_msgs)

    /* check args */
    HDassert(f);
    HDassert(oh);

    /* Encode any dirty messages */
    for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
        if(curr_msg->dirty) {
            p = curr_msg->raw - H5O_SIZEOF_MSGHDR(f);

            id = curr_msg->type->id;
            UINT16ENCODE(p, id);
            HDassert(curr_msg->raw_size < H5O_MAX_SIZE);
            UINT16ENCODE(p, curr_msg->raw_size);
            *p++ = curr_msg->flags;
            *p++ = 0; /*reserved*/
            *p++ = 0; /*reserved*/
            *p++ = 0; /*reserved*/

            if(curr_msg->native) {
                HDassert(curr_msg->type->encode);

                /* allocate file space for chunks that have none yet */
                if(H5O_CONT_ID == id && !H5F_addr_defined(((H5O_cont_t *)(curr_msg->native))->addr)) {
                    H5O_cont_t *cont = (H5O_cont_t *) (curr_msg->native);
                    assert(cont->chunkno < oh->nchunks);
                    assert(!H5F_addr_defined(oh->chunk[cont->chunkno].addr));
                    cont->size = oh->chunk[cont->chunkno].size;

                    /* Free the space we'd reserved in the file to hold this chunk */
                    H5MF_free_reserved(f, (hsize_t)cont->size);

                    if (HADDR_UNDEF==(cont->addr=H5MF_alloc(f, H5FD_MEM_OHDR, dxpl_id, (hsize_t)cont->size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate space for object header data");
                    oh->chunk[cont->chunkno].addr = cont->addr;
                } /* end if */

                /*
                 * Encode the message.  If the message is shared then we
                 * encode a Shared Object message instead of the object
                 * which is being shared.
                 */
                HDassert(curr_msg->raw >= oh->chunk[curr_msg->chunkno].image);
                HDassert(curr_msg->raw_size == H5O_ALIGN (curr_msg->raw_size));
                HDassert(curr_msg->raw + curr_msg->raw_size <=
                       oh->chunk[curr_msg->chunkno].image + oh->chunk[curr_msg->chunkno].size);
                if(curr_msg->flags & H5O_FLAG_SHARED)
                    encode = H5O_MSG_SHARED->encode;
                else
                    encode = curr_msg->type->encode;

#ifndef NDEBUG
/* Sanity check */
if(!(curr_msg->flags & H5O_FLAG_SHARED)) {
    size_t msg_size;

    /* Check for encoded message bigger than space available */
    msg_size = curr_msg->type->raw_size(f, curr_msg->native);
    msg_size = H5O_ALIGN(msg_size);
    HDassert(msg_size <= curr_msg->raw_size);
} /* end if */
#endif /* NDEBUG */

                if((encode)(f, curr_msg->raw, curr_msg->native) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode object header message")
            } /* end if */
            curr_msg->dirty = FALSE;
            oh->chunk[curr_msg->chunkno].dirty = TRUE;
        } /* end if */
    } /* end for */

    /* Sanity check for the correct # of messages in object header */
    if(oh->nmesgs != u)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFLUSH, FAIL, "corrupt object header - too few messages")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_flush_msgs() */


/*-------------------------------------------------------------------------
 * Function:	H5O_load
 *
 * Purpose:	Loads an object header from disk.
 *
 * Return:	Success:	Pointer to the new object header.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 *-------------------------------------------------------------------------
 */
static H5O_t *
H5O_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED * _udata1,
	 void UNUSED * _udata2)
{
    H5O_t	*oh = NULL;
    H5O_t	*ret_value;
    uint8_t	buf[16], *p;
    size_t	mesg_size;
    size_t	hdr_size;
    unsigned	id;
    int	mesgno;
    unsigned	curmesg = 0, nmesgs;
    unsigned	chunkno;
    unsigned    skipped_msgs = 0;       /* Number of unknown messages skipped */
    unsigned    merged_null_msgs = 0;   /* Number of null messages merged together */
    haddr_t	chunk_addr;
    size_t	chunk_size;
    uint8_t	flags;

    FUNC_ENTER_NOAPI(H5O_load, NULL)

    /* check args */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(!_udata1);
    HDassert(!_udata2);

    /* allocate ohdr and init chunk list */
    if (NULL==(oh = H5FL_CALLOC(H5O_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* read fixed-lenth part of object header */
    hdr_size = H5O_SIZEOF_HDR(f);
    assert(hdr_size<=sizeof(buf));
    if (H5F_block_read(f, H5FD_MEM_OHDR, addr, hdr_size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to read object header");
    p = buf;

    /* decode version */
    oh->version = *p++;
    if (H5O_VERSION != oh->version)
	HGOTO_ERROR(H5E_OHDR, H5E_VERSION, NULL, "bad object header version number");

    /* reserved */
    p++;

    /* decode number of messages */
    UINT16DECODE(p, nmesgs);

    /* decode link count */
    UINT32DECODE(p, oh->nlink);

    /* decode first chunk info */
    chunk_addr = addr + (hsize_t)hdr_size;
    UINT32DECODE(p, chunk_size);

    /* build the message array */
    oh->alloc_nmesgs = nmesgs;
    if (NULL==(oh->mesg=H5FL_SEQ_CALLOC(H5O_mesg_t,(size_t)oh->alloc_nmesgs)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* read each chunk from disk */
    while(H5F_addr_defined(chunk_addr)) {
	/* increase chunk array size */
	if(oh->nchunks >= oh->alloc_nchunks) {
	    unsigned na = oh->alloc_nchunks + H5O_NCHUNKS;
	    H5O_chunk_t *x = H5FL_SEQ_REALLOC (H5O_chunk_t, oh->chunk, (size_t)na);

	    if(!x)
                HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
	    oh->alloc_nchunks = na;
	    oh->chunk = x;
	} /* end if */

	/* read the chunk raw data */
	chunkno = oh->nchunks++;
	oh->chunk[chunkno].dirty = FALSE;
	oh->chunk[chunkno].addr = chunk_addr;
	oh->chunk[chunkno].size = chunk_size;
	if(NULL==(oh->chunk[chunkno].image = H5FL_BLK_MALLOC(chunk_image, chunk_size)))
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
	if(H5F_block_read(f, H5FD_MEM_OHDR, chunk_addr, chunk_size, dxpl_id, oh->chunk[chunkno].image) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to read object header data");

	/* load messages from this chunk */
	for(p = oh->chunk[chunkno].image; p < oh->chunk[chunkno].image + chunk_size; p += mesg_size) {
	    UINT16DECODE(p, id);
	    UINT16DECODE(p, mesg_size);
	    HDassert(mesg_size==H5O_ALIGN (mesg_size));
	    flags = *p++;
	    p += 3; /*reserved*/

            /* Try to detect invalidly formatted object header messages */
	    if(p + mesg_size > oh->chunk[chunkno].image + chunk_size)
		HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "corrupt object header")

            /* Skip header messages we don't know about */
            /* (Usually from future versions of the library */
	    if(id >= NELMTS(H5O_msg_class_g) || NULL == H5O_msg_class_g[id]) {
                skipped_msgs++;
                continue;
            } /* end if */

            if((H5F_get_intent(f) & H5F_ACC_RDWR) &&
	            H5O_NULL_ID == id && oh->nmesgs > 0 &&
                    H5O_NULL_ID == oh->mesg[oh->nmesgs - 1].type->id &&
                    oh->mesg[oh->nmesgs - 1].chunkno == chunkno) {
		/* combine adjacent null messages */
		mesgno = oh->nmesgs - 1;
		oh->mesg[mesgno].raw_size += H5O_SIZEOF_MSGHDR(f) + mesg_size;
		oh->mesg[mesgno].dirty = TRUE;
                merged_null_msgs++;
	    } else {
		/* Check if we need to extend message table to hold the new message */
		if(oh->nmesgs >= oh->alloc_nmesgs)
                    if(H5O_alloc_msgs(oh, (size_t)1) < 0)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate more space for messages")
		mesgno = oh->nmesgs++;
		oh->mesg[mesgno].type = H5O_msg_class_g[id];
		oh->mesg[mesgno].dirty = FALSE;
		oh->mesg[mesgno].flags = flags;
		oh->mesg[mesgno].native = NULL;
		oh->mesg[mesgno].raw = p;
		oh->mesg[mesgno].raw_size = mesg_size;
		oh->mesg[mesgno].chunkno = chunkno;
	    } /* end else */
	} /* end for */

        HDassert(p == oh->chunk[chunkno].image + chunk_size);

        /* decode next object header continuation message */
        for(chunk_addr = HADDR_UNDEF; !H5F_addr_defined(chunk_addr) && curmesg < oh->nmesgs; ++curmesg) {
            if(H5O_CONT_ID == oh->mesg[curmesg].type->id) {
                H5O_cont_t *cont;

                cont = (H5O_MSG_CONT->decode) (f, dxpl_id, oh->mesg[curmesg].raw);
                oh->mesg[curmesg].native = cont;
                chunk_addr = cont->addr;
                chunk_size = cont->size;
                cont->chunkno = oh->nchunks;	/*the next chunk to allocate */
            } /* end if */
        } /* end for */
    } /* end while */

    /* Mark the object header dirty if we've merged a message */
    if(merged_null_msgs)
	oh->cache_info.is_dirty = TRUE;

/* Don't check for the incorrect # of object header messages bug unless we've
 * enabled strict format checking.  This allows for older files, created with
 * a version of the library that had a bug in tracking the correct # of header
 * messages to be read in without the library fussing about things. -QAK
 */
#ifdef H5_STRICT_FORMAT_CHECKS
    /* Sanity check for the correct # of messages in object header */
    if((oh->nmesgs + skipped_msgs + merged_null_msgs) != nmesgs)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "corrupt object header - too few messages")
#else /* H5_STRICT_FORMAT_CHECKS */
    /* Check for incorrect # of messages in object header and if we have write
     * access on the file, flag the object header as dirty, so it gets fixed. 
     */
    if((oh->nmesgs + skipped_msgs + merged_null_msgs) != nmesgs &&
            (H5F_get_intent(f) & H5F_ACC_RDWR))
	oh->cache_info.is_dirty = TRUE;
#endif /* H5_STRICT_FORMAT_CHECKS */

    /* Set return value */
    ret_value = oh;

done:
    if(!ret_value && oh) {
        if(H5O_dest(f,oh) < 0)
	    HDONE_ERROR(H5E_OHDR, H5E_CANTFREE, NULL, "unable to destroy object header data")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_load() */


/*-------------------------------------------------------------------------
 * Function:	H5O_flush
 *
 * Purpose:	Flushes (and destroys) an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5O_t *oh)
{
    uint8_t	buf[16], *p;
    hbool_t combine = FALSE;    /* Whether to combine the object header prefix & the first chunk */
    unsigned	u;              /* Local index variable */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_flush, FAIL)

    /* check args */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(oh);

    /* flush */
    if(oh->cache_info.is_dirty) {
	/* Encode any dirty messages */
        if(H5O_flush_msgs(f, dxpl_id, oh) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTFLUSH, FAIL, "unable to flush object header messages")

        /* Encode header prefix */
	p = buf;

	/* encode version */
	*p++ = oh->version;

	/* reserved */
	*p++ = 0;

	/* encode number of messages */
	UINT16ENCODE(p, oh->nmesgs);

	/* encode link count */
	UINT32ENCODE(p, oh->nlink);

	/* encode body size */
	UINT32ENCODE(p, oh->chunk[0].size);

	/* zero to alignment */
	HDmemset (p, 0, (size_t)(H5O_SIZEOF_HDR(f)-12));

	/* write the object header prefix */

        /* Check if we can combine the object header prefix & the first chunk into one I/O operation */
        if(oh->chunk[0].dirty && (addr + H5O_SIZEOF_HDR(f)) == oh->chunk[0].addr) {
            combine = TRUE;
        } /* end if */
        else {
            if(H5F_block_write(f, H5FD_MEM_OHDR, addr, (size_t)H5O_SIZEOF_HDR(f), dxpl_id, buf) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header hdr to disk")
        } /* end else */

	/* write each chunk to disk */
	for(u = 0; u < oh->nchunks; u++) {
	    if(oh->chunk[u].dirty) {
                HDassert(H5F_addr_defined(oh->chunk[u].addr));
                if(u == 0 && combine) {
                    /* Allocate space for the combined prefix and first chunk */
                    if((p = H5FL_BLK_MALLOC(chunk_image,(H5O_SIZEOF_HDR(f)+oh->chunk[u].size))) == NULL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

                    /* Copy in the prefix */
                    HDmemcpy(p, buf, (size_t)H5O_SIZEOF_HDR(f));

                    /* Copy in the first chunk */
                    HDmemcpy(p + H5O_SIZEOF_HDR(f), oh->chunk[u].image, oh->chunk[u].size);

                    /* Write the combined prefix/chunk out */
                    if(H5F_block_write(f, H5FD_MEM_OHDR, addr,
                                (H5O_SIZEOF_HDR(f) + oh->chunk[u].size), dxpl_id, p) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header data to disk")

                    /* Release the memory for the combined prefix/chunk */
                    p = H5FL_BLK_FREE(chunk_image,p);
                } /* end if */
                else {
                    if(H5F_block_write(f, H5FD_MEM_OHDR, oh->chunk[u].addr,
                                (oh->chunk[u].size), dxpl_id, oh->chunk[u].image) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header data to disk")
                } /* end else */
                oh->chunk[u].dirty = FALSE;
	    } /* end if */
	} /* end for */
	oh->cache_info.is_dirty = FALSE;
    } /* end if */

    if (destroy) {
        if(H5O_dest(f,oh) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to destroy object header data")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5O_dest
 *
 * Purpose:	Destroys an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jan 15 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_dest(H5F_t UNUSED *f, H5O_t *oh)
{
    unsigned	i;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_dest);

    /* check args */
    assert(oh);

    /* Verify that node is clean */
    assert (oh->cache_info.is_dirty==FALSE);

    /* destroy chunks */
    for (i = 0; i < oh->nchunks; i++) {
        /* Verify that chunk is clean */
        assert (oh->chunk[i].dirty==0);

        oh->chunk[i].image = H5FL_BLK_FREE(chunk_image,oh->chunk[i].image);
    }
    if(oh->chunk)
        oh->chunk = H5FL_SEQ_FREE(H5O_chunk_t,oh->chunk);

    /* destroy messages */
    for (i = 0; i < oh->nmesgs; i++) {
        /* Verify that message is clean */
        assert (oh->mesg[i].dirty==0);

        H5O_free_mesg(&oh->mesg[i]);
    }
    if(oh->mesg)
        oh->mesg = H5FL_SEQ_FREE(H5O_mesg_t,oh->mesg);

    /* destroy object header */
    H5FL_FREE(H5O_t,oh);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* end H5O_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5O_clear
 *
 * Purpose:	Mark a object header in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 20 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_clear(H5F_t *f, H5O_t *oh, hbool_t destroy)
{
    unsigned	u;      /* Local index variable */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5O_clear);

    /* check args */
    assert(oh);

    /* Mark chunks as clean */
    for (u = 0; u < oh->nchunks; u++)
        oh->chunk[u].dirty=FALSE;

    /* Mark messages as clean */
    for (u = 0; u < oh->nmesgs; u++)
        oh->mesg[u].dirty=FALSE;

    /* Mark whole header as clean */
    oh->cache_info.is_dirty=FALSE;

    if (destroy)
        if (H5O_dest(f, oh) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to destroy object header data");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5O_compute_size
 *
 * Purpose:	Compute the size in bytes of the specified instance of
 *              H5O_t on disk, and return it in *len_ptr.  On failure,
 *              the value of *len_ptr is undefined.
 *
 *		The value returned will probably be low unless the object
 *		has just been flushed, as we simply total up the size of
 *		the header with the sizes of the chunks.  Thus any message
 *		that has been added since the last flush will not be
 *		reflected in the total.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/13/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_compute_size(const H5F_t *f, const H5O_t *oh, size_t *size_ptr)
{
    unsigned	u;
    size_t	size;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_compute_size);

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(size_ptr);

    size = H5O_SIZEOF_HDR(f);

    for (u = 0; u < oh->nchunks; u++)
        size += oh->chunk[u].size;

    HDassert(size >= H5O_SIZEOF_HDR(f));

    *size_ptr = size;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5O_compute_size() */

