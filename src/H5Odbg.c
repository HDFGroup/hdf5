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
 * Created:		H5Odbg.c
 *			Nov 17 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Object header debugging routines.
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
#include "H5MMprivate.h"	/* Memory management			*/
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


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


#ifdef H5O_DEBUG

/*-------------------------------------------------------------------------
 * Function:	H5O_assert
 *
 * Purpose:	Sanity check the information for an object header data
 *              structure.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_assert(const H5O_t *oh)
{
    H5O_mesg_t *curr_msg;               /* Pointer to current message to examine */
    H5O_mesg_t *tmp_msg;                /* Pointer to temporary message to examine */
    size_t meta_space;                  /* Size of header metadata */
    size_t mesg_space;                  /* Size of message raw data */
    size_t free_space;                  /* Size of free space in header */
    size_t hdr_size;                    /* Size of header's chunks */
    unsigned u, v;                      /* Local index variables */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_assert)

    /* Initialize the space tracking variables */
    hdr_size = 0;
    meta_space = H5O_SIZEOF_HDR_OH(oh) + (H5O_SIZEOF_CHKHDR_OH(oh) * (oh->nchunks - 1));
    mesg_space = 0;
    free_space = 0;

    /* Loop over all chunks in object header */
    for(u = 0; u < oh->nchunks; u++) {
        /* Accumulate the size of the header on header */
        hdr_size += oh->chunk[u].size;

        /* If the chunk has a gap, add it to the free space */
        free_space += oh->chunk[u].gap;

        /* Check for valid raw data image */
        HDassert(oh->chunk[u].image);
        HDassert(oh->chunk[u].size > (size_t)H5O_SIZEOF_CHKHDR_OH(oh));

        /* All chunks must be allocated on disk */
        HDassert(H5F_addr_defined(oh->chunk[u].addr));

        /* Version specific checks */
        if(oh->version > H5O_VERSION_1) {
            /* Make certain that the magic number is correct for each chunk */
            HDassert(!HDmemcmp(oh->chunk[u].image, (u == 0 ? H5O_HDR_MAGIC : H5O_CHK_MAGIC), H5O_SIZEOF_MAGIC));

            /* Check for valid gap size */
            HDassert(oh->chunk[u].gap < (size_t)H5O_SIZEOF_MSGHDR_OH(oh));
        } /* end if */
        else
            /* Gaps should never occur in version 1 of the format */
            HDassert(oh->chunk[u].gap == 0);
    } /* end for */

    /* Loop over all messages in object header */
    for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
        /* Accumulate information, based on the type of message */
	if(H5O_NULL_ID == curr_msg->type->id)
            free_space += H5O_SIZEOF_MSGHDR_OH(oh) + curr_msg->raw_size;
        else if(H5O_CONT_ID == curr_msg->type->id)
            meta_space += H5O_SIZEOF_MSGHDR_OH(oh) + curr_msg->raw_size;
        else {
            meta_space += H5O_SIZEOF_MSGHDR_OH(oh);
            mesg_space += curr_msg->raw_size;
        } /* end else */

        /* Make certain that the message is in a valid chunk */
        HDassert(curr_msg->chunkno < oh->nchunks);

        /* Make certain null messages aren't in chunks with gaps */
        if(H5O_NULL_ID == curr_msg->type->id)
            HDassert(oh->chunk[curr_msg->chunkno].gap == 0);

        /* Make certain that the message is completely in a chunk message area */
        HDassert(curr_msg->raw_size <= (oh->chunk[curr_msg->chunkno].size) - (H5O_SIZEOF_CHKSUM_OH(oh) + oh->chunk[curr_msg->chunkno].gap));
        if(curr_msg->chunkno == 0)
            HDassert(curr_msg->raw >= oh->chunk[curr_msg->chunkno].image + (H5O_SIZEOF_HDR_OH(oh) - H5O_SIZEOF_CHKSUM_OH(oh)));
        else
            HDassert(curr_msg->raw >= oh->chunk[curr_msg->chunkno].image + (H5O_SIZEOF_CHKHDR_OH(oh) - H5O_SIZEOF_CHKSUM_OH(oh)));
        HDassert(curr_msg->raw + curr_msg->raw_size <= (oh->chunk[curr_msg->chunkno].image + oh->chunk[curr_msg->chunkno].size) - (H5O_SIZEOF_CHKSUM_OH(oh) + oh->chunk[curr_msg->chunkno].gap));

        /* Make certain that no other messages overlap this message */
        for(v = 0, tmp_msg = &oh->mesg[0]; v < oh->nmesgs; v++, tmp_msg++) {
            if(u != v)
                HDassert(!(tmp_msg->raw >= curr_msg->raw && tmp_msg->raw < (curr_msg->raw + curr_msg->raw_size)));
        } /* end for */
    } /* end for */

    /* Sanity check that all the bytes are accounted for */
    HDassert(hdr_size == (free_space + meta_space + mesg_space + oh->skipped_mesg_size));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_assert() */
#endif /* H5O_DEBUG */


/*-------------------------------------------------------------------------
 * Function:	H5O_debug_id
 *
 * Purpose:	Act as a proxy for calling the 'debug' method for a
 *              particular class of object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 13 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_debug_id(unsigned type_id, H5F_t *f, hid_t dxpl_id, const void *mesg, FILE *stream, int indent, int fwidth)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    herr_t      ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_debug_id,FAIL)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->debug);
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /* Call the debug method in the class */
    if((ret_value = (type->debug)(f, dxpl_id, mesg, stream, indent, fwidth)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADTYPE, FAIL, "unable to debug message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_debug_id() */


/*-------------------------------------------------------------------------
 * Function:	H5O_debug_real
 *
 * Purpose:	Prints debugging info about an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_debug_real(H5F_t *f, hid_t dxpl_id, H5O_t *oh, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    unsigned	i, chunkno;
    size_t	mesg_total = 0, chunk_total = 0;
    int		*sequence;
    void	*(*decode)(H5F_t*, hid_t, const uint8_t*);
    herr_t      (*debug)(H5F_t*, hid_t, const void*, FILE*, int, int)=NULL;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_debug_real, FAIL)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /* debug */
    HDfprintf(stream, "%*sObject Header...\n", indent, "");

    HDfprintf(stream, "%*s%-*s %t\n", indent, "", fwidth,
	      "Dirty:",
	      oh->cache_info.is_dirty);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Version:",
	      oh->version);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Header size (in bytes):",
	      (unsigned)H5O_SIZEOF_HDR_OH(oh));
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Number of links:",
	      oh->nlink);
    /* Extra information for later versions */
    if(oh->version > H5O_VERSION_1) {
        struct tm *tm;          /* Time structure */
        char buf[128];          /* Buffer for formatting time info */

        /* Time fields */
        tm = HDlocaltime(&oh->atime);
        HDstrftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
        HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Access Time:", buf);
        tm = HDlocaltime(&oh->mtime);
        HDstrftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
        HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Modification Time:", buf);
        tm = HDlocaltime(&oh->ctime);
        HDstrftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
        HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Change Time:", buf);
        tm = HDlocaltime(&oh->btime);
        HDstrftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
        HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Birth Time:", buf);

        /* Attribute tracking fields */
        HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
                  "Max. compact attributes:",
                  (unsigned)oh->max_compact);
        HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
                  "Min. dense attributes:",
                  (unsigned)oh->min_dense);
        HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
                  "Number of attributes:",
                  oh->nattrs);
        HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                  "Attribute heap address:",
                  oh->attr_fheap_addr);
        HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                  "Attribute name index address:",
                  oh->name_bt2_addr);
    } /* end if */

    HDfprintf(stream, "%*s%-*s %Zu (%Zu)\n", indent, "", fwidth,
	      "Number of messages (allocated):",
	      oh->nmesgs, oh->alloc_nmesgs);
    HDfprintf(stream, "%*s%-*s %Zu (%Zu)\n", indent, "", fwidth,
	      "Number of chunks (allocated):",
	      oh->nchunks, oh->alloc_nchunks);

    /* debug each chunk */
    for(i = 0, chunk_total = 0; i < oh->nchunks; i++) {
        size_t chunk_size;

	chunk_total += oh->chunk[i].size;
	HDfprintf(stream, "%*sChunk %d...\n", indent, "", i);

	HDfprintf(stream, "%*s%-*s %t\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Dirty:",
		  oh->chunk[i].dirty);

	HDfprintf(stream, "%*s%-*s %a\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Address:",
                  oh->chunk[i].addr);

	if(0 == i) {
            if(H5F_addr_ne(oh->chunk[i].addr, addr))
                HDfprintf(stream, "*** WRONG ADDRESS!\n");
            chunk_size = oh->chunk[i].size - H5O_SIZEOF_HDR_OH(oh);
        } /* end if */
        else
            chunk_size = oh->chunk[i].size;
	HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Size in bytes:",
		  chunk_size);

	HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Gap:",
                  oh->chunk[i].gap);
    } /* end for */

    /* debug each message */
    if(NULL == (sequence = H5MM_calloc(NELMTS(H5O_msg_class_g) * sizeof(int))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    for(i = 0, mesg_total = 0; i < oh->nmesgs; i++) {
	mesg_total += H5O_SIZEOF_MSGHDR_OH(oh) + oh->mesg[i].raw_size;
	HDfprintf(stream, "%*sMessage %d...\n", indent, "", i);

	/* check for bad message id */
	if(oh->mesg[i].type->id >= (int)NELMTS(H5O_msg_class_g)) {
	    HDfprintf(stream, "*** BAD MESSAGE ID 0x%04x\n",
		      oh->mesg[i].type->id);
	    continue;
	} /* end if */

	/* message name and size */
	HDfprintf(stream, "%*s%-*s 0x%04x `%s' (%d)\n",
		  indent + 3, "", MAX(0, fwidth - 3),
		  "Message ID (sequence number):",
		  (unsigned) (oh->mesg[i].type->id),
		  oh->mesg[i].type->name,
		  sequence[oh->mesg[i].type->id]++);
	HDfprintf (stream, "%*s%-*s %t\n", indent+3, "", MAX (0, fwidth-3),
		   "Dirty:",
		   oh->mesg[i].dirty);
	HDfprintf (stream, "%*s%-*s %s\n", indent+3, "", MAX (0, fwidth-3),
		   "Shared:",
		   (oh->mesg[i].flags & H5O_MSG_FLAG_SHARED) ? "Yes" : "No");
	HDfprintf(stream, "%*s%-*s %s\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Constant:",
		  (oh->mesg[i].flags & H5O_MSG_FLAG_CONSTANT) ? "Yes" : "No");
	if(oh->mesg[i].flags & ~H5O_MSG_FLAG_BITS) {
	    HDfprintf (stream, "%*s%-*s 0x%02x\n", indent+3,"",MAX(0,fwidth-3),
		       "*** ADDITIONAL UNKNOWN FLAGS --->",
		       oh->mesg[i].flags & ~H5O_MSG_FLAG_BITS);
	} /* end if */
	HDfprintf(stream, "%*s%-*s %Zu bytes\n", indent+3, "", MAX(0,fwidth-3),
		  "Raw size in obj header:",
		  oh->mesg[i].raw_size);
	HDfprintf(stream, "%*s%-*s %u\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Chunk number:",
		  oh->mesg[i].chunkno);
	chunkno = oh->mesg[i].chunkno;
	if(chunkno >= oh->nchunks)
	    HDfprintf(stream, "*** BAD CHUNK NUMBER\n");
	HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Raw data offset in chunk:",
		  (size_t)(oh->mesg[i].raw - oh->chunk[chunkno].image));

	/* check the size */
	if((oh->mesg[i].raw + oh->mesg[i].raw_size >
                 oh->chunk[chunkno].image + oh->chunk[chunkno].size) ||
                (oh->mesg[i].raw < oh->chunk[chunkno].image))
	    HDfprintf(stream, "*** BAD MESSAGE RAW ADDRESS\n");

	/* decode the message */
	if(oh->mesg[i].flags & H5O_MSG_FLAG_SHARED) {
	    decode = H5O_MSG_SHARED->decode;
	    debug = H5O_MSG_SHARED->debug;
	} else {
	    decode = oh->mesg[i].type->decode;
	    debug = oh->mesg[i].type->debug;
	} /* end else */
	if(NULL==oh->mesg[i].native && decode)
	    oh->mesg[i].native = (decode)(f, dxpl_id, oh->mesg[i].raw);
	if(NULL == oh->mesg[i].native)
	    debug = NULL;

	/* print the message */
	HDfprintf(stream, "%*s%-*s\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Message Information:");
	if(debug)
	    (debug)(f, dxpl_id, oh->mesg[i].native, stream, indent + 6, MAX(0, fwidth - 6));
	else
	    HDfprintf(stream, "%*s<No info for this message>\n", indent + 6, "");

	/* If the message is shared then also print the pointed-to message */
	if(oh->mesg[i].flags & H5O_MSG_FLAG_SHARED) {
	    H5O_shared_t *shared = (H5O_shared_t*)(oh->mesg[i].native);
	    void *mesg;

            mesg = H5O_shared_read(f, dxpl_id, shared, oh->mesg[i].type, NULL);
	    if(oh->mesg[i].type->debug)
		(oh->mesg[i].type->debug)(f, dxpl_id, mesg, stream, indent + 3, MAX (0, fwidth - 3));
	    H5O_free_real(oh->mesg[i].type, mesg);
	} /* end if */
    } /* end for */
    sequence = H5MM_xfree(sequence);

    if(mesg_total != chunk_total)
	HDfprintf(stream, "*** TOTAL SIZE DOES NOT MATCH ALLOCATED SIZE!\n");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_debug_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_debug
 *
 * Purpose:	Prints debugging info about an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    H5O_t	*oh = NULL;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_debug, FAIL)

    /* check args */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    if(NULL == (oh = H5AC_protect(f, dxpl_id, H5AC_OHDR, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* debug */
    H5O_debug_real(f, dxpl_id, oh, addr, stream, indent, fwidth);

done:
    if(oh && H5AC_unprotect(f, dxpl_id, H5AC_OHDR, addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_debug() */

