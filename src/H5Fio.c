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
 * Created:             H5Fio.c
 *                      Jan 10 2008
 *                      Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:             File I/O routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/


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



/*-------------------------------------------------------------------------
 * Function:	H5F_block_read
 *
 * Purpose:	Reads some data from a file/server/etc into a buffer.
 *		The data is contiguous.	 The address is relative to the base
 *		address for the file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_read(const H5F_t *f, H5FD_mem_t type, haddr_t addr, size_t size,
    hid_t dxpl_id, void *buf/*out*/)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(f->shared);
    HDassert(buf);
    HDassert(H5F_addr_defined(addr));

    /* Check for attempting I/O on 'temporary' file address */
    if(H5F_addr_le(f->shared->tmp_addr, (addr + size)))
        HGOTO_ERROR(H5E_IO, H5E_BADRANGE, FAIL, "attempting I/O in temporary file space")

    /* Pass through metadata accumulator layer */
    if(H5F_accum_read(f, dxpl_id, type, addr, size, buf) < 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "read through metadata accumulator failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_block_read() */


/*-------------------------------------------------------------------------
 * Function:	H5F_block_write
 *
 * Purpose:	Writes some data from memory to a file/server/etc.  The
 *		data is contiguous.  The address is relative to the base
 *		address.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_write(const H5F_t *f, H5FD_mem_t type, haddr_t addr, size_t size,
    hid_t dxpl_id, const void *buf)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
#ifdef QAK
HDfprintf(stderr, "%s: write to addr = %a, size = %Zu\n", FUNC, addr, size);
#endif /* QAK */

    HDassert(f);
    HDassert(f->shared);
    HDassert(f->intent & H5F_ACC_RDWR);
    HDassert(buf);
    HDassert(H5F_addr_defined(addr));

    /* Check for attempting I/O on 'temporary' file address */
    if(H5F_addr_le(f->shared->tmp_addr, (addr + size)))
        HGOTO_ERROR(H5E_IO, H5E_BADRANGE, FAIL, "attempting I/O in temporary file space")

    /* Pass through metadata accumulator layer */
    if(H5F_accum_write(f, dxpl_id, type, addr, size, buf) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "write through metadata accumulator failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_block_write() */


/*-------------------------------------------------------------------------
 * Function:    H5F_flush_tagged_metadata
 *
 * Purpose:     Flushes metadata with specified tag in the metadata cache 
 *              to disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:	Mike McGreevy
 *              September 9, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_flush_tagged_metadata(H5F_t * f, haddr_t tag, hid_t dxpl_id)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Use tag to search for and flush associated metadata */
    if(H5AC_flush_tagged_metadata(f, tag, dxpl_id)<0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush tagged metadata")
    
    /* Flush out the metadata accumulator */
    if(H5F_accum_flush(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTFLUSH, FAIL, "unable to flush metadata accumulator")

    /* Flush file buffers to disk. */
    if(H5FD_flush(f->shared->lf, dxpl_id, FALSE) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "low level flush failed")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5F_flush_tagged_metadata */


/*-------------------------------------------------------------------------
 * Function:    H5F_evict_tagged_metadata
 *
 * Purpose:     Evicts metadata from the cache with specified tag.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:	Mike McGreevy
 *              September 9, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_evict_tagged_metadata(H5F_t * f, haddr_t tag, hid_t dxpl_id)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Unpin the superblock, as this will be marked for eviction and it can't 
        be pinned. */
    if(H5AC_unpin_entry(f->shared->sblock) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "unable to unpin superblock")
    f->shared->sblock = NULL;

    /* Evict the object's metadata */
    if(H5AC_evict_tagged_metadata(f, tag, dxpl_id)<0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, FAIL, "unable to evict tagged metadata")

    /* Re-read the superblock. */
    if (H5F_super_read(f, dxpl_id) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_READERROR, FAIL, "unable to read superblock")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5F_evict_tagged_metadata */


/*-------------------------------------------------------------------------
 * Function:    H5F_get_checksums
 *
 * Purpose:   	Decode checksum stored in the buffer
 *		Calculate checksum for the data in the buffer
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_get_checksums(uint8_t *buf, size_t chk_size, uint32_t *s_chksum/*out*/, uint32_t *c_chksum/*out*/)
{
    uint32_t stored_chksum;  	/* Stored metadata checksum value */
    uint32_t computed_chksum; 	/* Computed metadata checksum value */
    const uint8_t *chk_p;      	/* Pointer into raw data buffer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check arguments */
    HDassert(buf);
    HDassert(chk_size);

    /* Compute checksum for the buffer */
    computed_chksum = H5_checksum_metadata(buf, chk_size - H5_SIZEOF_CHKSUM, 0);

    /* Offset to the checksum in the buffer */
    chk_p = buf + chk_size - H5_SIZEOF_CHKSUM;

    /* Decode the checksum stored in the buffer */
    UINT32DECODE(chk_p, stored_chksum);

    /* Return the stored checksum */
    if(s_chksum)
	*s_chksum = stored_chksum;

    /* Return the computed checksum */
    if(c_chksum)
	*c_chksum = computed_chksum;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5F_get_chksums() */


/*-------------------------------------------------------------------------
 * Function:    H5F_read_check_meatadata
 *
 * Purpose:   	Attempts to read and validate a piece of meatadata that has
 *		checksum as follows:
 * 		a) read the piece of metadata
 * 		b) calculate checksum for the buffer of metadata
 * 		c) decode the checksum stored in the buffer of metadata
 *		d) compare the computed checksum with its stored checksum
 *	       	The library will perform (a) to (d) above for "f->read_attempts"
 *		times or until the checksum comparison in (d) passes.
 *		This routine also tracks the # of retries via 
 *		H5F_track_metadata_read_retries()
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_read_check_metadata(H5F_t *f, H5FD_mem_t type, H5AC_type_t actype, haddr_t addr, size_t read_size, size_t chk_size,
    hid_t dxpl_id, uint8_t *buf/*out*/, uint32_t *chksum/*out*/)
{
    size_t tries, max_tries;	/* The # of read attempts */
    size_t retries;		/* The # of retries */
    uint32_t stored_chksum;  	/* Stored metadata checksum value */
    uint32_t computed_chksum; 	/* Computed metadata checksum value */
    herr_t ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get the # of read attempts */
    max_tries = tries = f->read_attempts;

    do {
        /* Read header from disk */
        if(H5F_block_read(f, type, addr, read_size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read metadata")

	/* Get stored and computed checksums */
	H5F_get_checksums(buf, chk_size, &stored_chksum, &computed_chksum);

        /* Verify checksum */
        if(stored_chksum == computed_chksum)
            break;

    } while(--tries);

    if(tries == 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "incorrect metadatda checksum after all read attempts (%u) for %u bytes:c_chksum=%u, s_chkum=%u", 
	    max_tries, chk_size, computed_chksum, stored_chksum)

    /* Calculate and track the # of retries */
    retries = max_tries - tries;
    if(retries) { /* Does not track 0 retry */
        HDfprintf(stderr, "%s: SUCCESS after %u retries; actype=%u\n", FUNC, retries, actype);
	if(H5F_track_metadata_read_retries(f, actype, retries) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, NULL, "cannot track read tries = %u ", retries)
    }

    /* Return the computed checksum */
    if(chksum)
	*chksum = computed_chksum;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5F_read_check_metadata */
