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
 * Created:             H5Faccum.c
 *                      Jan 10 2008
 *                      Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:             File metadata "accumulator" routines.  (Used to
 *                      cache small metadata I/Os and group them into a
 *                      single larger I/O)
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

/* Metadata accumulator controls */
#define H5F_ACCUM_THROTTLE      8
#define H5F_ACCUM_THRESHOLD     2048


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

/* Declare a PQ free list to manage the metadata accumulator buffer */
H5FL_BLK_DEFINE_STATIC(meta_accum);



/*-------------------------------------------------------------------------
 * Function:	H5F_accum_read
 *
 * Purpose:	Attempts to read some data from the metadata accumulator for
 *              a file into a buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jan 10 2008
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5F_accum_read(const H5F_t *f, hid_t dxpl_id, H5FD_mem_t type, haddr_t addr,
    size_t size, void *buf/*out*/)
{
    htri_t      ret_value = FALSE;    /* Return value */

    FUNC_ENTER_NOAPI(H5F_accum_read, FAIL)

    HDassert(f);
    HDassert(f->shared);
    HDassert(buf);

    /* Check if this information is in the metadata accumulator */
    if((f->shared->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) && type != H5FD_MEM_DRAW) {
        /* Current read overlaps with metadata accumulator */
        if(H5F_addr_overlap(addr, size, f->shared->accum.loc, f->shared->accum.size)) {
            unsigned char *read_buf = (unsigned char *)buf; /* Pointer to the buffer being read in */
            size_t amount_read;         /* Amount to read at a time */
            hsize_t read_off;           /* Offset to read from */

            /* Read the part before the metadata accumulator */
            if(addr < f->shared->accum.loc) {
                /* Set the amount to read */
                H5_ASSIGN_OVERFLOW(amount_read, (f->shared->accum.loc - addr), hsize_t, size_t);

                /* Dispatch to driver */
                if(H5FD_read(f->shared->lf, dxpl_id, type, addr, amount_read, read_buf) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "driver read request failed")

                /* Adjust the buffer, address & size */
                read_buf += amount_read;
                addr += amount_read;
                size -= amount_read;
            } /* end if */

            /* Copy the part overlapping the metadata accumulator */
            if(size > 0 && (addr >= f->shared->accum.loc && addr < (f->shared->accum.loc + f->shared->accum.size))) {
                /* Set the offset to "read" from */
                read_off = addr - f->shared->accum.loc;

                /* Set the amount to "read" */
#ifndef NDEBUG
{
                hsize_t tempamount_read;         /* Amount to read at a time */

                tempamount_read = f->shared->accum.size - read_off;
                H5_CHECK_OVERFLOW(tempamount_read, hsize_t, size_t);
                amount_read = MIN(size, (size_t)tempamount_read);
}
#else /* NDEBUG */
                amount_read = MIN(size, (size_t)(f->shared->accum.size - read_off));
#endif /* NDEBUG */

                /* Copy the data out of the buffer */
                HDmemcpy(read_buf, f->shared->accum.buf + read_off, amount_read);

                /* Adjust the buffer, address & size */
                read_buf += amount_read;
                addr += amount_read;
                size -= amount_read;
            } /* end if */

            /* Read the part after the metadata accumulator */
            if(size > 0 && addr >= (f->shared->accum.loc + f->shared->accum.size)) {
                /* Dispatch to driver */
                if(H5FD_read(f->shared->lf, dxpl_id, type, addr, size, read_buf) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "driver read request failed")

                /* Adjust the buffer, address & size */
                read_buf += size;
                addr += size;
                size -= size;
            } /* end if */

            /* Make certain we've read it all */
            HDassert(size == 0);
        } /* end if */
        /* Current read doesn't overlap with metadata accumulator, read it into accumulator */
        else {
            /* Only update the metadata accumulator if it is not dirty or if
             * we are allowed to write the accumulator out during reads (when
             * it is dirty)
             */
            if(f->shared->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA_READ || !f->shared->accum.dirty) {
                /* Flush current contents, if dirty */
                if(f->shared->accum.dirty) {
                    if(H5FD_write(f->shared->lf, dxpl_id, H5FD_MEM_DEFAULT, f->shared->accum.loc, f->shared->accum.size, f->shared->accum.buf) < 0)
                        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "driver write request failed")

                    /* Reset accumulator dirty flag */
                    f->shared->accum.dirty = FALSE;
                } /* end if */

                /* Cache the new piece of metadata */
                /* Check if we need to resize the buffer */
                if(size > f->shared->accum.alloc_size) {
                    /* Grow the metadata accumulator buffer */
                    if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")

                    /* Note the new buffer size */
                    f->shared->accum.alloc_size = size;
                } /* end if */
                else {
                    /* Check if we should shrink the accumulator buffer */
                    if(size < (f->shared->accum.alloc_size / H5F_ACCUM_THROTTLE) &&
                            f->shared->accum.alloc_size > H5F_ACCUM_THRESHOLD) {
                        size_t new_size = (f->shared->accum.alloc_size / H5F_ACCUM_THROTTLE); /* New size of accumulator buffer */

                        /* Shrink the accumulator buffer */
                        if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, new_size)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")

                        /* Note the new buffer size */
                        f->shared->accum.alloc_size = new_size;
                    } /* end if */
                } /* end else */

                /* Update accumulator information */
                f->shared->accum.loc = addr;
                f->shared->accum.size = size;
                f->shared->accum.dirty = FALSE;

                /* Read into accumulator */
                if(H5FD_read(f->shared->lf, dxpl_id, H5FD_MEM_DEFAULT, f->shared->accum.loc, f->shared->accum.size, f->shared->accum.buf) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "driver read request failed")

                /* Copy into buffer */
                HDmemcpy(buf, f->shared->accum.buf, size);
            } /* end if */
            else {
                /* Dispatch to driver */
                if(H5FD_read(f->shared->lf, dxpl_id, type, addr, size, buf) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "driver read request failed")
            } /* end else */
        } /* end else */

        /* Indicate success */
        HGOTO_DONE(TRUE);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_accum_read() */


/*-------------------------------------------------------------------------
 * Function:	H5F_accum_write
 *
 * Purpose:	Attempts to read some data from the metadata accumulator for
 *              a file into a buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jan 10 2008
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5F_accum_write(const H5F_t *f, hid_t dxpl_id, H5FD_mem_t type, haddr_t addr,
    size_t size, const void *buf)
{
    htri_t      ret_value = FALSE;    /* Return value */

    FUNC_ENTER_NOAPI(H5F_accum_write, FAIL)

    HDassert(f);
    HDassert(f->shared);
    HDassert(f->intent & H5F_ACC_RDWR);
    HDassert(buf);

    /* Check for accumulating metadata */
    if((f->shared->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) && type != H5FD_MEM_DRAW) {
        /* Check if there is already metadata in the accumulator */
        if(f->shared->accum.size > 0) {
            /* Check if the piece of metadata being written adjoins or is inside the metadata accumulator */
            if(H5F_addr_overlap(addr, size, f->shared->accum.loc, f->shared->accum.size)) {
                size_t new_size;    /* New size of the accumulator buffer */
                size_t old_offset;  /* Offset of old data within the accumulator buffer */

                /* Check if the new metadata adjoins the beginning of the current accumulator */
                if((addr + size) == f->shared->accum.loc) {
                    /* Check if we need more buffer space */
                    if((size + f->shared->accum.size) > f->shared->accum.alloc_size) {
                        /* Adjust the buffer size, by doubling it */
                        f->shared->accum.alloc_size = MAX(f->shared->accum.alloc_size * 2, size + f->shared->accum.size);

                        /* Reallocate the metadata accumulator buffer */
                        if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, f->shared->accum.alloc_size)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")
#ifdef H5_CLEAR_MEMORY
HDmemset(f->shared->accum.buf + f->shared->accum.size, 0, (f->shared->accum.alloc_size - (f->shared->accum.size + size)));
#endif /* H5_CLEAR_MEMORY */
                    } /* end if */

                    /* Move the existing metadata to the proper location */
                    HDmemmove(f->shared->accum.buf + size, f->shared->accum.buf, f->shared->accum.size);

                    /* Copy the new metadata at the front */
                    HDmemcpy(f->shared->accum.buf, buf, size);

                    /* Set the new size & location of the metadata accumulator */
                    f->shared->accum.loc = addr;
                    f->shared->accum.size += size;

                    /* Mark it as written to */
                    f->shared->accum.dirty = TRUE;
                } /* end if */
                /* Check if the new metadata adjoins the end of the current accumulator */
                else if(addr == (f->shared->accum.loc + f->shared->accum.size)) {
                    /* Check if we need more buffer space */
                    if((size + f->shared->accum.size) > f->shared->accum.alloc_size) {
                        /* Adjust the buffer size, by doubling it */
                        f->shared->accum.alloc_size = MAX(f->shared->accum.alloc_size * 2, size + f->shared->accum.size);

                        /* Reallocate the metadata accumulator buffer */
                        if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, f->shared->accum.alloc_size)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")
#ifdef H5_CLEAR_MEMORY
HDmemset(f->shared->accum.buf + f->shared->accum.size + size, 0, (f->shared->accum.alloc_size - (f->shared->accum.size + size)));
#endif /* H5_CLEAR_MEMORY */
                    } /* end if */

                    /* Copy the new metadata to the end */
                    HDmemcpy(f->shared->accum.buf + f->shared->accum.size, buf, size);

                    /* Set the new size of the metadata accumulator */
                    f->shared->accum.size += size;

                    /* Mark it as written to */
                    f->shared->accum.dirty = TRUE;
                } /* end if */
                /* Check if the new metadata is entirely within the current accumulator */
                else if(addr >= f->shared->accum.loc && (addr + size) <= (f->shared->accum.loc + f->shared->accum.size)) {
                    /* Copy the new metadata to the proper location within the accumulator */
                    HDmemcpy(f->shared->accum.buf + (addr - f->shared->accum.loc), buf, size);

                    /* Mark it as written to */
                    f->shared->accum.dirty = TRUE;
                } /* end if */
                /* Check if the new metadata overlaps the beginning of the current accumulator */
                else if(addr < f->shared->accum.loc && (addr + size) <= (f->shared->accum.loc + f->shared->accum.size)) {
                    /* Calculate the new accumulator size, based on the amount of overlap */
                    H5_ASSIGN_OVERFLOW(new_size, (f->shared->accum.loc - addr) + f->shared->accum.size, hsize_t, size_t);

                    /* Check if we need more buffer space */
                    if(new_size > f->shared->accum.alloc_size) {
                        /* Adjust the buffer size, by doubling it */
                        f->shared->accum.alloc_size = MAX(f->shared->accum.alloc_size + 2, new_size);

                        /* Reallocate the metadata accumulator buffer */
                        if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, f->shared->accum.alloc_size)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")
#ifdef H5_CLEAR_MEMORY
HDmemset(f->shared->accum.buf + f->shared->accum.size, 0, (f->shared->accum.alloc_size - f->shared->accum.size));
#endif /* H5_CLEAR_MEMORY */
                    } /* end if */

                    /* Calculate the proper offset of the existing metadata */
                    H5_ASSIGN_OVERFLOW(old_offset, (addr + size) - f->shared->accum.loc, hsize_t, size_t);

                    /* Move the existing metadata to the proper location */
                    HDmemmove(f->shared->accum.buf + size, f->shared->accum.buf + old_offset, (f->shared->accum.size - old_offset));

                    /* Copy the new metadata at the front */
                    HDmemcpy(f->shared->accum.buf, buf, size);

                    /* Set the new size & location of the metadata accumulator */
                    f->shared->accum.loc = addr;
                    f->shared->accum.size = new_size;

                    /* Mark it as written to */
                    f->shared->accum.dirty = TRUE;
                } /* end if */
                /* Check if the new metadata overlaps the end of the current accumulator */
                else if(addr >= f->shared->accum.loc && (addr + size) > (f->shared->accum.loc + f->shared->accum.size)) {
                    /* Calculate the new accumulator size, based on the amount of overlap */
                    H5_ASSIGN_OVERFLOW(new_size, (addr - f->shared->accum.loc) + size, hsize_t, size_t);

                    /* Check if we need more buffer space */
                    if(new_size > f->shared->accum.alloc_size) {
                        /* Adjust the buffer size, by doubling it */
                        f->shared->accum.alloc_size = MAX(f->shared->accum.alloc_size * 2, new_size);

                        /* Reallocate the metadata accumulator buffer */
                        if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, f->shared->accum.alloc_size)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")
#ifdef H5_CLEAR_MEMORY
HDmemset(f->shared->accum.buf + f->shared->accum.size, 0, (f->shared->accum.alloc_size - f->shared->accum.size));
#endif /* H5_CLEAR_MEMORY */
                    } /* end if */

                    /* Copy the new metadata to the end */
                    HDmemcpy(f->shared->accum.buf + (addr - f->shared->accum.loc), buf, size);

                    /* Set the new size & location of the metadata accumulator */
                    f->shared->accum.size = new_size;

                    /* Mark it as written to */
                    f->shared->accum.dirty = TRUE;
                } /* end if */
                else {
                    HDassert(0 && "New metadata overlapped both beginning and end of existing metadata accumulator!");
                } /* end else */
            } /* end if */
            /* New piece of metadata doesn't adjoin or overlap the existing accumulator */
            else {
                /* Write out the existing metadata accumulator, with dispatch to driver */
                if(f->shared->accum.dirty) {
                    if(H5FD_write(f->shared->lf, dxpl_id, H5FD_MEM_DEFAULT, f->shared->accum.loc, f->shared->accum.size, f->shared->accum.buf) < 0)
                        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "file write failed")

                    /* Reset accumulator dirty flag */
                    f->shared->accum.dirty = FALSE;
                } /* end if */

                /* Cache the new piece of metadata */
                /* Check if we need to resize the buffer */
                if(size > f->shared->accum.alloc_size) {
                    /* Grow the metadata accumulator buffer */
                    if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")

                    /* Note the new buffer size */
                    f->shared->accum.alloc_size = size;
#ifdef H5_CLEAR_MEMORY
{
size_t clear_size = MAX(f->shared->accum.size, size);
HDmemset(f->shared->accum.buf + clear_size, 0, (f->shared->accum.alloc_size - clear_size));
}
#endif /* H5_CLEAR_MEMORY */
                } /* end if */
                else {
                    /* Check if we should shrink the accumulator buffer */
                    if(size < (f->shared->accum.alloc_size / H5F_ACCUM_THROTTLE) &&
                            f->shared->accum.alloc_size > H5F_ACCUM_THRESHOLD) {
                        size_t tmp_size = (f->shared->accum.alloc_size / H5F_ACCUM_THROTTLE); /* New size of accumulator buffer */

                        /* Shrink the accumulator buffer */
                        if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, tmp_size)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")

                        /* Note the new buffer size */
                        f->shared->accum.alloc_size = tmp_size;
                    } /* end if */
                } /* end else */

                /* Update the metadata accumulator information */
                f->shared->accum.loc = addr;
                f->shared->accum.size = size;
                f->shared->accum.dirty = TRUE;

                /* Store the piece of metadata in the accumulator */
                HDmemcpy(f->shared->accum.buf, buf, size);
            } /* end else */
        } /* end if */
        /* No metadata in the accumulator, grab this piece and keep it */
        else {
            /* Check if we need to reallocate the buffer */
            if(size > f->shared->accum.alloc_size) {
                /* Reallocate the metadata accumulator buffer */
                if(NULL == (f->shared->accum.buf = H5FL_BLK_REALLOC(meta_accum, f->shared->accum.buf, size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate metadata accumulator buffer")

                /* Note the new buffer size */
                f->shared->accum.alloc_size = size;
            } /* end if */

            /* Update the metadata accumulator information */
            f->shared->accum.loc = addr;
            f->shared->accum.size = size;
            f->shared->accum.dirty = TRUE;

            /* Store the piece of metadata in the accumulator */
            HDmemcpy(f->shared->accum.buf, buf, size);
        } /* end else */

        /* Indicate success */
        HGOTO_DONE(TRUE);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_accum_write() */


/*-------------------------------------------------------------------------
 * Function:    H5F_accum_free
 *
 * Purpose:     Check for free space invalidating [part of] a metadata
 *              accumulator.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Jan 10 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_accum_free(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type, haddr_t addr,
    hsize_t size)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5F_accum_free, FAIL)

    /* check arguments */
    HDassert(f);

    /* Adjust the metadata accumulator to remove the freed block, if it overlaps */
    if((f->shared->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA)
            && H5F_addr_overlap(addr, size, f->shared->accum.loc, f->shared->accum.size)) {
        size_t overlap_size;        /* Size of overlap with accumulator */

        /* Sanity check */
        /* (The metadata accumulator should not intersect w/raw data */
        HDassert(H5FD_MEM_DRAW != type);

        /* Check for overlapping the beginning of the accumulator */
        if(H5F_addr_le(addr, f->shared->accum.loc)) {
            /* Check for completely overlapping the accumulator */
            if(H5F_addr_ge(addr + size, f->shared->accum.loc + f->shared->accum.size)) {
                /* Reset the accumulator, but don't free buffer */
                f->shared->accum.loc = HADDR_UNDEF;
                f->shared->accum.size = 0;
                f->shared->accum.dirty = FALSE;
            } /* end if */
            /* Block to free must end within the accumulator */
            else {
                size_t new_accum_size;      /* Size of new accumulator buffer */

                /* Calculate the size of the overlap with the accumulator, etc. */
                H5_ASSIGN_OVERFLOW(overlap_size, (addr + size) - f->shared->accum.loc, haddr_t, size_t);
                new_accum_size = f->shared->accum.size - overlap_size;

                /* Move the accumulator buffer information to eliminate the freed block */
                HDmemmove(f->shared->accum.buf, f->shared->accum.buf + overlap_size, new_accum_size);

                /* Adjust the accumulator information */
                f->shared->accum.loc += overlap_size;
                f->shared->accum.size = new_accum_size;
            } /* end else */
        } /* end if */
        /* Block to free must start within the accumulator */
        else {
            /* Calculate the size of the overlap with the accumulator */
            H5_ASSIGN_OVERFLOW(overlap_size, (f->shared->accum.loc + f->shared->accum.size) - addr, haddr_t, size_t);

            /* Block to free is in the middle of the accumulator */
            if(H5F_addr_lt((addr + size), f->shared->accum.loc + f->shared->accum.size)) {
                haddr_t tail_addr;
                size_t tail_size;

                /* Calculate the address & size of the tail to write */
                tail_addr = addr + size;
                H5_ASSIGN_OVERFLOW(tail_size, (f->shared->accum.loc + f->shared->accum.size) - tail_addr, haddr_t, size_t);

                /* Write out the part of the accumulator after the block to free */
                if(H5FD_write(f->shared->lf, dxpl_id, H5FD_MEM_DEFAULT, tail_addr, tail_size, f->shared->accum.buf + (tail_addr - f->shared->accum.loc)) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "file write failed")
            } /* end if */

            /* Adjust the accumulator information */
            f->shared->accum.size = f->shared->accum.size - overlap_size;
        } /* end else */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_accum_free() */


/*-------------------------------------------------------------------------
 * Function:	H5F_accum_flush
 *
 * Purpose:	Flush the metadata accumulator to the file
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jan 10 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_accum_flush(H5F_t *f, hid_t dxpl_id)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5F_accum_flush, FAIL)

    HDassert(f);
    HDassert(f->shared);

    /* Check if we need to flush out the metadata accumulator */
    if((f->shared->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) && f->shared->accum.dirty && f->shared->accum.size > 0) {
        /* Flush the metadata contents */
        if(H5FD_write(f->shared->lf, dxpl_id, H5FD_MEM_DEFAULT, f->shared->accum.loc, f->shared->accum.size, f->shared->accum.buf) < 0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "file write failed")

        /* Reset the dirty flag */
        f->shared->accum.dirty = FALSE;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_accum_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5F_accum_reset
 *
 * Purpose:	Reset the metadata accumulator for the file
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jan 10 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_accum_reset(H5F_t *f)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5F_accum_reset)

    HDassert(f);
    HDassert(f->shared);

    /* Check if we need to reset the metadata accumulator information */
    if(f->shared->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) {
        /* Sanity check */
        HDassert(!f->closing || FALSE == f->shared->accum.dirty);

        /* Free the buffer */
        if(f->shared->accum.buf)
            f->shared->accum.buf = H5FL_BLK_FREE(meta_accum, f->shared->accum.buf);

        /* Reset the buffer sizes & location */
        f->shared->accum.alloc_size = f->shared->accum.size = 0;
        f->shared->accum.loc = HADDR_UNDEF;
        f->shared->accum.dirty = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5F_accum_reset() */


