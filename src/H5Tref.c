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

/*
 * Module Info: This module contains the functionality for reference
 *      datatypes in the H5T interface.
 */

#include "H5Tmodule.h"          /* This source code file is part of the H5T module */


#include "H5private.h"      /* Generic Functions    */
#include "H5Dprivate.h"     /* Dataset functions    */
#include "H5Eprivate.h"     /* Error handling       */
#include "H5HGprivate.h"    /* Global Heaps         */
#include "H5Iprivate.h"     /* IDs                  */
#include "H5MMprivate.h"    /* Memory management    */
#include "H5Pprivate.h"     /* Property lists       */
#include "H5Tpkg.h"         /* Datatypes            */

/* Local functions */
static size_t H5T__ref_reg_mem_getsize(const void *_ref);
static herr_t H5T__ref_reg_mem_read(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, size_t buf_size);
static herr_t H5T__ref_reg_mem_write(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, void *_bg, size_t buf_size);

static size_t H5T__ref_disk_getsize(const void *_ref);
static herr_t H5T__ref_disk_read(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, size_t buf_size);
static herr_t H5T__ref_disk_write(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, void *_bg, size_t buf_size);

/* Local variables */

/*-------------------------------------------------------------------------
 * Function: H5T__ref_set_loc
 *
 * Purpose:	Sets the location of a reference datatype to be either on disk
 *          or in memory
 *
 * Return:
 *  One of two values on success:
 *      TRUE - If the location of any reference types changed
 *      FALSE - If the location of any reference types is the same
 *  Negative value is returned on failure
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5T__ref_set_loc(const H5T_t *dt, H5F_t *f, H5T_loc_t loc)
{
    htri_t ret_value = FALSE; /* Indicate success, but no location change */

    FUNC_ENTER_PACKAGE

    /* check parameters */
    HDassert(dt);
    HDassert(loc >= H5T_LOC_BADLOC && loc < H5T_LOC_MAXLOC);

    /* Only change the location if it's different */
    if(loc != dt->shared->u.atomic.u.r.loc || f != dt->shared->u.atomic.u.r.f) {
        switch(loc) {
            case H5T_LOC_MEMORY: /* Memory based reference datatype */
                HDassert(NULL == f);

                /* Mark this type as being stored in memory */
                dt->shared->u.atomic.u.r.loc = H5T_LOC_MEMORY;

                if(dt->shared->u.atomic.u.r.rtype == H5R_REGION) {
                    /* size in memory, disk size is different */
                    dt->shared->size = sizeof(hreg_ref_t);

                    /* Set up the function pointers to access the region
                     * reference in memory */
                    dt->shared->u.atomic.u.r.getsize = H5T__ref_reg_mem_getsize;
                    dt->shared->u.atomic.u.r.read = H5T__ref_reg_mem_read;
                    dt->shared->u.atomic.u.r.write = H5T__ref_reg_mem_write;
                } else {
                    HDassert(0 && "Invalid reference type");
                }

                /* Reset file ID (since this reference is in memory) */
                dt->shared->u.atomic.u.r.f = NULL;
                break;

            case H5T_LOC_DISK: /* Disk based reference datatype */
                HDassert(f);

                /* Mark this type as being stored on disk */
                dt->shared->u.atomic.u.r.loc = H5T_LOC_DISK;

                /*
                 * Size of element on disk is 4 bytes for the length, plus the size
                 * of an address in this file, plus 4 bytes for the size of a heap
                 * ID. Memory size is different
                 */
                dt->shared->size = (size_t) (2 * H5_SIZEOF_UINT32_T) + H5F_SIZEOF_ADDR(f);

                /*
                 * Set up the function pointers to access the information on
                 * disk. Region and attribute references are stored identically
                 * on disk, so use the same functions
                 */
                dt->shared->u.atomic.u.r.getsize = H5T__ref_disk_getsize;
                dt->shared->u.atomic.u.r.read = H5T__ref_disk_read;
                dt->shared->u.atomic.u.r.write = H5T__ref_disk_write;

                /* Set file ID (since this reference is on disk) */
                dt->shared->u.atomic.u.r.f = f;
                break;

            case H5T_LOC_BADLOC:
            case H5T_LOC_MAXLOC:
            default:
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADRANGE, FAIL, "invalid reference datatype location")
        } /* end switch */

        /* Indicate that the location changed */
        ret_value = TRUE;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__ref_set_loc() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_reg_mem_getsize
 *
 * Purpose:	Retrieves the size of a memory based region reference.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5T__ref_reg_mem_getsize(const void *_ref)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    const hreg_ref_t *ref = (const hreg_ref_t *)_ref;
#else
    hreg_ref_t ref;
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters, return result */
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    HDassert(ref);

    FUNC_LEAVE_NOAPI(ref->buf_size)
#else
    HDassert(_ref);
    HDmemcpy(&ref, _ref, sizeof(hreg_ref_t));

    FUNC_LEAVE_NOAPI(ref.buf_size)
#endif
}   /* end H5T__ref_reg_mem_getsize() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_reg_mem_read
 *
 * Purpose:	"Reads" the memory based region reference into a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__ref_reg_mem_read(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
        void *_ref, void *buf, size_t buf_size)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    const hreg_ref_t *ref = (const hreg_ref_t *)_ref;
#else
    hreg_ref_t ref;
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters, copy data */
    HDassert(buf);
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    HDassert(ref && ref->buf);

    HDmemcpy(buf, ref->buf, buf_size);
#else
    HDassert(_ref);
    HDmemcpy(&ref, _ref, sizeof(hreg_ref_t));
    HDassert(ref.buf);

    HDmemcpy(buf, ref.buf, buf_size);
#endif

    FUNC_LEAVE_NOAPI(SUCCEED)
}   /* end H5T__ref_reg_mem_read() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_reg_mem_write
 *
 * Purpose:	"Writes" the memory region reference from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__ref_reg_mem_write(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
        void *_ref, void *buf, void H5_ATTR_UNUSED *_bg, size_t buf_size)
{
    hreg_ref_t ref;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check parameters */
    HDassert(_ref);
    HDassert(buf);

    if (buf_size != 0) {
        if (NULL == (ref.buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for reference data")

        /* Copy the data into the newly allocated buffer */
        HDmemcpy(ref.buf, buf, buf_size);
    } /* end if */
    else
        ref.buf = NULL;

    /* Set the size */
    ref.buf_size = buf_size;

    /* Set pointer in user's buffer with memcpy, to avoid alignment issues */
    HDmemcpy(_ref, &ref, sizeof(hreg_ref_t));

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__ref_reg_mem_write() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_disk_getsize
 *
 * Purpose:	Retrieves the length of a disk based reference element.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5T__ref_disk_getsize(const void *_ref)
{
    const uint8_t *ref = (const uint8_t *)_ref; /* Pointer to the disk reference information */
    size_t buf_size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters */
    HDassert(ref);

    UINT32DECODE(ref, buf_size);

    FUNC_LEAVE_NOAPI(buf_size)
}   /* end H5T__ref_disk_getsize() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_disk_read
 *
 * Purpose:	Reads the disk based region element into a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__ref_disk_read(H5F_t *f, hid_t dxpl_id, void *_ref, void *buf,
        size_t H5_ATTR_UNUSED buf_size)
{
    const uint8_t *ref = (const uint8_t *)_ref;
    H5HG_t hobjid;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check parameters */
    HDassert(ref);
    HDassert(buf);
    HDassert(f);

    /* Skip the length of the sequence */
    ref += 4;

    /* Get the heap information */
    H5F_addr_decode(f, (const uint8_t **)&ref, &(hobjid.addr));
    UINT32DECODE(ref, hobjid.idx);

    /* Check if this sequence actually has any data */
    if(hobjid.addr > 0) {
        /* Read the VL information from disk */
        if(H5HG_read(f, dxpl_id, &hobjid, buf, NULL) == NULL)
            HGOTO_ERROR(H5E_DATATYPE, H5E_READERROR, FAIL, "Unable to read reference information")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__ref_disk_read() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_disk_write
 *
 * Purpose:	Writes the disk based region element from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__ref_disk_write(H5F_t *f, hid_t dxpl_id, void *_ref, void *buf, void *_bg,
        size_t buf_size)
{
    uint8_t *ref = (uint8_t *)_ref; /* Pointer to the user's information */
    const uint8_t *bg = (const uint8_t *)_bg; /* Pointer to the old data */
    H5HG_t hobjid; /* New reference's heap ID */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check parameters */
    HDassert(ref);
    HDassert(buf_size == 0 || buf);
    HDassert(f);

    /* TODO not sure if we need that but oh well... */
    /* Free heap object for old data.  */
    if(bg != NULL) {
        H5HG_t bg_hobjid; /* "Background" reference's ID info */

        /* Skip the length of the reference and heap object ID from background data. */
        bg += 4;

        /* Get heap information */
        H5F_addr_decode(f, (const uint8_t **)&bg, &(bg_hobjid.addr));
        UINT32DECODE(bg, bg_hobjid.idx);

        /* Free heap object for old data */
        if(bg_hobjid.addr > 0) {
            /* Free heap object */
            if(H5HG_remove(f, dxpl_id, &bg_hobjid) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "Unable to remove heap object")
        } /* end if */
    } /* end if */

    /* Set the size */
    UINT32ENCODE(ref, buf_size);

    /* Write the reference information to disk (allocates space also) */
    if(H5HG_insert(f, dxpl_id, buf_size, buf, &hobjid) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "Unable to write reference information")

    /* Encode the heap information */
    H5F_addr_encode(f, &ref, hobjid.addr);
    UINT32ENCODE(ref, hobjid.idx);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__ref_disk_write() */

