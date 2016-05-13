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
#define H5R_FRIEND              /*suppress error about including H5Rpkg   */

#include "H5private.h"      /* Generic Functions    */
#include "H5Tpkg.h"         /* Datatypes            */
#include "H5Rpkg.h"         /* References           */
#include "H5Eprivate.h"     /* Error handling       */
#include "H5Iprivate.h"     /* IDs                  */
#include "H5MMprivate.h"    /* Memory management    */
#include "H5HGprivate.h"    /* Global Heaps         */

/* Local functions */
static herr_t H5T__ref_reclaim_recurse(void *elem, const H5T_t *dt);

static size_t H5T__ref_mem_getsize(const void *_ref);
static herr_t H5T__ref_mem_read(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, size_t buf_size);
static herr_t H5T__ref_mem_write(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, void *_bg, size_t buf_size, H5R_type_t ref_type);

static size_t H5T__obj_ref_mem_getsize(const void *_ref);
static herr_t H5T__obj_ref_mem_read(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, size_t buf_size);
static herr_t H5T__obj_ref_mem_write(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, void *_bg, size_t buf_size, H5R_type_t ref_type);

static size_t H5T__ref_disk_getsize(const void *_ref);
static herr_t H5T__ref_disk_read(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, size_t buf_size);
static herr_t H5T__ref_disk_write(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, void *_bg, size_t buf_size, H5R_type_t ref_type);

static size_t H5T__obj_ref_disk_getsize(const void *_ref);
static herr_t H5T__obj_ref_disk_read(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, size_t buf_size);
static herr_t H5T__obj_ref_disk_write(H5F_t *f, hid_t dxpl_id, void *_ref, void *_buf, void *_bg, size_t buf_size, H5R_type_t ref_type);

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

                /* size in memory, disk size is different */
                dt->shared->size = sizeof(href_t);

                if (dt->shared->u.atomic.u.r.rtype == H5R_OBJECT) {
                    /* Set up the function pointers to access the object
                     * reference in memory */
                    dt->shared->u.atomic.u.r.getsize = H5T__obj_ref_mem_getsize;
                    dt->shared->u.atomic.u.r.read = H5T__obj_ref_mem_read;
                    dt->shared->u.atomic.u.r.write = H5T__obj_ref_mem_write;
                } else {
                    /* Set up the function pointers to access the reference in memory */
                    dt->shared->u.atomic.u.r.getsize = H5T__ref_mem_getsize;
                    dt->shared->u.atomic.u.r.read = H5T__ref_mem_read;
                    dt->shared->u.atomic.u.r.write = H5T__ref_mem_write;
                }

                /* Reset file ID (since this reference is in memory) */
                dt->shared->u.atomic.u.r.f = NULL;
                break;

            case H5T_LOC_DISK: /* Disk based reference datatype */
                HDassert(f);

                /* Mark this type as being stored on disk */
                dt->shared->u.atomic.u.r.loc = H5T_LOC_DISK;

                if (dt->shared->u.atomic.u.r.rtype == H5R_OBJECT) {
                    /* Size of element on disk */
                    dt->shared->size = sizeof(haddr_t);

                    /*
                     * Set up the function pointers to access the information on
                     * disk
                     */
                    dt->shared->u.atomic.u.r.getsize = H5T__obj_ref_disk_getsize;
                    dt->shared->u.atomic.u.r.read = H5T__obj_ref_disk_read;
                    dt->shared->u.atomic.u.r.write = H5T__obj_ref_disk_write;
                } else {
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
                }

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
 * Function:	H5T__ref_mem_getsize
 *
 * Purpose:	Retrieves the size of a memory based reference.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5T__ref_mem_getsize(const void *_ref)
{
    const href_t *ref_ptr = (const href_t *)_ref;
    const struct href *ref = NULL;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters, return result */
    HDassert(ref_ptr);
    ref = *ref_ptr;
    HDassert(ref);

    FUNC_LEAVE_NOAPI(ref->ref.serial.buf_size)
}   /* end H5T__ref_mem_getsize() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_mem_read
 *
 * Purpose:	"Reads" the memory based reference into a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__ref_mem_read(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
    void *_ref, void *buf, size_t buf_size)
{
    const href_t *ref_ptr = (const href_t *)_ref;
    const struct href *ref;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters, copy data */
    HDassert(buf);
    HDassert(ref_ptr);
    ref = *ref_ptr;
    HDassert(ref && ref->ref.serial.buf);

    HDmemcpy(buf, ref->ref.serial.buf, buf_size);

    FUNC_LEAVE_NOAPI(SUCCEED)
}   /* end H5T__ref_mem_read() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_mem_write
 *
 * Purpose:	"Writes" the memory reference from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__ref_mem_write(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
    void *_ref, void *buf, void H5_ATTR_UNUSED *_bg, size_t buf_size,
    H5R_type_t ref_type)
{
    herr_t ret_value = SUCCEED; /* Return value */
    href_t *ref_ptr = (href_t *)_ref;
    struct href *ref;

    FUNC_ENTER_NOAPI_NOINIT

    /* check parameters */
    HDassert(ref_ptr);
    HDassert(buf);
    HDassert(buf_size);

    if (NULL == (ref = (struct href *)H5MM_malloc(sizeof(struct href))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for reference data")
    if (NULL == (ref->ref.serial.buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for reference data")

    /* Copy the data into the newly allocated buffer */
    HDmemcpy(ref->ref.serial.buf, buf, buf_size);

    /* Set the size */
    ref->ref.serial.buf_size = buf_size;

    /* Set the type */
    ref->ref_type = ref_type;

    /* Set the ref */
    *ref_ptr = ref;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__ref_mem_write() */

/*-------------------------------------------------------------------------
 * Function:    H5T__obj_ref_mem_getsize
 *
 * Purpose: Retrieves the size of a memory based object reference.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5T__obj_ref_mem_getsize(const void H5_ATTR_UNUSED *_ref)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(sizeof(haddr_t))
}   /* end H5T__obj_ref_mem_getsize() */

/*-------------------------------------------------------------------------
 * Function:    H5T__obj_ref_mem_read
 *
 * Purpose: "Reads" the memory based object reference into a buffer
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__obj_ref_mem_read(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
    void *_ref, void *buf, size_t H5_ATTR_UNUSED buf_size)
{
    const href_t *ref_ptr = (const href_t *)_ref;
    const struct href *ref;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters, copy data */
    HDassert(buf);
    HDassert(ref_ptr);
    ref = *ref_ptr;
    HDassert(ref);

    HDmemcpy(buf, &ref->ref.addr, sizeof(haddr_t));

    FUNC_LEAVE_NOAPI(SUCCEED)
}   /* end H5T__obj_ref_mem_read() */

/*-------------------------------------------------------------------------
 * Function:    H5T__obj_ref_mem_write
 *
 * Purpose: "Writes" the memory object reference from a buffer
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__obj_ref_mem_write(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
    void *_ref, void *buf, void H5_ATTR_UNUSED *_bg, size_t buf_size,
    H5R_type_t ref_type)
{
    herr_t ret_value = SUCCEED; /* Return value */
    href_t *ref_ptr = (href_t *)_ref;
    struct href *ref;

    FUNC_ENTER_NOAPI_NOINIT

    /* check parameters */
    HDassert(ref_ptr);
    HDassert(buf);
    HDassert(buf_size);

    if (NULL == (ref = (struct href *)H5MM_malloc(sizeof(struct href))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for reference data")

    /* Copy the data into the newly allocated buffer */
    HDmemcpy(&ref->ref.addr, buf, buf_size);

    /* Set the type */
    ref->ref_type = ref_type;

    /* Set the ref */
    *ref_ptr = ref;
done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__obj_ref_mem_write() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_disk_getsize
 *
 * Purpose:	Retrieves the length of a disk based reference.
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
 * Purpose:	Reads the disk based reference into a buffer
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

    if(!H5F_addr_defined(hobjid.addr) || hobjid.addr == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Undefined reference pointer")

    /* Read the information from disk */
    if(H5HG_read(f, dxpl_id, &hobjid, buf, NULL) == NULL)
        HGOTO_ERROR(H5E_DATATYPE, H5E_READERROR, FAIL, "Unable to read reference information")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__ref_disk_read() */

/*-------------------------------------------------------------------------
 * Function:	H5T__ref_disk_write
 *
 * Purpose:	Writes the disk based reference from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__ref_disk_write(H5F_t *f, hid_t dxpl_id, void *_ref, void *buf, void *_bg,
    size_t buf_size, H5R_type_t H5_ATTR_UNUSED ref_type)
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

    /* Free heap object for old data.  */
    if(bg != NULL) { /* TODO Should get rid of bg stuff */
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

/*-------------------------------------------------------------------------
 * Function:    H5T__obj_ref_disk_getsize
 *
 * Purpose: Retrieves the length of a disk based object reference.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5T__obj_ref_disk_getsize(const void H5_ATTR_UNUSED *_ref)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(sizeof(haddr_t))
}   /* end H5T__obj_ref_disk_getsize() */

/*-------------------------------------------------------------------------
 * Function:    H5T__obj_ref_disk_read
 *
 * Purpose: Reads the disk based object reference into a buffer
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__obj_ref_disk_read(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
    void *_ref, void *buf, size_t H5_ATTR_UNUSED buf_size)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters */
    HDassert(_ref);
    HDassert(buf);

    HDmemcpy(buf, _ref, sizeof(haddr_t));

    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__obj_ref_disk_read() */

/*-------------------------------------------------------------------------
 * Function:    H5T__obj_ref_disk_write
 *
 * Purpose: Writes the disk based object reference from a buffer
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__obj_ref_disk_write(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
    void *_ref, void *buf, void H5_ATTR_UNUSED *_bg, size_t H5_ATTR_UNUSED buf_size,
    H5R_type_t H5_ATTR_UNUSED ref_type)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters */
    HDassert(_ref);
    HDassert(buf_size == 0 || buf);

    HDmemcpy(_ref, buf, sizeof(haddr_t));

    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__obj_ref_disk_write() */

/*-------------------------------------------------------------------------
 * Function:    H5T__ref_reclaim_recurse
 *
 * Purpose: Internal recursive routine to free reference datatypes
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__ref_reclaim_recurse(void *elem, const H5T_t *dt)
{
    unsigned u;                     /* Local index variable */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(elem);
    HDassert(dt);

    /* Check the datatype of this element */
    switch(dt->shared->type) {
        case H5T_ARRAY:
            /* Recurse on each element, if the array's base type is array, VL, enum or compound */
            if(H5T_IS_COMPLEX(dt->shared->parent->shared->type)) {
                void *off;     /* offset of field */

                /* Calculate the offset member and recurse on it */
                for(u = 0; u < dt->shared->u.array.nelem; u++) {
                    off = ((uint8_t *)elem) + u * (dt->shared->parent->shared->size);
                    if(H5T__ref_reclaim_recurse(off, dt->shared->parent) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "Unable to free array element")
                } /* end for */
            } /* end if */
            break;

        case H5T_COMPOUND:
            /* Check each field and recurse on VL, compound, enum or array ones */
            for(u = 0; u < dt->shared->u.compnd.nmembs; u++) {
                /* Recurse if it's VL, compound, enum or array */
                if(H5T_IS_COMPLEX(dt->shared->u.compnd.memb[u].type->shared->type)) {
                    void *off;     /* offset of field */

                    /* Calculate the offset member and recurse on it */
                    off = ((uint8_t *)elem) + dt->shared->u.compnd.memb[u].offset;
                    if(H5T__ref_reclaim_recurse(off, dt->shared->u.compnd.memb[u].type) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "Unable to free compound field")
                } /* end if */
            } /* end for */
            break;
        case H5T_REFERENCE:
        {
            href_t *ref = (href_t *)elem;

            HDassert(ref);
            if (H5R_destroy(*ref) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTFREE, FAIL, "cannot free reference")
        }
            break;

        /* Don't do anything for other types */
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_TIME:
        case H5T_STRING:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_VLEN:
            break;

        /* Should never have these values */
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADRANGE, FAIL, "invalid datatype class")
            break;

    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T__ref_reclaim_recurse() */

/*-------------------------------------------------------------------------
 * Function:    H5T__ref_reclaim
 *
 * Purpose: Default method to reclaim any reference data for a buffer element
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_ref_reclaim(void *elem, hid_t type_id, unsigned H5_ATTR_UNUSED ndim,
    const hsize_t H5_ATTR_UNUSED *point, void H5_ATTR_UNUSED *op_data)
{
    H5T_t	*dt;
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(elem);
    HDassert(H5I_DATATYPE == H5I_get_type(type_id));

    /* Check args */
    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")

    /* Pull the free function and free info pointer out of the op_data and call the recurse datatype free function */
    if(H5T__ref_reclaim_recurse(elem, dt) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't reclaim reference elements")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T_ref_reclaim() */

