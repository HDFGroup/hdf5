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
 * Created:		H5Bcache.c
 *			Oct 31 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement B-tree metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5B_PACKAGE		/*suppress error about including H5Bpkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Bpkg.h"		/* B-link trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/* General routines */
static herr_t H5B_serialize(const H5F_t *f, const H5B_t *bt);

/* Metadata cache callbacks */
static H5B_t *H5B_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata);
static herr_t H5B_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B_t *b);
static herr_t H5B_clear(H5F_t *f, H5B_t *b, hbool_t destroy);
static herr_t H5B_compute_size(const H5F_t *f, const H5B_t *bt, size_t *size_ptr);

/*********************/
/* Package Variables */
/*********************/

/* H5B inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_BT[1] = {{
    H5AC_BT_ID,
    (H5AC_load_func_t)H5B_load,
    (H5AC_flush_func_t)H5B_flush,
    (H5AC_dest_func_t)H5B_dest,
    (H5AC_clear_func_t)H5B_clear,
    (H5AC_size_func_t)H5B_compute_size,
}};

/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:    H5B_serialize
 *
 * Purpose:     Serialize the data structure for writing to disk or
 *              storing on the SAP (for FPHDF5).
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept. 15, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_serialize(const H5F_t *f, const H5B_t *bt)
{
    H5B_shared_t *shared=NULL;  /* Pointer to shared B-tree info */
    unsigned    u;
    uint8_t    *p;              /* Pointer into raw data buffer */
    uint8_t    *native;         /* Pointer to native keys */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5B_serialize, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(bt);
    HDassert(bt->rc_shared);
    shared=(H5B_shared_t *)H5RC_GET_OBJ(bt->rc_shared);
    HDassert(shared);

    p = shared->page;

    /* magic number */
    HDmemcpy(p, H5B_MAGIC, (size_t)H5B_SIZEOF_MAGIC);
    p += 4;

    /* node type and level */
    *p++ = (uint8_t)shared->type->id;
    H5_CHECK_OVERFLOW(bt->level, unsigned, uint8_t);
    *p++ = (uint8_t)bt->level;

    /* entries used */
    UINT16ENCODE(p, bt->nchildren);

    /* sibling pointers */
    H5F_addr_encode(f, &p, bt->left);
    H5F_addr_encode(f, &p, bt->right);

    /* child keys and pointers */
    native=bt->native;
    for (u = 0; u < bt->nchildren; ++u) {
        /* encode the key */
        if (shared->type->encode(f, bt, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree key")
        p += shared->sizeof_rkey;
        native += shared->type->sizeof_nkey;

        /* encode the child address */
        H5F_addr_encode(f, &p, bt->child[u]);
    } /* end for */
    if(bt->nchildren>0) {
        /* Encode the final key */
        if (shared->type->encode(f, bt, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree key")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5B_load
 *
 * Purpose:	Loads a B-tree node from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree node.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 *-------------------------------------------------------------------------
 */
static H5B_t *
H5B_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata)
{
    const H5B_class_t	*type = (const H5B_class_t *) _type;
    H5B_t		*bt = NULL;
    H5B_shared_t        *shared;        /* Pointer to shared B-tree info */
    uint8_t		*p;             /* Pointer into raw data buffer */
    uint8_t		*native;        /* Pointer to native keys */
    unsigned		u;              /* Local index variable */
    H5B_t		*ret_value;

    FUNC_ENTER_NOAPI(H5B_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(type);
    HDassert(type->get_shared);

    if (NULL==(bt = H5FL_MALLOC(H5B_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&bt->cache_info,0,sizeof(H5AC_info_t));
    if((bt->rc_shared=(type->get_shared)(f, udata))==NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "can't retrieve B-tree node buffer")
    shared=(H5B_shared_t *)H5RC_GET_OBJ(bt->rc_shared);
    HDassert(shared);
    if (NULL==(bt->native=H5FL_BLK_MALLOC(native_block,shared->sizeof_keys)) ||
            NULL==(bt->child=H5FL_SEQ_MALLOC(haddr_t,(size_t)shared->two_k)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    if (H5F_block_read(f, H5FD_MEM_BTREE, addr, shared->sizeof_rnode, dxpl_id, shared->page)<0)
	HGOTO_ERROR(H5E_BTREE, H5E_READERROR, NULL, "can't read B-tree node")

    p = shared->page;

    /* magic number */
    if (HDmemcmp(p, H5B_MAGIC, (size_t)H5B_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree signature")
    p += 4;

    /* node type and level */
    if (*p++ != (uint8_t)type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree node type")
    bt->level = *p++;

    /* entries used */
    UINT16DECODE(p, bt->nchildren);

    /* sibling pointers */
    H5F_addr_decode(f, (const uint8_t **) &p, &(bt->left));
    H5F_addr_decode(f, (const uint8_t **) &p, &(bt->right));

    /* the child/key pairs */
    native=bt->native;
    for (u = 0; u < bt->nchildren; u++) {
        /* Decode native key value */
        if ((type->decode) (f, bt, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "unable to decode key")
        p += shared->sizeof_rkey;
        native += type->sizeof_nkey;

        /* Decode address value */
        H5F_addr_decode(f, (const uint8_t **) &p, bt->child + u);
    }

    /* Decode final key */
    if(bt->nchildren>0) {
        /* Decode native key value */
        if ((type->decode) (f, bt, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "unable to decode key")
    } /* end if */

    /* Set return value */
    ret_value = bt;

done:
    if (!ret_value && bt)
        (void)H5B_dest(f,bt);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B_load() */  /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5B_flush
 *
 * Purpose:	Flushes a dirty B-tree node to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B_t *bt)
{
    H5B_shared_t        *shared;        /* Pointer to shared B-tree info */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5B_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(bt);
    shared=(H5B_shared_t *)H5RC_GET_OBJ(bt->rc_shared);
    HDassert(shared);
    HDassert(shared->type);
    HDassert(shared->type->encode);

    if (bt->cache_info.is_dirty) {
        if (H5B_serialize(f, bt) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTSERIALIZE, FAIL, "unable to serialize B-tree")

	/*
         * Write the disk page.	We always write the header, but we don't
         * bother writing data for the child entries that don't exist or
         * for the final unchanged children.
	 */
	if (H5F_block_write(f, H5FD_MEM_BTREE, addr, shared->sizeof_rnode, dxpl_id, shared->page) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFLUSH, FAIL, "unable to save B-tree node to disk")

	bt->cache_info.is_dirty = FALSE;
    } /* end if */

    if (destroy)
        if (H5B_dest(f,bt) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5B_dest
 *
 * Purpose:	Destroys a B-tree node in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jan 15 2003
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5B_dest(H5F_t UNUSED *f, H5B_t *bt)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B_dest)

    /*
     * Check arguments.
     */
    HDassert(bt);
    HDassert(bt->rc_shared);

    H5FL_SEQ_FREE(haddr_t,bt->child);
    H5FL_BLK_FREE(native_block,bt->native);
    H5RC_DEC(bt->rc_shared);
    H5FL_FREE(H5B_t,bt);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5B_clear
 *
 * Purpose:	Mark a B-tree node in memory as non-dirty.
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
H5B_clear(H5F_t *f, H5B_t *bt, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B_clear)

    /*
     * Check arguments.
     */
    HDassert(bt);

    /* Reset the dirty flag.  */
    bt->cache_info.is_dirty = FALSE;

    if (destroy)
        if (H5B_dest(f, bt) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5B_compute_size
 *
 * Purpose:	Compute the size in bytes of the specified instance of
 *		H5B_t on disk, and return it in *len_ptr.  On failure,
 *		the value of *len_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/13/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_compute_size(const H5F_t *f, const H5B_t *bt, size_t *size_ptr)
{
    H5B_shared_t        *shared;        /* Pointer to shared B-tree info */
    size_t	size;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B_compute_size)

    /* check arguments */
    HDassert(f);
    HDassert(bt);
    HDassert(bt->rc_shared);
    shared=(H5B_shared_t *)H5RC_GET_OBJ(bt->rc_shared);
    HDassert(shared);
    HDassert(shared->type);
    HDassert(size_ptr);

    /* Check node's size */
    if ((size = H5B_nodesize(f, shared, NULL)) == 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGETSIZE, FAIL, "H5B_nodesize() failed")

    /* Set size value */
    *size_ptr = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B_compute_size() */
