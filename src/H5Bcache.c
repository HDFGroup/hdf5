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

/* Metadata cache callbacks */
static void *H5B_deserialize(haddr_t addr, size_t len, const void *image,
    const void *udata, hbool_t *dirty);
static herr_t H5B_serialize(const H5F_t *f, haddr_t addr, size_t len, void *image,
    void *thing, unsigned *flags, haddr_t *new_addr, size_t *new_len, void **new_image);
static herr_t H5B_free_icr(haddr_t addr, size_t len, void *thing);


/*********************/
/* Package Variables */
/*********************/

/* H5B inherits cache-like properties from H5AC2 */
const H5AC2_class_t H5AC2_BT[1] = {{
    H5AC2_BT_ID,
    "v1 B-tree",
    H5FD_MEM_BTREE,
    H5B_deserialize,
    NULL,
    H5B_serialize,
    H5B_free_icr,
    NULL
}};

/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5B_deserialize
 *
 * Purpose:     Deserialize the data structure from disk.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar 24, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
H5B_deserialize(haddr_t UNUSED addr, size_t UNUSED len, const void *image, const void *_udata,
    hbool_t UNUSED *dirty)
{
    H5B_t *bt = NULL;           /* Pointer to the deserialized B-tree node */
    const H5B_cache_ud_t *udata = (const H5B_cache_ud_t *)_udata;       /* User data for callback */
    H5B_shared_t *shared;       /* Pointer to shared B-tree info */
    const uint8_t *p;           /* Pointer into image buffer */
    uint8_t *native;            /* Pointer to native keys */
    unsigned u;                 /* Local index variable */
    H5B_t *ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B_deserialize)

    /* check arguments */
    HDassert(image);

    /* Allocate the B-tree node in memory */
    if(NULL == (bt = H5FL_MALLOC(H5B_t)))
	HGOTO_ERROR(H5E_BTREE, H5E_NOSPACE, NULL, "can't allocate B-tree struct")
    HDmemset(&bt->cache_info, 0, sizeof(H5AC2_info_t));

    /* Set & increment the ref-counted "shared" B-tree information for the node */
    bt->rc_shared = udata->rc_shared;
    H5RC_INC(bt->rc_shared);

    /* Get a pointer to the shared info, for convenience */
    shared = (H5B_shared_t *)H5RC_GET_OBJ(bt->rc_shared);
    HDassert(shared);

    /* Allocate space for the native keys and child addresses */
    if(NULL == (bt->native = H5FL_BLK_MALLOC(native_block, shared->sizeof_keys)))
	HGOTO_ERROR(H5E_BTREE, H5E_NOSPACE, NULL, "can't allocate buffer for native keys")
    if(NULL == (bt->child = H5FL_SEQ_MALLOC(haddr_t, (size_t)shared->two_k)))
	HGOTO_ERROR(H5E_BTREE, H5E_NOSPACE, NULL, "can't allocate buffer for child addresses")

    /* Set the pointer into the image */
    p = image;

    /* magic number */
    if(HDmemcmp(p, H5B_MAGIC, (size_t)H5B_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree signature")
    p += 4;

    /* node type and level */
    if(*p++ != (uint8_t)udata->type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree node type")
    bt->level = *p++;

    /* entries used */
    UINT16DECODE(p, bt->nchildren);

    /* sibling pointers */
    H5F_addr_decode(udata->f, (const uint8_t **)&p, &(bt->left));
    H5F_addr_decode(udata->f, (const uint8_t **)&p, &(bt->right));

    /* the child/key pairs */
    native = bt->native;
    for(u = 0; u < bt->nchildren; u++) {
        /* Decode native key value */
        if((udata->type->decode)(shared, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "unable to decode key")
        p += shared->sizeof_rkey;
        native += udata->type->sizeof_nkey;

        /* Decode address value */
        H5F_addr_decode(udata->f, (const uint8_t **)&p, bt->child + u);
    } /* end for */

    /* Decode final key */
    if(bt->nchildren > 0) {
        /* Decode native key value */
        if((udata->type->decode)(shared, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "unable to decode key")
    } /* end if */

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Set return value */
    ret_value = bt;

done:
    if(!ret_value && bt)
        (void)H5B_dest(bt);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5B_serialize
 *
 * Purpose:     Serialize the data structure for writing to disk.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar 24, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_serialize(const H5F_t *f, haddr_t UNUSED addr, size_t UNUSED len, void *image,
    void *_thing, unsigned *flags, haddr_t UNUSED *new_addr, size_t UNUSED *new_len,
    void UNUSED **new_image)
{
    H5B_t *bt = (H5B_t *)_thing;        /* Pointer to the B-tree node */
    H5B_shared_t *shared;       /* Pointer to shared B-tree info */
    uint8_t    *p;              /* Pointer into image buffer */
    uint8_t    *native;         /* Pointer to native keys */
    unsigned    u;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B_serialize)

    /* check arguments */
    HDassert(image);
    HDassert(bt);
    HDassert(bt->rc_shared);
    HDassert(flags);
    shared = (H5B_shared_t *)H5RC_GET_OBJ(bt->rc_shared);
    HDassert(shared);

    /* Set the local pointer into the serialized image */
    p = image;

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
    native = bt->native;
    for(u = 0; u < bt->nchildren; ++u) {
        /* encode the key */
        if(shared->type->encode(shared, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree key")
        p += shared->sizeof_rkey;
        native += shared->type->sizeof_nkey;

        /* encode the child address */
        H5F_addr_encode(f, &p, bt->child[u]);
    } /* end for */
    if(bt->nchildren > 0) {
        /* Encode the final key */
        if(shared->type->encode(shared, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree key")
    } /* end if */

    /* Reset the cache flags for this operation (metadata not resized or renamed) */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5B_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data structure
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar 26, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_free_icr(haddr_t UNUSED addr, size_t UNUSED len, void *thing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy B-tree node */
    H5B_dest(thing);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B_free_icr() */

