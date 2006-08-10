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
 * Created:		H5HFhuge.c
 *			Aug  7 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Routines for "huge" objects in fractal heap
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5MFprivate.h"	/* File memory management		*/


/****************/
/* Local Macros */
/****************/

/* v2 B-tree creation macros */
#define H5HF_HUGE_BT2_NODE_SIZE         512
#define H5HF_HUGE_BT2_SPLIT_PERC        100
#define H5HF_HUGE_BT2_MERGE_PERC         40


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* local v2 B-tree operations */
static herr_t H5HF_huge_bt2_create(H5HF_hdr_t *hdr, hid_t dxpl_id);

/* v2 B-tree function callbacks (in H5HFbtree2.c) */
herr_t H5HF_huge_bt2_found(const void *nrecord, void *op_data);
herr_t H5HF_huge_bt2_remove(const void *nrecord, void *op_data);

/*********************/
/* Package Variables */
/*********************/

/* The v2 B-tree class for tracking huge objects */
H5_DLLVAR const H5B2_class_t H5HF_BTREE2[1];


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_bt2_create
 *
 * Purpose:	Create the v2 B-tree for tracking the huge objects in the heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  7 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_huge_bt2_create(H5HF_hdr_t *hdr, hid_t dxpl_id)
{
    size_t rrec_size;                   /* Size of 'raw' records on disk */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_bt2_create)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Compute the size of 'raw' records on disk */
    if(hdr->huge_ids_direct)
        rrec_size = hdr->sizeof_addr + hdr->sizeof_size;
    else
        rrec_size = hdr->sizeof_addr + hdr->sizeof_size + hdr->huge_id_size;

    /* Create v2 B-tree for tracking 'huge' objects */
    if(H5B2_create(hdr->f, dxpl_id, &hdr->huge_bt2_class, H5HF_HUGE_BT2_NODE_SIZE, rrec_size,
            H5HF_HUGE_BT2_SPLIT_PERC, H5HF_HUGE_BT2_MERGE_PERC, &hdr->huge_bt2_addr/*out*/) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "can't create v2 B-tree for tracking 'huge' heap objects")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_huge_bt2_create() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_init
 *
 * Purpose:	Initialize information for tracking 'huge' objects
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  7 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_init(H5HF_hdr_t *hdr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_huge_init)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Compute information about 'huge' objects for the heap */

    /* Check if we can completely hold the 'huge' object's offset & length in
     *  the file in the heap ID (which will speed up accessing it)
     */
    if((hdr->sizeof_addr + hdr->sizeof_size) <= (hdr->id_len - 1)) {
        /* Indicate that v2 B-tree doesn't have to be used to locate object */
        hdr->huge_ids_direct = TRUE;

        /* Set the size of 'huge' object IDs */
        hdr->huge_id_size = hdr->sizeof_addr + hdr->sizeof_size;
    } /* end if */
    else {
        /* Indicate that v2 B-tree must be used to locate object */
        hdr->huge_ids_direct = FALSE;

        /* Set the size and maximum value of 'huge' object ID */
        if((hdr->id_len - 1) < sizeof(hsize_t)) {
            hdr->huge_id_size = hdr->id_len - 1;
            hdr->huge_max_id = ((hsize_t)1 << (hdr->huge_id_size * 8)) - 1;
        } /*end if */
        else {
            hdr->huge_id_size = sizeof(hsize_t);
            hdr->huge_max_id = HSIZET_MAX;
        } /* end else */
    } /* end else */


    /* Set up the v2 B-tree for tracking 'huge' objects in the heap */

    /* Copy the standard v2 B-tree class */
    HDmemcpy(&hdr->huge_bt2_class, H5HF_BTREE2, sizeof(H5B2_class_t));

    /* Set the native record size for the v2 B-tree */
    hdr->huge_bt2_class.nrec_size = sizeof(H5HF_huge_bt2_rec_t);

    /* Set v2 B-tree class's "class private" pointer to the heap header */
    hdr->huge_bt2_class.cls_private = hdr;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_huge_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_insert
 *
 * Purpose:	Insert a huge object into the file and track it
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  7 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_insert(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t obj_size, const void *obj,
    void *_id)
{
    H5HF_huge_bt2_rec_t obj_rec;        /* Record for tracking object */
    uint8_t *id = (uint8_t *)_id;       /* Pointer to ID buffer */
    haddr_t obj_addr;                   /* Address of object in the file */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_insert)
#ifdef QAK
HDfprintf(stderr, "%s: obj_size = %Zu\n", FUNC, obj_size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(obj_size > hdr->max_man_size);
    HDassert(obj);
    HDassert(id);

    /* Check if the v2 B-tree for tracking 'huge' heap objects has been created yet */
    if(!H5F_addr_defined(hdr->huge_bt2_addr))
        if(H5HF_huge_bt2_create(hdr, dxpl_id) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "can't create v2 B-tree for tracking 'huge' heap objects")

    /* Allocate space in the file for storing the 'huge' object */
    if(HADDR_UNDEF == (obj_addr = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_HUGE_OBJ, dxpl_id, (hsize_t)obj_size)))
        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap huge object")

    /* Write the object's data to disk */
    if(H5F_block_write(hdr->f, H5FD_MEM_FHEAP_HUGE_OBJ, obj_addr, obj_size, dxpl_id, obj) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "writing 'huge' object to file failed")

    /* Initialize shared part of record for tracking object in v2 B-tree */
    obj_rec.addr = obj_addr;
    obj_rec.len = obj_size;

    /* If the 'huge' object will be indirectly accessed, through the v2 B-tree,
     *  create an ID for it, otherwise put a zero in for ID
     */
    if(hdr->huge_ids_direct)
        obj_rec.id = 0;
    else {
        /* Check for wrapping around 'huge' object ID space */
        if(hdr->huge_ids_wrapped)
            /* Fail for now - eventually should iterate through v2 B-tree, looking for available ID */
            HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "wrapping 'huge' object IDs not supported yet")
        else {
            /* Get new 'huge' object ID to use for object */
            /* (avoid using ID 0) */
            obj_rec.id = ++hdr->huge_next_id;

            /* Check for wrapping 'huge' object IDs around */
            if(hdr->huge_next_id == hdr->huge_max_id)
                hdr->huge_ids_wrapped = TRUE;
        } /* end else */
    } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: obj_rec = {%a, %Hu, %Hu}\n", FUNC, obj_rec.addr, obj_rec.len, obj_rec.id);
#endif /* QAK */

    /* Insert record for object in v2 B-tree */
    if(H5B2_insert(hdr->f, dxpl_id, &hdr->huge_bt2_class, hdr->huge_bt2_addr, &obj_rec) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINSERT, FAIL, "couldn't insert object tracking record in v2 B-tree")

    /* Encode ID for user */
    *id++ = H5HF_ID_VERS_CURR | H5HF_ID_TYPE_HUGE;
    if(hdr->huge_ids_direct) {
        H5F_addr_encode(hdr->f, &id, obj_addr);
        H5F_ENCODE_LENGTH(hdr->f, id, (hsize_t)obj_size);
    } /* end if */
    else
        UINT64ENCODE_VAR(id, obj_rec.id, hdr->huge_id_size)

    /* Update statistics about heap */
    hdr->huge_size += obj_size;
    hdr->huge_nobjs++;

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_huge_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_get_obj_len
 *
 * Purpose:	Get the size of a 'huge' object in a fractal heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_get_obj_len(H5HF_hdr_t *hdr, hid_t dxpl_id, const uint8_t *id,
    size_t *obj_len_p)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5HF_huge_get_obj_len, FAIL)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(H5F_addr_defined(hdr->huge_bt2_addr));
    HDassert(id);
    HDassert(obj_len_p);

    /* Check if 'huge' object ID encodes address & length directly */
    if(hdr->huge_ids_direct) {
        /* Skip over object offset in file */
        id += hdr->sizeof_addr;

        /* Retrieve the object's length */
        H5F_DECODE_LENGTH(hdr->f, id, *obj_len_p);
    } /* end if */
    else {
        H5HF_huge_bt2_rec_t found_rec;  /* Record found from tracking object */
        H5HF_huge_bt2_rec_t search_rec; /* Record for searching for object */

        /* Get ID for looking up 'huge' object in v2 B-tree */
        UINT64DECODE_VAR(id, search_rec.id, hdr->huge_id_size)

        /* Look up object in v2 B-tree */
        if(H5B2_find(hdr->f, dxpl_id, &hdr->huge_bt2_class, hdr->huge_bt2_addr,
                    &search_rec, H5HF_huge_bt2_found, &found_rec) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_NOTFOUND, FAIL, "can't find object in B-tree")

        /* Retrieve the object's length */
        *obj_len_p = (size_t)found_rec.len;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_huge_get_obj_len() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_read
 *
 * Purpose:	Read a 'huge' object from the heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_read(H5HF_hdr_t *hdr, hid_t dxpl_id, const uint8_t *id, void *obj)
{
    haddr_t obj_addr;                   /* Object's address in the file */
    hsize_t obj_size;                   /* Object's size in the file */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_read)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(id);
    HDassert(obj);

    /* Check for 'huge' object ID that encodes address & length directly */
    if(hdr->huge_ids_direct) {
        /* Retrieve the object's address and length */
        H5F_addr_decode(hdr->f, &id, &obj_addr);
        H5F_DECODE_LENGTH(hdr->f, id, obj_size);
    } /* end if */
    else {
        H5HF_huge_bt2_rec_t found_rec;  /* Record found from tracking object */
        H5HF_huge_bt2_rec_t search_rec; /* Record for searching for object */

        /* Get ID for looking up 'huge' object in v2 B-tree */
        UINT64DECODE_VAR(id, search_rec.id, hdr->huge_id_size)

        /* Look up object in v2 B-tree */
        if(H5B2_find(hdr->f, dxpl_id, &hdr->huge_bt2_class, hdr->huge_bt2_addr,
                    &search_rec, H5HF_huge_bt2_found, &found_rec) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_NOTFOUND, FAIL, "can't find object in B-tree")

        /* Retrieve the object's address & length */
        obj_addr = found_rec.addr;
        obj_size = found_rec.len;
    } /* end else */

    /* Read the object's data from the file */
    if (H5F_block_read(hdr->f, H5FD_MEM_FHEAP_HUGE_OBJ, obj_addr, (size_t)obj_size, dxpl_id, obj) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_READERROR, FAIL, "can't read 'huge' object's data from the file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_huge_read() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_remove
 *
 * Purpose:	Remove a 'huge' object from the file and the v2 B-tree tracker
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_remove(H5HF_hdr_t *hdr, hid_t dxpl_id, const uint8_t *id)
{
    H5HF_huge_bt2_rec_t search_rec;     /* Record for searching for object */
    H5HF_huge_remove_ud1_t udata;       /* User callback data for v2 B-tree remove call */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_remove)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(id);

    /* Check for 'huge' object ID that encodes address & length directly */
    if(hdr->huge_ids_direct) {
        /* Retrieve the object's address and length */
        /* (used as key in v2 B-tree record) */
        H5F_addr_decode(hdr->f, &id, &search_rec.addr);
        H5F_DECODE_LENGTH(hdr->f, id, search_rec.len);
    } /* end if */
    else
        /* Get ID for looking up 'huge' object in v2 B-tree */
        UINT64DECODE_VAR(id, search_rec.id, hdr->huge_id_size)

    /* Set up the callback info */
    udata.hdr = hdr;
    udata.dxpl_id = dxpl_id;

    /* Remove the record for tracking the 'huge' object from the v2 B-tree */
    /* (space in the file for the object is freed in the 'remove' callback) */
    if(H5B2_remove(hdr->f, dxpl_id, &hdr->huge_bt2_class, hdr->huge_bt2_addr,
                &search_rec, H5HF_huge_bt2_remove, &udata) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTREMOVE, FAIL, "can't remove object from B-tree")

    /* Update statistics about heap */
    hdr->huge_size -= udata.obj_len;
    hdr->huge_nobjs--;

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_huge_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_term
 *
 * Purpose:	Shut down the information for tracking 'huge' objects
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_term(H5HF_hdr_t *hdr, hid_t dxpl_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_term)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Check if there are no more 'huge' objects in the heap and delete the
     *  v2 B-tree that tracks them, if so
     */
    if(H5F_addr_defined(hdr->huge_bt2_addr) && hdr->huge_nobjs == 0) {
        /* Sanity check */
        HDassert(hdr->huge_size == 0);

        /* Delete the v2 B-tree */
        if(H5B2_delete(hdr->f, dxpl_id, &hdr->huge_bt2_class, hdr->huge_bt2_addr, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDELETE, FAIL, "can't delete v2 B-tree")

        /* Reset the information about 'huge' objects in the file */
        hdr->huge_bt2_addr = HADDR_UNDEF;
        hdr->huge_next_id = 0;
        hdr->huge_ids_wrapped = FALSE;

        /* Mark heap header as modified */
        if(H5HF_hdr_dirty(hdr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_huge_term() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_huge_delete
 *
 * Purpose:	Delete all the 'huge' objects in the heap, and the v2 B-tree
 *              tracker for them
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_huge_delete(H5HF_hdr_t *hdr, hid_t dxpl_id)
{
    H5HF_huge_remove_ud1_t udata;       /* User callback data for v2 B-tree remove call */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_huge_delete)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(H5F_addr_defined(hdr->huge_bt2_addr));
    HDassert(hdr->huge_nobjs);
    HDassert(hdr->huge_size);

    /* Set up the callback info */
    udata.hdr = hdr;
    udata.dxpl_id = dxpl_id;

    /* Delete the v2 B-tree */
    if(H5B2_delete(hdr->f, dxpl_id, &hdr->huge_bt2_class, hdr->huge_bt2_addr, H5HF_huge_bt2_remove, &udata) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDELETE, FAIL, "can't delete v2 B-tree")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_huge_delete() */

