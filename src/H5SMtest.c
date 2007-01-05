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

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg 	  */
#define H5SM_PACKAGE		/*suppress error about including H5SMpkg	  */
#define H5SM_TESTING		/*suppress warning about H5SM testing funcs*/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access                          */
#include "H5SMpkg.h"            /* Shared object header messages        */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


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
 * Function:	H5SM_get_refcount_bt2_cb
 *
 * Purpose:	v2 B-tree 'find' callback to retrieve the record for a message
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, December 19, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_get_refcount_bt2_cb(const void *_record, void *_op_data)
{
    const H5SM_sohm_t *record = (const H5SM_sohm_t *)_record;  /* v2 B-tree record for message */
    H5SM_sohm_t *op_data = (H5SM_sohm_t *)_op_data;       /* "op data" from v2 B-tree find */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_get_refcount_bt2_cb)

    /*
     * Check arguments.
     */
    HDassert(record);
    HDassert(op_data);

    /* Make a copy of the record */
    *op_data = *record;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_get_refcount_bt2_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_get_refcount_test
 *
 * Purpose:     Retrieve the reference count for a message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, December 19, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_get_refcount_test(H5F_t *f, hid_t dxpl_id, unsigned type_id,
    const H5O_shared_t *sh_mesg, hsize_t *ref_count)
{    
    H5HF_t *fheap = NULL;               /* Fractal heap that contains shared messages */
    H5SM_master_table_t *table = NULL;  /* SOHM master table */
    H5SM_list_t *list = NULL;           /* SOHM index list for message type (if in list form) */
    H5SM_index_header_t *header=NULL;   /* Index header for message type */
    H5SM_mesg_key_t key;                /* Key for looking up message */
    H5SM_fh_ud_gh_t udata;              /* User data for fractal heap 'op' callback */
    H5SM_sohm_t message;                /* Record for shared message */
    ssize_t index_num;                  /* Table index for message type */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_get_refcount_test)

    /* Sanity check */
    HDassert(f);
    HDassert(sh_mesg);
    HDassert(ref_count);

    /* Look up the master SOHM table */
    if(NULL == (table = (H5SM_master_table_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, FAIL, "unable to load SOHM master table")

    /* Find the correct index and try to delete from it */
    if((index_num = H5SM_get_index(table, type_id)) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_NOTFOUND, FAIL, "unable to find correct SOHM index")
    header = &(table->indexes[index_num]);

    /* Open the heap that this message is in */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, header->heap_addr)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Prepare user data for callback */
    udata.type_id = type_id;

    /* Compute the hash value for the B-tree lookup */
    if(H5HF_op(fheap, dxpl_id, &(sh_mesg->u.heap_id), H5SM_get_hash_fh_cb, &udata) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't access message in fractal heap")

    /* Set up key for message to locate */
    key.hash = udata.hash;
    key.encoding = NULL;
    key.encoding_size = 0;
    key.fheap = fheap;
    key.mesg_heap_id = sh_mesg->u.heap_id;

    /* Try to find the message in the index */
    if(header->index_type == H5SM_LIST) {
        size_t list_pos;        /* Position of the message in the list */

        /* If the index is stored as a list, get it from the cache */
        if(NULL == (list = (H5SM_list_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, NULL, header, H5AC_READ)))
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTPROTECT, FAIL, "unable to load SOHM index")

        /* Find the message in the list */
        if((list_pos = H5SM_find_in_list(list, &key)) == UFAIL)
	    HGOTO_ERROR(H5E_SOHM, H5E_NOTFOUND, FAIL, "message not in index")

        /* Copy the message */
        message = list->messages[list_pos];
    } /* end if */
    else /* Index is a B-tree */
    {
        HDassert(header->index_type == H5SM_BTREE);

        /* Look up the message in the v2 B-tree */
        if(H5B2_find(f, dxpl_id, H5SM_INDEX, header->index_addr, &key, H5SM_get_refcount_bt2_cb, &message) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_NOTFOUND, FAIL, "message not in index")
    } /* end else */

    /* Set the refcount for the message */
    *ref_count = message.ref_count;

done:
    /* Release resources */
    if(list && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, list, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to close SOHM index")
    if(table && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, table, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTRELEASE, FAIL, "unable to close SOHM master table")
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_get_refcount_test() */

