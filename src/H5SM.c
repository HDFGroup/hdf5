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

#define H5SM_PACKAGE		/*suppress error about including H5SMpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg 	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access                          */
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MFprivate.h"        /* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
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
static herr_t H5SM_create_index(H5F_t *f, H5SM_index_header_t *header,
                                hid_t dxpl_id);
static herr_t H5SM_delete_index(H5F_t *f, H5SM_index_header_t *header,
                                hid_t dxpl_id, hbool_t delete_heap);
static haddr_t H5SM_create_list(H5F_t *f, H5SM_index_header_t *header, hid_t dxpl_id);
static herr_t H5SM_convert_list_to_btree(H5F_t * f, H5SM_index_header_t * header,
                           H5SM_list_t **_list, H5HF_t *fheap, hid_t dxpl_id);
static herr_t H5SM_convert_btree_to_list(H5F_t * f, H5SM_index_header_t * header, hid_t dxpl_id);
static herr_t H5SM_write_mesg(H5F_t *f, hid_t dxpl_id, H5SM_index_header_t *header,
     unsigned type_id, void *mesg, unsigned *cache_flags_ptr);
static herr_t H5SM_delete_from_index(H5F_t *f, hid_t dxpl_id,
        H5SM_index_header_t *header, unsigned type_id, const H5O_shared_t * mesg,
        unsigned *cache_flags, void ** encoded_mesg);
static herr_t H5SM_type_to_flag(unsigned type_id, unsigned *type_flag);


/*********************/
/* Package Variables */
/*********************/

H5FL_DEFINE(H5SM_master_table_t);
H5FL_ARR_DEFINE(H5SM_index_header_t, H5O_SHMESG_MAX_NINDEXES);
H5FL_DEFINE(H5SM_list_t);
H5FL_ARR_DEFINE(H5SM_sohm_t, H5O_SHMESG_MAX_LIST_SIZE);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:    H5SM_init
 *
 * Purpose:     Initializes the Shared Message interface.
 *
 *              Creates a master SOHM table in the file and in the cache.
 *              This function should not be called for files that have
 *              SOHMs disabled in the FCPL.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, May 2, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_init(H5F_t *f, H5P_genplist_t * fc_plist, hid_t dxpl_id)
{
    H5SM_master_table_t *table = NULL;
    haddr_t table_addr = HADDR_UNDEF;
    unsigned num_indexes;
    unsigned list_max, btree_min;
    unsigned index_type_flags[H5O_SHMESG_MAX_NINDEXES];
    unsigned minsizes[H5O_SHMESG_MAX_NINDEXES];
    unsigned type_flags_used;
    unsigned x;
    hsize_t table_size;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_init, NULL)

    HDassert(f);
    /* File should not already have a SOHM table */
    HDassert(f->shared->sohm_addr == HADDR_UNDEF);

    /* Initialize master table */
    if(NULL == (table = H5FL_MALLOC(H5SM_master_table_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for SOHM table")

    /* Get information from fcpl */
    if(H5P_get(fc_plist, H5F_CRT_SHMSG_NINDEXES_NAME, &num_indexes)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get number of indexes")
    if(H5P_get(fc_plist, H5F_CRT_SHMSG_INDEX_TYPES_NAME, &index_type_flags)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get SOHM type flags")
    if(H5P_get(fc_plist, H5F_CRT_SHMSG_LIST_MAX_NAME, &list_max)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get SOHM list maximum")
    if(H5P_get(fc_plist, H5F_CRT_SHMSG_BTREE_MIN_NAME, &btree_min)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get SOHM btree minimum")
    if(H5P_get(fc_plist, H5F_CRT_SHMSG_INDEX_MINSIZE_NAME, &minsizes) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get SOHM message min sizes")

    /* Verify that values are valid */
    if(num_indexes > H5O_SHMESG_MAX_NINDEXES)
        HGOTO_ERROR(H5E_PLIST, H5E_BADRANGE, FAIL, "number of indexes in property list is too large")

    /* Check that type flags weren't duplicated anywhere */
    type_flags_used = 0;
    for(x = 0; x < num_indexes; ++x) {
        if(index_type_flags[x] & type_flags_used)
            HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "the same shared message type flag is assigned to more than one index")
        type_flags_used |= index_type_flags[x];
    }

    /* Set version and number of indexes in table and in superblock.
     * Right now we just use one byte to hold the number of indexes.
     */
    HDassert(num_indexes < 256);
    table->num_indexes = num_indexes;

    /* Check that list and btree cutoffs make sense.  There can't be any
     * values greater than the list max but less than the btree min; the
     * list max has to be greater than or equal to one less than the btree
     * min.
     */
    HDassert(list_max + 1 >= btree_min);
    HDassert(table->num_indexes > 0 && table->num_indexes <= H5O_SHMESG_MAX_NINDEXES);

    /* Allocate the SOHM indexes as an array. */
    if(NULL == (table->indexes = (H5SM_index_header_t *)H5FL_ARR_MALLOC(H5SM_index_header_t, (size_t)table->num_indexes)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for SOHM indexes")

    /* Initialize all of the indexes, but don't allocate space for them to
     * hold messages until we actually need to write to them.
     */
    for(x=0; x<table->num_indexes; x++)
    {
        table->indexes[x].btree_min = btree_min;
        table->indexes[x].list_max = list_max;
        table->indexes[x].mesg_types = index_type_flags[x];
        table->indexes[x].min_mesg_size = minsizes[x];
        table->indexes[x].index_addr = HADDR_UNDEF;
        table->indexes[x].heap_addr = HADDR_UNDEF;
        table->indexes[x].num_messages = 0;
        /* Indexes start as lists unless the list-to-btree threshold is zero */
        if(table->indexes[x].list_max > 0) {
            table->indexes[x].index_type = H5SM_LIST;
        } else {
            table->indexes[x].index_type = H5SM_BTREE;
        }
    } /* end for */

    /* Allocate space for the table on disk */
    table_size = (hsize_t) H5SM_TABLE_SIZE(f) + (hsize_t) (table->num_indexes * H5SM_INDEX_HEADER_SIZE(f));
    if(HADDR_UNDEF == (table_addr = H5MF_alloc(f, H5FD_MEM_SOHM_TABLE, dxpl_id, table_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for SOHM table")

    /* Cache the new table */
    if(H5AC_set(f, dxpl_id, H5AC_SOHM_TABLE, table_addr, table, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_CACHE, H5E_CANTLOAD, FAIL, "can't add SOHM table to cache")

    /* Record the address of the master table in the file */
    f->shared->sohm_addr = table_addr;

done:
    if(ret_value < 0)
    {
        if(table_addr != HADDR_UNDEF)
            H5MF_xfree(f, H5FD_MEM_SOHM_TABLE, dxpl_id, table_addr, (hsize_t)H5SM_TABLE_SIZE(f));
        if(table != NULL)
            H5FL_FREE(H5SM_master_table_t, table);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_init() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_type_to_flag
 *
 * Purpose:     Get the shared message flag for a given message type.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, October 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_type_to_flag(unsigned type_id, unsigned *type_flag)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_type_to_flag)

    /* Translate the H5O type_id into an H5SM type flag */
    switch(type_id) {
        case H5O_SDSPACE_ID:
            *type_flag = H5O_MESG_SDSPACE_FLAG;
            break;
        case H5O_DTYPE_ID:
            *type_flag = H5O_MESG_DTYPE_FLAG;
            break;
        case H5O_FILL_ID:
        case H5O_FILL_NEW_ID:
            *type_flag = H5O_MESG_FILL_FLAG;
            break;
        case H5O_PLINE_ID:
            *type_flag = H5O_MESG_PLINE_FLAG;
            break;
        case H5O_ATTR_ID:
            *type_flag = H5O_MESG_ATTR_FLAG;
            break;
        default:
            HGOTO_ERROR(H5E_OHDR, H5E_BADTYPE, FAIL, "unknown message type ID")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_type_to_flag() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_get_index
 *
 * Purpose:     Get the index number for a given message type.
 *
 *              Returns the number of the index in the supplied table
 *              that holds messages of type type_id, or negative if
 *              there is no index for this message type.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, October 10, 2006
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5SM_get_index(const H5SM_master_table_t *table, unsigned type_id)
{
    ssize_t x;
    unsigned type_flag;
    ssize_t ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT(H5SM_get_index)

    /* Translate the H5O type_id into an H5SM type flag */
    if(H5SM_type_to_flag(type_id, &type_flag) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't map message type to flag")

    /* Search the indexes until we find one that matches this flag or we've
     * searched them all.
     */
    for(x = 0; x < table->num_indexes; ++x)
        if(table->indexes[x].mesg_types & type_flag)
            HGOTO_DONE(x)

    /* At this point, ret_value is either the location of the correct
     * index or it's still FAIL because we didn't find an index.
     */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_get_index() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_type_shared
 *
 * Purpose:     Check if a given message type is shared in a file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, December 12, 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5SM_type_shared(H5F_t *f, unsigned type_id, hid_t dxpl_id)
{
    H5SM_master_table_t *table = NULL;  /* Shared object master table */
    unsigned type_flag;                 /* Flag corresponding to message type */
    size_t u;                           /* Local index variable */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_type_shared)

    /* Translate the H5O type_id into an H5SM type flag */
    if(H5SM_type_to_flag(type_id, &type_flag) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't map message type to flag")

    /* Look up the master SOHM table */
    if(H5F_addr_defined(f->shared->sohm_addr)) {
        if(NULL == (table = (H5SM_master_table_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, NULL, NULL, H5AC_READ)))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load SOHM master table")
    } /* end if */
    else
        /* No shared messages of any type */
        HGOTO_DONE(FALSE)

    /* Search the indexes until we find one that matches this flag or we've
     * searched them all.
     */
    for(u = 0; u < table->num_indexes; u++)
        if(table->indexes[u].mesg_types & type_flag)
            HGOTO_DONE(TRUE)

done:
    /* Release the master SOHM table */
    if(table && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, table, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to close SOHM master table")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_type_shared() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_get_fheap_addr
 *
 * Purpose:     Gets the address of the fractal heap used to store
 *              messages of type type_id.
 *
 * Return:      Non-negative on success/negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, October 3, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_get_fheap_addr(H5F_t *f, hid_t dxpl_id, unsigned type_id, haddr_t *fheap_addr)
{
    H5SM_master_table_t *table = NULL;  /* Shared object master table */
    ssize_t index_num;                  /* Which index */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5SM_get_fheap_addr, FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(fheap_addr);

    /* Look up the master SOHM table */
    if(NULL == (table = (H5SM_master_table_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, FAIL, "unable to load SOHM master table")

    /* Look up index for message type */
    if((index_num = H5SM_get_index(table, type_id)) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_CANTPROTECT, FAIL, "unable to find correct SOHM index")

    /* Retrieve heap address for index */
    *fheap_addr = table->indexes[index_num].heap_addr;

done:
    /* Release the master SOHM table */
    if(table && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, table, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to close SOHM master table")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_get_fheap_addr() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_create_index
 *
 * Purpose:     Allocates storage for an index, populating the HEADER struct.
 *
 * Return:      Non-negative on success/negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, May 2, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_create_index(H5F_t *f, H5SM_index_header_t *header, hid_t dxpl_id)
{
    haddr_t list_addr=HADDR_UNDEF;   /* Address of SOHM list */
    haddr_t tree_addr=HADDR_UNDEF;   /* Address of SOHM B-tree */
    H5HF_create_t fheap_cparam;      /* Fractal heap creation parameters */
    H5HF_t *fheap = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_create_index, FAIL)

    HDassert(header);
    HDassert(header->index_addr == HADDR_UNDEF);
    HDassert(header->btree_min <= header->list_max + 1);

    /* In most cases, the index starts as a list */
    if(header->list_max > 0)
    {
        header->index_type = H5SM_LIST;

        if((list_addr = H5SM_create_list(f, header, dxpl_id)) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_SOHM, H5E_CANTCREATE, FAIL, "list creation failed for SOHM index")

        header->index_addr = list_addr;
    }
    else /* index is a B-tree */
    {
        header->index_type = H5SM_BTREE;

        if(H5B2_create(f, dxpl_id, H5SM_INDEX, (size_t)H5SM_B2_NODE_SIZE,
                  (size_t)H5SM_SOHM_ENTRY_SIZE(f), H5SM_B2_SPLIT_PERCENT,
                  H5SM_B2_MERGE_PERCENT, &tree_addr) <0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTCREATE, FAIL, "B-tree creation failed for SOHM index")

        header->index_addr = tree_addr;
    }

    /* Create a heap to hold the shared messages that the list or B-tree will index */
    HDmemset(&fheap_cparam, 0, sizeof(fheap_cparam));
    fheap_cparam.managed.width = H5O_FHEAP_MAN_WIDTH;
    fheap_cparam.managed.start_block_size = H5O_FHEAP_MAN_START_BLOCK_SIZE;
    fheap_cparam.managed.max_direct_size = H5O_FHEAP_MAN_MAX_DIRECT_SIZE;
    fheap_cparam.managed.max_index = H5O_FHEAP_MAN_MAX_INDEX;
    fheap_cparam.managed.start_root_rows = H5O_FHEAP_MAN_START_ROOT_ROWS;
    fheap_cparam.checksum_dblocks = H5O_FHEAP_CHECKSUM_DBLOCKS;
    fheap_cparam.id_len = 0;
    fheap_cparam.max_man_size = H5O_FHEAP_MAX_MAN_SIZE;
    if(NULL == (fheap = H5HF_create(f, dxpl_id, &fheap_cparam)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "unable to create fractal heap")

    if(H5HF_get_heap_addr(fheap, &(header->heap_addr )) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGETSIZE, FAIL, "can't get fractal heap address")

#ifndef NDEBUG
{
    size_t fheap_id_len;             /* Size of a fractal heap ID */

    /* Sanity check ID length */
    if(H5HF_get_id_len(fheap, &fheap_id_len) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGETSIZE, FAIL, "can't get fractal heap ID length")
    HDassert(fheap_id_len == H5O_FHEAP_ID_LEN);
}
#endif /* NDEBUG */

done:
    /* Close the fractal heap if one has been created */
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_create_index */


/*-------------------------------------------------------------------------
 * Function:    H5SM_delete_index
 *
 * Purpose:     De-allocates storage for an index whose header is HEADER.
 *
 *              If DELETE_HEAP is TRUE, deletes the index's heap, eliminating
 *              it completely.
 *
 *              If DELETE_HEAP is FALSE, the heap is not deleted.  This is
 *              useful when deleting only the index header as the index is
 *              converted from a list to a B-tree and back again.
 *
 * Return:      Non-negative on success/negative on failure
 *
 * Programmer:  James Laird
 *              Thursday, January 4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_delete_index(H5F_t *f, H5SM_index_header_t *header, hid_t dxpl_id, hbool_t delete_heap)
{
    hsize_t     list_size;        /* Size of list on disk */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_delete_index, FAIL)

    /* Determine whether index is a list or a B-tree. */
    if(header->index_type == H5SM_LIST) {
        /* Eject entry from cache */
        if(H5AC_expunge_entry(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTREMOVE, FAIL, "unable to remove list index from cache")

        /* Free the file space used */
        list_size = H5SM_LIST_SIZE(f, header->list_max);
        if(H5MF_xfree(f, H5FD_MEM_SOHM_INDEX, dxpl_id, header->index_addr, list_size) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to free shared message list")
    } else {
        HDassert(header->index_type == H5SM_BTREE);

        /* Delete from the B-tree. */
        if(H5B2_delete(f, dxpl_id, H5SM_INDEX, header->index_addr, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to delete B-tree")

        /* Revert to list unless B-trees can have zero records */
        if(header->btree_min > 0)
            header->index_type = H5SM_LIST;
    }

    /* Free the index's heap if requested. */
    if(delete_heap == TRUE) {
        if(H5HF_delete(f, dxpl_id, header->heap_addr) < 0)
            HGOTO_ERROR(H5E_SOHM, H5E_CANTDELETE, FAIL, "unable to delete fractal heap")
    }

    header->index_addr = HADDR_UNDEF;
    header->heap_addr = HADDR_UNDEF;
    header->num_messages = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}/* end H5SM_delete_index */


/*-------------------------------------------------------------------------
 * Function:    H5SM_create_list
 *
 * Purpose:     Creates a list of SOHM messages.
 *
 *              Called when a new index is created from scratch or when a
 *              B-tree needs to be converted back into a list.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Monday, August 28, 2006
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5SM_create_list(H5F_t *f, H5SM_index_header_t * header, hid_t dxpl_id)
{
    H5SM_list_t *list = NULL;   /* List of messages */
    hsize_t x;                  /* Counter variable */
    hsize_t size = 0;           /* Size of list on disk */
    size_t num_entries;         /* Number of messages to create in list */
    haddr_t addr = HADDR_UNDEF; /* Address of the list on disk */
    haddr_t ret_value;

    FUNC_ENTER_NOAPI(H5SM_create_list, HADDR_UNDEF)

    HDassert(f);
    HDassert(header);

    num_entries = header->list_max;

    /* Allocate list in memory */
    if((list = H5FL_MALLOC(H5SM_list_t)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, HADDR_UNDEF, "file allocation failed for SOHM list")
    if((list->messages = (H5SM_sohm_t *)H5FL_ARR_MALLOC(H5SM_sohm_t, num_entries)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, HADDR_UNDEF, "file allocation failed for SOHM list")

    /* Initialize messages in list */
    HDmemset(list->messages, 0, sizeof(H5SM_sohm_t) * num_entries);

    for(x=0; x<num_entries; x++) {
        list->messages[x].ref_count=0;
    }

    list->header = header;

    /* Allocate space for the list on disk */
    size = H5SM_LIST_SIZE(f, num_entries);
    if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_SOHM_INDEX, dxpl_id, size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, HADDR_UNDEF, "file allocation failed for SOHM list")

    /* Put the list into the cache */
    if(H5AC_set(f, dxpl_id, H5AC_SOHM_LIST, addr, list, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, HADDR_UNDEF, "can't add SOHM list to cache")

    /* Set return value */
    ret_value = addr;

done:
    if(ret_value == HADDR_UNDEF)
    {
        if(list != NULL)
        {
            if(list->messages != NULL)
                H5FL_ARR_FREE(H5SM_sohm_t, list->messages);
            H5FL_FREE(H5SM_list_t, list);
        }
        if(addr != HADDR_UNDEF)
            H5MF_xfree(f, H5FD_MEM_SOHM_INDEX, dxpl_id, addr, size);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_create_list */


/*-------------------------------------------------------------------------
 * Function:    H5SM_convert_list_to_btree
 *
 * Purpose:     Given a list index, turns it into a B-tree index.  This is
 *              done when too many messages are added to the list.
 *
 *              Requires that *_LIST be a valid list and currently protected
 *              in the cache.  Unprotects (and expunges) *_LIST from the cache.
 *
 *              _LIST needs to be a double pointer so that the calling function
 *              knows if it is released from the cache if this function exits
 *              in error.  Trying to free it again will trigger an assert.
 *
 * Return:      Non-negative on success
 *              Negative on failure
 *
 * Programmer:  James Laird
 *              Thursday, January 4, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_convert_list_to_btree(H5F_t * f, H5SM_index_header_t * header,
                           H5SM_list_t **_list, H5HF_t *fheap, hid_t dxpl_id)
{
    H5SM_index_header_t temp_header;
    H5SM_list_t *list;
    H5SM_mesg_key_t key;
    haddr_t     tree_addr;
    size_t      x;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_convert_list_to_btree, FAIL)

    HDassert(_list && *_list);
    HDassert(header);

    list = *_list;
    /* Copy the old index header */
    HDmemcpy(&temp_header, header, sizeof(H5SM_index_header_t));

    if(H5B2_create(f, dxpl_id, H5SM_INDEX, (size_t)H5SM_B2_NODE_SIZE,
            (size_t)H5SM_SOHM_ENTRY_SIZE(f), H5SM_B2_SPLIT_PERCENT,
            H5SM_B2_MERGE_PERCENT, &tree_addr) <0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTCREATE, FAIL, "B-tree creation failed for SOHM index")

    /* Insert each record into the new B-tree */
    for(x = 0; x < header->list_max; x++)
    {
        /* Set up key values that all messages will use.  Since these messages
         * are in the heap, they have a heap ID and no encoding.
         */
        key.encoding = NULL;
        key.encoding_size = 0;
        key.fheap = fheap;

        if(list->messages[x].ref_count > 0)
        {
            key.message = list->messages[x];

            if(H5B2_insert(f, dxpl_id, H5SM_INDEX, tree_addr, &key) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "couldn't add SOHM to B-tree")
        }
    }

    /* Unprotect list in cache and release heap */
    if(H5AC_unprotect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, list, H5AC__DELETED_FLAG) < 0)
	HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to release SOHM list")
    *_list = list = NULL;

    /* Delete the old list index (but not its heap, which the new index is
     * still using!)
     */
    HDmemcpy(&temp_header, header, sizeof(H5SM_index_header_t));
    if(H5SM_delete_index(f, &temp_header, dxpl_id, FALSE) < 0)
        HGOTO_ERROR(H5E_SOHM, H5E_CANTDELETE, FAIL, "can't free list index");

    header->index_addr = tree_addr;
    header->index_type = H5SM_BTREE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5SM_convert_btree_to_list
 *
 * Purpose:     Given a B-tree index, turns it into a list index.  This is
 *              done when too many messages are deleted from the B-tree.
 *
 * Return:      Non-negative on success
 *              Negative on failure
 *
 * Programmer:  James Laird
 *              Thursday, January 4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_convert_btree_to_list(H5F_t * f, H5SM_index_header_t * header, hid_t dxpl_id)
{
    H5SM_list_t     *list = NULL;
    haddr_t          btree_addr;
    herr_t           ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_convert_btree_to_list, FAIL)

    /* Remember the address of the old B-tree, but change the header over to be
     * a list..
     */
    btree_addr = header->index_addr;

    header->num_messages = 0;
    header->index_type = H5SM_LIST;

    /* Create a new list index */
    if(HADDR_UNDEF == (header->index_addr = H5SM_create_list(f, header, dxpl_id)))
        HGOTO_ERROR(H5E_SOHM, H5E_CANTINIT, FAIL, "unable to create shared message list")

    if(NULL == (list = (H5SM_list_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, NULL, header, H5AC_WRITE)))
        HGOTO_ERROR(H5E_SOHM, H5E_CANTPROTECT, FAIL, "unable to load SOHM list index")

    /* Delete the B-tree and have messages copy themselves to the
     * list as they're deleted
     */
    if(H5B2_delete(f, dxpl_id, H5SM_INDEX, btree_addr, H5SM_btree_convert_to_list_op, list) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to delete B-tree")

done:
    /* Release the SOHM list from the cache */
    if(list && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, list, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to unprotect SOHM index")

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5SM_try_share
 *
 * Purpose:     Attempts to share an object header message.  If the message
 *              should be shared (if sharing has been enabled and this
 *              message qualified), turns the message into a shared message.
 *
 *              If not, returns FALSE and does nothing.
 *
 *              If this message was already shared, increments its reference
 *              count and leaves it otherwise unchanged.
 *
 * Return:      TRUE if message is now a SOHM
 *              FALSE if this message is not a SOHM
 *              Negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, May 2, 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5SM_try_share(H5F_t *f, hid_t dxpl_id, unsigned type_id, void *mesg)
{
    size_t              mesg_size;
    H5SM_master_table_t *table = NULL;
    unsigned            cache_flags = H5AC__NO_FLAGS_SET;
    ssize_t             index_num;
    htri_t              tri_ret;
    herr_t              ret_value = TRUE;

    FUNC_ENTER_NOAPI(H5SM_try_share, FAIL)

    /* Check whether this message ought to be shared or not */
    /* If sharing is disabled in this file, don't share the message */
    if(f->shared->sohm_addr == HADDR_UNDEF)
        HGOTO_DONE(FALSE);

    /* Type-specific check */
    if((tri_ret = H5O_msg_can_share(type_id, mesg)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADTYPE, FAIL, "can_share callback returned error")
    if(tri_ret == FALSE)
        HGOTO_DONE(FALSE);

    /* Look up the master SOHM table */
    if(NULL == (table = (H5SM_master_table_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, FAIL, "unable to load SOHM master table")

    /* Find the right index for this message type.  If there is no such index
     * then this type of message isn't shareable
     */
    if((index_num = H5SM_get_index(table, type_id)) < 0)
    {
        H5E_clear_stack(NULL); /*ignore error*/
        HGOTO_DONE(FALSE);
    } /* end if */

    /* If the message isn't big enough, don't bother sharing it */
    if(0 == (mesg_size = H5O_msg_mesg_size(f, type_id, mesg, (size_t)0)))
	HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "unable to get OH message size")
    if(mesg_size < table->indexes[index_num].min_mesg_size)
        HGOTO_DONE(FALSE);

    /* At this point, the message will be shared. */

    /* If the index hasn't been allocated yet, create it */
    if(table->indexes[index_num].index_addr == HADDR_UNDEF)
    {
        if(H5SM_create_index(f, &(table->indexes[index_num]), dxpl_id) < 0)
            HGOTO_ERROR(H5E_SOHM, H5E_CANTINIT, FAIL, "unable to create SOHM index")
        cache_flags |= H5AC__DIRTIED_FLAG;
    }

    /* Write the message as a shared message */
    if(H5SM_write_mesg(f, dxpl_id, &(table->indexes[index_num]), type_id, mesg, &cache_flags) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_CANTINSERT, FAIL, "can't write shared message")

done:
    /* Release the master SOHM table */
    if(table && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, table, cache_flags) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTRELEASE, FAIL, "unable to close SOHM master table")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_try_share() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_write_mesg
 *
 * Purpose:     Writes a message to an existing index and change the message
 *              to reflect that it's now shared.
 *
 *              If the message is already in the index, increment its
 *              reference count instead of writing it again and make the
 *              user's copy point to the copy we wrote before.
 *
 *              The index could be a list or a B-tree.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, May 2, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_write_mesg(H5F_t *f, hid_t dxpl_id, H5SM_index_header_t *header,
     unsigned type_id, void *mesg, unsigned *cache_flags_ptr)
{
    H5SM_list_t           *list = NULL;     /* List index */
    H5SM_mesg_key_t       key;      /* Key used to search the index */
    H5O_shared_t          shared;           /* Shared H5O message */
    hsize_t               list_pos;         /* Position in a list index */
    hbool_t               found = FALSE;    /* Was the message in the index? */
    H5HF_t                *fheap = NULL;         /* Fractal heap handle */
    size_t                buf_size;         /* Size of the encoded message */
    void *                encoding_buf=NULL; /* Buffer for encoded message */
    size_t               empty_pos=UFAIL;   /* Empty entry in list */
    herr_t                ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_write_mesg, FAIL)

    HDassert(cache_flags_ptr);
    HDassert(header);
    HDassert(header->index_type != H5SM_BADTYPE);

    /* Set up a shared message so that we can make this message shared once it's
     * written to the index.  This message is always stored to the heap, not to
     * an object header.
     */
    shared.flags = H5O_SHARED_IN_HEAP_FLAG;

    /* Encode the message to be written */
    if((buf_size = H5O_msg_raw_size(f, type_id, mesg)) <= 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADSIZE, FAIL, "can't find message size")
    if(NULL == (encoding_buf = H5MM_calloc(buf_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate buffer for encoding")

    if(H5O_msg_encode(f, type_id, (unsigned char *)encoding_buf, mesg) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "can't encode message to be shared")

    /* Open the fractal heap for this index */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, header->heap_addr)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Set up a key for the message to be written */
    key.message.fheap_id = 0; /* Message doesn't yet have a heap ID */
    key.message.hash = H5_checksum_lookup3(encoding_buf, buf_size, type_id);
    key.message.ref_count = 1;

    key.encoding = encoding_buf;
    key.encoding_size = buf_size;
    key.fheap = fheap;

    /* Assume the message is already in the index and try to increment its
     * reference count.  If this fails, the message isn't in the index after
     * all and we'll need to add it.
     */
    if(header->index_type == H5SM_LIST)
    {
        /* The index is a list; get it from the cache */
        if (NULL == (list = (H5SM_list_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, NULL, header, H5AC_WRITE)))
	    HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, FAIL, "unable to load SOHM index")

        /* See if the message is already in the index and get its location.
         * Also record the first empty list position we find in case we need it
         * later.
         */
        list_pos = H5SM_find_in_list(list, &key, &empty_pos);
        if(list_pos != UFAIL)
        {
            /* The message was in the index.  Increment its reference count. */
            ++(list->messages[list_pos].ref_count);

            /* Set up the shared location to point to the shared location */
            shared.u.heap_id = list->messages[list_pos].fheap_id;
            found = TRUE;
        }
    }
    else /* Index is a B-tree */
    {
        HDassert(header->index_type == H5SM_BTREE);

        /* If this returns failure, it means that the message wasn't found. */
        /* If it succeeds, the heap_id in the shared struct will be set */
        if(H5B2_modify(f, dxpl_id, H5SM_INDEX, header->index_addr, &key, H5SM_incr_ref, &shared.u.heap_id) >= 0)
	    found = TRUE;
    }

    /* If the message isn't in the list, add it */
    if(!found)
    {
        /* Put the message in the heap and record its new heap ID */
        if(H5HF_insert(fheap, dxpl_id, key.encoding_size, key.encoding, &shared.u.heap_id) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINSERT, FAIL, "unable to insert message into fractal heap")

        key.message.fheap_id = shared.u.heap_id;

        /* Check whether the list has grown enough that it needs to become a B-tree */
        if(header->index_type == H5SM_LIST && header->num_messages >= header->list_max)
        {
            if(H5SM_convert_list_to_btree(f, header, &list, fheap, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SOHM, H5E_CANTDELETE, FAIL, "unable to convert list to B-tree")
        }


        /* Insert the new message into the SOHM index */
        if(header->index_type == H5SM_LIST)
        {
            /* Index is a list.  Find an empty spot if we haven't already */
            if(empty_pos == UFAIL) {
                if((H5SM_find_in_list(list, NULL, &empty_pos) == UFAIL) || empty_pos == UFAIL)
                    HGOTO_ERROR(H5E_SOHM, H5E_CANTINSERT, FAIL, "unable to find empty entry in list")
            }

            /* Insert message into list */
            HDassert(list->messages[empty_pos].ref_count == 0);
            list->messages[empty_pos] = key.message;
            HDassert(list->messages[empty_pos].ref_count > 0);
        }
        else /* Index is a B-tree */
        {
            HDassert(header->index_type == H5SM_BTREE);

            if(H5B2_insert(f, dxpl_id, H5SM_INDEX, header->index_addr, &key) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "couldn't add SOHM to B-tree")
        }

        ++(header->num_messages);
        (*cache_flags_ptr) |= H5AC__DIRTIED_FLAG;
    }

    /* Change the original message passed in to reflect that it's now shared */
    if(H5O_msg_set_share(type_id, &shared, mesg) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "unable to set sharing information")

done:
    /* Release the fractal heap if we opened it */
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    /* If we got a list out of the cache, release it (it is always dirty after writing a message) */
    if(list && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, list, H5AC__DIRTIED_FLAG) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to close SOHM index")

    if(encoding_buf)
        encoding_buf = H5MM_xfree(encoding_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_write_mesg() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_try_delete
 *
 * Purpose:     Given an object header message that is being deleted,
 *              checks if it is a SOHM.  If so, decrements its reference
 *              count.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, May 2, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_try_delete(H5F_t *f, hid_t dxpl_id, unsigned type_id,
    const H5O_shared_t *sh_mesg)
{
    H5SM_master_table_t  *table = NULL;
    unsigned              cache_flags = H5AC__NO_FLAGS_SET;
    ssize_t               index_num;
    void                 *mesg_buf = NULL;
    void                 *native_mesg = NULL;
    herr_t                ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_try_delete, FAIL)

    HDassert(f);
    HDassert(sh_mesg);

    /* Make sure SHARED_IN_HEAP flag is set; if not, there's no message to delete */
    if(0 == (sh_mesg->flags & H5O_SHARED_IN_HEAP_FLAG))
        HGOTO_DONE(SUCCEED);

    HDassert(f->shared->sohm_addr != HADDR_UNDEF);

    /* Look up the master SOHM table */
    if(NULL == (table = (H5SM_master_table_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, FAIL, "unable to load SOHM master table")

    /* Find the correct index and try to delete from it */
    if((index_num = H5SM_get_index(table, type_id)) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_NOTFOUND, FAIL, "unable to find correct SOHM index")

    /* If mesg_buf is not NULL, the message's reference count has reached
     * zero and any file space it uses needs to be freed.  mesg_buf holds the
     * serialized form of the message.
     */
    if(H5SM_delete_from_index(f, dxpl_id, &(table->indexes[index_num]), type_id, sh_mesg, &cache_flags, &mesg_buf) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_CANTDELETE, FAIL, "unable to delete mesage from SOHM index")

    /* Release the master SOHM table */
    if(H5AC_unprotect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, table, cache_flags) < 0)
	HGOTO_ERROR(H5E_CACHE, H5E_CANTRELEASE, FAIL, "unable to close SOHM master table")
    table = NULL;

    /* If buf was allocated, delete the message it holds.  This message may
     * reference other shared messages that also need to be deleted, so the
     * master table needs to be unprotected when we do this.
     */
    if(mesg_buf) {
        if(NULL == (native_mesg = H5O_msg_decode(f, dxpl_id, type_id, mesg_buf)))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "can't decode shared message.")

        if(H5O_msg_delete(f, dxpl_id, type_id, native_mesg) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "can't delete shared message.")
    } /* end if */

done:
    /* Release the master SOHM table on error */
    if(table && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_TABLE, f->shared->sohm_addr, table, cache_flags) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTRELEASE, FAIL, "unable to close SOHM master table")

    if(native_mesg)
        H5O_msg_free(type_id, native_mesg);

    /* Free buf */
    if(mesg_buf)
        mesg_buf = H5MM_xfree(mesg_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_try_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_find_in_list
 *
 * Purpose:     Find a message's location in a list.  Also find the first
 *              empty location in the list (since if we don't find the
 *              message, we may want to insert it into an open spot).
 *
 *              If KEY is NULL, simply find the first empty location in the
 *              list.
 *
 *              If EMPTY_POS is NULL, don't store anything in it.
 *
 * Return:      Message's position in the list on success
 *              UFAIL if message couldn't be found
 *              empty_pos set to position of empty message or UFAIL.
 *
 * Programmer:  James Laird
 *              Tuesday, May 2, 2006
 *
 *-------------------------------------------------------------------------
 */
size_t
H5SM_find_in_list(H5SM_list_t *list, const H5SM_mesg_key_t *key, size_t *empty_pos)
{
    size_t               x;
    size_t               ret_value;

    FUNC_ENTER_NOAPI(H5SM_find_in_list, UFAIL)

    HDassert(list);
    /* Both key and empty_pos can be NULL, but not both! */
    HDassert(key || empty_pos);

    /* Initialize empty_pos to an invalid value */
    if(empty_pos)
        *empty_pos = UFAIL;

    /* Find the first (only) message equal to the key passed in.
     * Also record the first empty position we find.
     */
    for(x = 0; x < list->header->list_max; x++) {
        if(key && (list->messages[x].ref_count > 0) &&
                (0 == H5SM_message_compare(key, &(list->messages[x]))))
            HGOTO_DONE(x)
        else if(empty_pos && *empty_pos == UFAIL && list->messages[x].ref_count == 0)
            *empty_pos = x;
    }

    /* If we reached this point, we didn't find the message */
    HGOTO_ERROR(H5E_SOHM, H5E_NOTFOUND, UFAIL, "message not in list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_find_in_list */


/*-------------------------------------------------------------------------
 * Function:	H5SM_get_hash_fh_cb
 *
 * Purpose:	Callback for fractal heap operator, to make copy of link when
 *              when lookup up a link by index
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov  7 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_get_hash_fh_cb(const void *obj, size_t obj_len, void *_udata)
{
    H5SM_fh_ud_gh_t *udata = (H5SM_fh_ud_gh_t *)_udata;       /* User data for fractal heap 'op' callback */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_get_hash_fh_cb)

    /* Compute hash value on raw message */
    udata->hash = H5_checksum_lookup3(obj, obj_len, udata->type_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_get_hash_fh_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_delete_from_index
 *
 * Purpose:     Decrement the reference count for a particular message in this
 *              index.  If the reference count reaches zero, allocate a buffer
 *              to hold the serialized form of this message so that any
 *              resources it uses can be freed, and return this buffer in
 *              ENCODED_MESG.
 *
 * Return:      Non-negative on success
 *              Negative on failure
 *
 * Programmer:  James Laird
 *              Tuesday, May 2, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_delete_from_index(H5F_t *f, hid_t dxpl_id, H5SM_index_header_t *header,
    unsigned type_id, const H5O_shared_t * mesg, unsigned *cache_flags,
    void ** /*out*/ encoded_mesg)
{
    H5SM_list_t     *list = NULL;
    H5SM_mesg_key_t key;
    H5SM_sohm_t     message;            /* Deleted message returned from index */
    size_t          list_pos = UFAIL;   /* Position of the message in the list */
    H5HF_t         *fheap = NULL;       /* Fractal heap that contains the message */
    H5SM_fh_ud_gh_t udata;              /* User data for fractal heap 'op' callback */
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_delete_from_index, FAIL)

    HDassert(header);
    HDassert(mesg);
    HDassert(cache_flags);
    HDassert(mesg->flags & H5O_SHARED_IN_HEAP_FLAG);
    HDassert(*encoded_mesg == NULL);

    /* Open the heap that this message is in */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, header->heap_addr)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Prepare user data for fractal heap 'op' callback */
    udata.type_id = type_id;

    /* Compute the hash value for the B-tree lookup */
    if(H5HF_op(fheap, dxpl_id, &(mesg->u.heap_id), H5SM_get_hash_fh_cb, &udata) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't access message in fractal heap")


    /* Set up key for message to be deleted. */
    key.message.fheap_id = mesg->u.heap_id;
    key.message.hash = udata.hash;
    key.message.ref_count = 0;  /* Refcount isn't relevant here */

    key.encoding = NULL;
    key.encoding_size = 0;
    key.fheap = fheap;

    /* Try to find the message in the index */
    if(header->index_type == H5SM_LIST)
    {
        /* If the index is stored as a list, get it from the cache */
        if (NULL == (list = (H5SM_list_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, NULL, header, H5AC_WRITE)))
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTPROTECT, FAIL, "unable to load SOHM index")

        /* Find the message in the list */
        if((list_pos = H5SM_find_in_list(list, &key, NULL)) == UFAIL)
	    HGOTO_ERROR(H5E_SOHM, H5E_NOTFOUND, FAIL, "message not in index")

        --(list->messages[list_pos].ref_count);

        /* Copy the message */
        message = list->messages[list_pos];
    }
    else /* Index is a B-tree */
    {
        HDassert(header->index_type == H5SM_BTREE);

        /* If this returns failure, it means that the message wasn't found.
         * If it succeeds, a copy of the modified message will be returned. */
        if(H5B2_modify(f, dxpl_id, H5SM_INDEX, header->index_addr, &key, H5SM_decr_ref, &message) <0)
	    HGOTO_ERROR(H5E_SOHM, H5E_NOTFOUND, FAIL, "message not in index")
    }

    /* If the ref count is zero, delete the message from the index */
    if(message.ref_count <= 0)
    {
        size_t buf_size;

        /* Close the message being deleted, since it may need to adjust
         * other reference counts (e.g., an attribute needs to free its
         * shared datatype).
         */
        /* Get the size of the message in the heap */
        if(H5HF_get_obj_len(fheap, dxpl_id, &(message.fheap_id), &buf_size) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't get message size from fractal heap.")

        /* Allocate a buffer to hold the message */
        if(NULL == (*encoded_mesg = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Remove the message from the index */
        if(header->index_type == H5SM_LIST)
        {
            list->messages[list_pos].ref_count = 0;
        }
        else
        {
            if(H5B2_remove(f, dxpl_id, H5SM_INDEX, header->index_addr, &key, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTREMOVE, FAIL, "unable to delete message")
        }

        /* Updated the index header, so set its dirty flag */
        --header->num_messages;
        *cache_flags |= H5AC__DIRTIED_FLAG;


        /* Retrieve the message from the heap so we can return it (to free any
         * other messages it may reference)
         */
        if(H5HF_read(fheap, dxpl_id, &(message.fheap_id), *encoded_mesg) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "can't read message from fractal heap.")

        /* Remove the message from the heap */
        if(H5HF_remove(fheap, dxpl_id, &(message.fheap_id)) < 0)
            HGOTO_ERROR(H5E_SOHM, H5E_CANTREMOVE, FAIL, "unable to remove message from heap")


        /* If there are no messages left in the index, delete it */
        if(header->num_messages == 0) {

            /* Unprotect cache and release heap */
            if(list && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, list, H5AC__DELETED_FLAG) < 0)
	        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to release SOHM list")
            list = NULL;

            HDassert(fheap);
            if(H5HF_close(fheap, dxpl_id) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
            fheap = NULL;

            /* Delete the index and its heap */
            if(H5SM_delete_index(f, header, dxpl_id, TRUE) < 0)
                HGOTO_ERROR(H5E_SOHM, H5E_CANTDELETE, FAIL, "can't delete empty index")

        } else if(header->index_type == H5SM_BTREE && header->num_messages < header->btree_min)
        {
            /* Otherwise, if we've just passed the btree-to-list cutoff, convert
             * this B-tree into a list
             */
           if(H5SM_convert_btree_to_list(f, header, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SOHM, H5E_CANTINIT, FAIL, "unable to convert btree to list")
        } /* end if */
    } /* end if */

done:
    /* Release the SOHM list */
    if(list && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, list, H5AC__DIRTIED_FLAG) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to close SOHM index")

    /* Release the fractal heap if we opened it */
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    /* Free the serialized message buffer on error */
    if(ret_value < 0 && *encoded_mesg)
        *encoded_mesg = H5MM_xfree(*encoded_mesg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_delete_from_index() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_get_info
 *
 * Purpose:     Get the list-to-btree and btree-to-list cutoff numbers for
 *              an index within the master table.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Thursday, May 11, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_get_info(H5F_t *f, unsigned *index_flags, unsigned *minsizes,
    unsigned *list_max, unsigned *btree_min, hid_t dxpl_id)
{
    H5SM_master_table_t *table = NULL;
    haddr_t table_addr;
    uint8_t i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_get_info, FAIL)

    HDassert(f && f->shared);
    HDassert(f->shared->sohm_addr != HADDR_UNDEF);

    /* Convenience variables */
    table_addr = f->shared->sohm_addr;


    /* Read the rest of the SOHM table information from the cache */
    if(NULL == (table = (H5SM_master_table_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_TABLE, table_addr, NULL, NULL, H5AC_READ)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, FAIL, "unable to load SOHM master table")

    /* Return info */
    *list_max = table->indexes[0].list_max;
    *btree_min = table->indexes[0].btree_min;

    /* Get information about the individual SOHM indexes */
    for(i=0; i<table->num_indexes; ++i) {
        index_flags[i] = table->indexes[i].mesg_types;
        minsizes[i] = table->indexes[i].min_mesg_size;
    }

done:
    /* Release the master SOHM table if we took it out of the cache */
    if(table && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_TABLE, table_addr, table, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTRELEASE, FAIL, "unable to close SOHM master table")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_get_info() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_message_encode
 *
 * Purpose:	Serialize a H5SM_sohm_t struct into a buffer RAW.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_message_encode(const H5F_t UNUSED *f, uint8_t *raw, const void *_nrecord)
{
    const H5SM_sohm_t *message = (const H5SM_sohm_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_encode)

    /* Encode the SOHM's fields */
    UINT32ENCODE(raw, message->hash);
    UINT32ENCODE(raw, message->ref_count);
    UINT64ENCODE(raw, message->fheap_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_message_encode */


/*-------------------------------------------------------------------------
 * Function:	H5SM_message_decode
 *
 * Purpose:	Read an encoded SOHM message from RAW into an H5SM_sohm_t struct.
 *
 * Return:	Non-negative on success
 *              Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_message_decode(const H5F_t UNUSED *f, const uint8_t *raw, void *_nrecord)
{
    H5SM_sohm_t *message = (H5SM_sohm_t *)_nrecord;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_message_decode)

    /* Encode the SOHM's fields */
    UINT32DECODE(raw, message->hash);
    UINT32DECODE(raw, message->ref_count);
    UINT64DECODE(raw, message->fheap_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_message_decode */


/*-------------------------------------------------------------------------
 * Function:    H5SM_reconstitute
 *
 * Purpose:     Reconstitute a shared object header message structure from
 *              a plain heap ID.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Monday, December 18, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_reconstitute(H5O_shared_t *sh_mesg, H5O_fheap_id_t heap_id)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_reconstitute)

    /* Sanity check args */
    HDassert(sh_mesg);

    /* Set flag for shared message */
    sh_mesg->flags = H5O_SHARED_IN_HEAP_FLAG;
    sh_mesg->u.heap_id = heap_id;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_reconstitute() */


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
 * Function:    H5SM_get_refcount
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
H5SM_get_refcount(H5F_t *f, hid_t dxpl_id, unsigned type_id,
    const H5O_shared_t *sh_mesg, hsize_t *ref_count)
{    
    H5HF_t *fheap = NULL;               /* Fractal heap that contains shared messages */
    H5SM_master_table_t *table = NULL;  /* SOHM master table */
    H5SM_list_t *list = NULL;           /* SOHM index list for message type (if in list form) */
    H5SM_index_header_t *header=NULL;   /* Index header for message type */
    H5SM_mesg_key_t key;                /* Key for looking up message */
    H5SM_fh_ud_gh_t udata;              /* User data for fractal heap 'op' callback */
    H5SM_sohm_t message;                /* Shared message returned from callback */
    ssize_t index_num;                  /* Table index for message type */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_get_refcount)

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
    key.message.fheap_id = sh_mesg->u.heap_id;
    key.message.hash = udata.hash;
    key.message.ref_count = 0; /* Ref count isn't needed to find message */

    key.encoding = NULL;
    key.encoding_size = 0;
    key.fheap = fheap;

    /* Try to find the message in the index */
    if(header->index_type == H5SM_LIST) {
        size_t list_pos;        /* Position of the message in the list */

        /* If the index is stored as a list, get it from the cache */
        if(NULL == (list = (H5SM_list_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_LIST, header->index_addr, NULL, header, H5AC_READ)))
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTPROTECT, FAIL, "unable to load SOHM index")

        /* Find the message in the list */
        if((list_pos = H5SM_find_in_list(list, &key, NULL)) == UFAIL)
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
} /* end H5SM_get_refcount() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_table_debug
 *
 * Purpose:     Print debugging information for the master table.
 *
 *              If table_vers and num_indexes are not UFAIL, they are used
 *              instead of the values in the superblock.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Thursday, January 18, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_table_debug(H5F_t *f, hid_t dxpl_id, haddr_t table_addr,
                         FILE *stream, int indent, int fwidth,
                         unsigned table_vers, unsigned num_indexes)
{
    H5SM_master_table_t *table = NULL;  /* SOHM master table */
    unsigned x;                         /* Counter variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5SM_table_debug, FAIL)

    HDassert(f);
    HDassert(table_addr != HADDR_UNDEF);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /* If table_vers and num_indexes are UFAIL, replace them with values from
     * userblock
     */
    if(table_vers == UFAIL)
        table_vers = f->shared->sohm_vers;
    else if(table_vers = f->shared->sohm_vers)
	HDfprintf(stream, "*** SOHM TABLE VERSION DOESN'T MATCH VERSION IN SUPERBLOCK!\n");
    if(num_indexes == UFAIL)
        num_indexes = f->shared->sohm_nindexes;
    else if(num_indexes = f->shared->sohm_nindexes)
	HDfprintf(stream, "*** NUMBER OF SOHM INDEXES DOESN'T MATCH VALUE IN SUPERBLOCK!\n");

    /* Check arguments.  Version must be 0, the only version implemented so far */
    if(table_vers > HDF5_SHAREDHEADER_VERSION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown shared message table version")
    if(num_indexes == 0 || num_indexes > H5O_SHMESG_MAX_NINDEXES)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "number of indexes must be between 1 and H5O_SHMESG_MAX_NINDEXES")

    /* Look up the master SOHM table */
    if(NULL == (table = (H5SM_master_table_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_TABLE, table_addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, FAIL, "unable to load SOHM master table")

    HDfprintf(stream, "%*sShared Message Master Table...\n", indent, "");
    for(x=0; x<num_indexes; ++x) {
        HDfprintf(stream, "%*sIndex %d...\n", indent, "", x);
        HDfprintf(stream, "%*s%-*s %s\n", indent + 3, "", fwidth,
                "SOHM Index Type:",
                (table->indexes[x].index_type == H5SM_LIST ? "List" :
                (table->indexes[x].index_type == H5SM_BTREE ? "B-Tree" : "Unknown")));

        HDfprintf(stream, "%*s%-*s %a\n", indent + 3, "", fwidth,
                "Address of index:", table->indexes[x].index_addr);
        HDfprintf(stream, "%*s%-*s %a\n", indent + 3, "", fwidth,
                "Address of index's heap:", table->indexes[x].heap_addr);
        HDfprintf(stream, "%*s%-*s 0x%08x\n", indent + 3, "", fwidth,
                "Message type flags:", table->indexes[x].mesg_types);
        HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", fwidth,
                "Minimum size of messages:", table->indexes[x].min_mesg_size);
        HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", fwidth,
                "Number of messages:", table->indexes[x].num_messages);
        HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", fwidth,
                "Maximum list size:", table->indexes[x].list_max);
        HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", fwidth,
                "Minimum B-tree size:", table->indexes[x].btree_min);
    }

done:
    if(table && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_TABLE, table_addr, table, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTRELEASE, FAIL, "unable to close SOHM master table")
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5SM_list_debug
 *
 * Purpose:     Print debugging information for a SOHM list.
 *
 *              Relies on the list version and number of messages passed in.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Thursday, January 18, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SM_list_debug(H5F_t *f, hid_t dxpl_id, haddr_t list_addr,
                         FILE *stream, int indent, int fwidth,
                         unsigned table_vers, size_t num_messages)
{

    H5SM_list_t *list = NULL;           /* SOHM index list for message type (if in list form) */
    H5SM_index_header_t header;         /* A "false" header used to read the list */
    unsigned x;                         /* Counter variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5SM_list_debug, FAIL)

    HDassert(f);
    HDassert(num_messages != HADDR_UNDEF);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /* Check arguments.  Version must be 0, the only version implemented so far */
    if(table_vers > H5SM_LIST_VERSION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown shared message list version")
    if(num_messages == 0 || num_messages > H5O_SHMESG_MAX_LIST_SIZE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "number of indexes must be between 1 and H5O_SHMESG_MAX_NINDEXES")

    /* Create a temporary header using the arguments.  The cache needs this to load the list. */
    HDmemset(&header, 0, sizeof(H5SM_index_header_t));
    header.list_max = header.num_messages = num_messages;
    header.index_type = H5SM_LIST;
    header.index_addr = list_addr;

    /* Get the list from the cache */
    if (NULL == (list = (H5SM_list_t *)H5AC_protect(f, dxpl_id, H5AC_SOHM_LIST, list_addr, NULL, &header, H5AC_READ)))
	HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, FAIL, "unable to load SOHM index")

    HDfprintf(stream, "%*sShared Message List Index...\n", indent, "");
    for(x=0; x<num_messages; ++x) {
        HDfprintf(stream, "%*sShared Object Header Message %d...\n", indent, "", x);
        HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", fwidth,
                "Heap ID:", list->messages[x].fheap_id);
        HDfprintf(stream, "%*s%-*s %Zu\n", indent + 3, "", fwidth, /* JAMES: better flag for this? */
                "Hash value:", list->messages[x].hash);
        HDfprintf(stream, "%*s%-*s %u\n", indent + 3, "", fwidth,
                "Reference count:", list->messages[x].ref_count);
    }

done:
    if(list && H5AC_unprotect(f, dxpl_id, H5AC_SOHM_LIST, list_addr, list, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to close SOHM index")
    FUNC_LEAVE_NOAPI(ret_value)
}

