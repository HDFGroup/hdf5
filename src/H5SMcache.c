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

/****************/
/* Module Setup */
/****************/

#define H5SM_PACKAGE		/*suppress error about including H5SMpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg 	  */

/***********/
/* Headers */
/***********/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access                          */
#include "H5FLprivate.h"	/* Free Lists                           */
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
static herr_t H5SM_flush_table(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SM_master_table_t *table);
static H5SM_master_table_t *H5SM_load_table(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata1, void *table);
static herr_t H5SM_clear_table(H5F_t *f, H5SM_master_table_t *table, hbool_t destroy);
static herr_t H5SM_dest_table(H5F_t *f, H5SM_master_table_t* table);
static herr_t H5SM_table_size(const H5F_t *f, const H5SM_master_table_t *table, size_t *size_ptr);

static herr_t H5SM_flush_list(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SM_list_t *list);
static H5SM_list_t *H5SM_load_list(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata1, void *udata2);
static herr_t H5SM_clear_list(H5F_t *f, H5SM_list_t *list, hbool_t destroy);
static herr_t H5SM_dest_list(H5F_t *f, H5SM_list_t* list);
static herr_t H5SM_list_size(const H5F_t *f, const H5SM_list_t UNUSED *list, size_t *size_ptr);

/*********************/
/* Package Variables */
/*********************/
/* H5SM inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_SOHM_TABLE[1] = {{
    H5AC_SOHM_TABLE_ID,
      (H5AC_load_func_t) H5SM_load_table,
      (H5AC_flush_func_t) H5SM_flush_table,
      (H5AC_dest_func_t) H5SM_dest_table,
      (H5AC_clear_func_t)H5SM_clear_table,
      (H5AC_size_func_t) H5SM_table_size,
}};

const H5AC_class_t H5AC_SOHM_LIST[1] = {{
      H5AC_SOHM_LIST_ID,
      (H5AC_load_func_t) H5SM_load_list,
      (H5AC_flush_func_t) H5SM_flush_list,
      (H5AC_dest_func_t) H5SM_dest_list,
      (H5AC_clear_func_t)H5SM_clear_list,
      (H5AC_size_func_t) H5SM_list_size,
}};

/* Declare a free list to manage data to/from disk */
H5FL_BLK_DEFINE_STATIC(shared_mesg_cache);

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:	H5SM_flush_table
 *
 * Purpose:	Flushes (and destroys) the table of Shared Object Header
 *              Message indexes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_flush_table(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SM_master_table_t *table)
{
    uint8_t *buf=NULL;                /* Temporary buffer */
    herr_t          ret_value=SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5SM_flush_table)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(table);

    if(table->cache_info.is_dirty) {
        uint8_t  *p;                 /* Pointer into raw data buffer */
        size_t	 size;               /* Header size on disk */
        uint32_t computed_chksum;    /* Computed metadata checksum value */
        int      x;                  /* Counter variable */

        /* Verify that we're writing version 0 of the table; this is the only
         * version defined so far.
         */
        HDassert(f->shared->sohm_vers == HDF5_SHAREDHEADER_VERSION);

        /* Encode the master table and all of the index headers as one big blob */
        size = H5SM_TABLE_SIZE(f) + (H5SM_INDEX_HEADER_SIZE(f) * table->num_indexes);

        /* Allocate the buffer */
        if(NULL == (buf = H5FL_BLK_MALLOC(shared_mesg_cache, size)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Encode the master table */
        p = buf;

        /* Encode magic number */
        HDmemcpy(p, H5SM_TABLE_MAGIC, (size_t)H5SM_TABLE_SIZEOF_MAGIC);
        p += H5SM_TABLE_SIZEOF_MAGIC;

        /* Encode each index header */
        for(x=0; x<table->num_indexes; ++x) {
            *p++ = H5SM_LIST_VERSION;   /* Encode version for this list. */
            *p++ = table->indexes[x].index_type;      /* Is message index a list or a B-tree? */

            UINT16ENCODE(p, table->indexes[x].mesg_types);    /* Type of messages in the index */
            UINT32ENCODE(p, table->indexes[x].min_mesg_size); /* Minimum size of message to share */
            UINT16ENCODE(p, table->indexes[x].list_max);  /* List cutoff; fewer than this number and index becomes a list */
            UINT16ENCODE(p, table->indexes[x].btree_min);  /* B-tree cutoff; more than this number and index becomes a B-tree */
            UINT16ENCODE(p, table->indexes[x].num_messages);   /* Number of messages shared */
            H5F_addr_encode(f, &p, table->indexes[x].index_addr); /* Address of the actual index */
            H5F_addr_encode(f, &p, table->indexes[x].heap_addr); /* Address of the index's heap */
        }

        /* Compute checksum on buffer */
        computed_chksum = H5_checksum_metadata(buf, (size - H5SM_SIZEOF_CHECKSUM), 0);
        UINT32ENCODE(p, computed_chksum);

        /* Write the table to disk */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_SOHM_TABLE, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFLUSH, FAIL, "unable to save sohm table to disk")

	table->cache_info.is_dirty = FALSE;

    } /* end if */

    if(destroy)
        if(H5SM_dest_table(f, table) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to destroy sohm table")

done:
    /* Free buffer if allocated */
    if(buf)
        buf = H5FL_BLK_FREE(shared_mesg_cache, buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_flush_table */



/*-------------------------------------------------------------------------
 * Function:	H5SM_load_table
 *
 * Purpose:	Loads the master table of Shared Object Header Message
 *              indexes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5SM_master_table_t *
H5SM_load_table(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void UNUSED *udata2)
{
    H5SM_master_table_t *table = NULL;
    size_t        table_size;          /* Size of SOHM master table on disk */
    uint8_t       *buf=NULL;           /* Reading buffer */
    const uint8_t *p;                  /* Pointer into input buffer */
    uint32_t      stored_chksum;       /* Stored metadata checksum value */
    uint32_t      computed_chksum;     /* Computed metadata checksum value */
    uint8_t       x;                   /* Counter variable for index headers */
    H5SM_master_table_t *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5SM_load_table)

    /* Verify that we're reading version 0 of the table; this is the only
        * version defined so far.
        */
    HDassert(f->shared->sohm_vers == HDF5_SHAREDHEADER_VERSION);

    /* Allocate space for the master table in memory */
    if(NULL == (table = (H5SM_master_table_t *)H5MM_calloc(sizeof(H5SM_master_table_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read number of indexes and version from file superblock */
    table->num_indexes = f->shared->sohm_nindexes;

    HDassert(addr == f->shared->sohm_addr);
    HDassert(addr != HADDR_UNDEF);
    HDassert(table->num_indexes > 0);

    /* Compute the size of the SOHM table header on disk.  This is the "table" itself
     * plus each index within the table
     */
    table_size = H5SM_TABLE_SIZE(f) + (table->num_indexes * H5SM_INDEX_HEADER_SIZE(f));

    /* Allocate temporary buffer */
    if(NULL == (buf = H5FL_BLK_MALLOC(shared_mesg_cache, table_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_SOHM_TABLE, addr, table_size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_READERROR, NULL, "can't read SOHM table")

    p = buf;

    /* Check magic number */
    if(HDmemcmp(p, H5SM_TABLE_MAGIC, (size_t)H5SM_TABLE_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "bad SOHM table signature");
    p += H5SM_TABLE_SIZEOF_MAGIC;

    /* Don't count the checksum in the table size yet, since it comes after
     * all of the index headers
     */
    HDassert((size_t)(p - buf) == H5SM_TABLE_SIZE(f) - H5SM_SIZEOF_CHECKSUM);

    /* Allocate space for the index headers in memory*/
    if(NULL == (table->indexes = (H5SM_index_header_t *)H5FL_ARR_MALLOC(H5SM_index_header_t, (size_t)table->num_indexes)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for SOHM indexes")

    /* Read in the index headers */
    for(x=0; x<table->num_indexes; ++x) {
        if (H5SM_LIST_VERSION != *p++)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "bad shared message list version number")

        table->indexes[x].index_type= *p++;  /* type of the index (list or B-tree) */
        UINT16DECODE(p, table->indexes[x].mesg_types);
        UINT32DECODE(p, table->indexes[x].min_mesg_size);
        UINT16DECODE(p, table->indexes[x].list_max);
        UINT16DECODE(p, table->indexes[x].btree_min);
        UINT16DECODE(p, table->indexes[x].num_messages);
        H5F_addr_decode(f, &p, &(table->indexes[x].index_addr));
        H5F_addr_decode(f, &p, &(table->indexes[x].heap_addr));
    }

    /* Read in checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == table_size);

    /* Compute checksum on entire header */
    computed_chksum = H5_checksum_metadata(buf, (table_size - H5SM_SIZEOF_CHECKSUM), 0);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_SOHM, H5E_BADVALUE, NULL, "incorrect metadata checksum for shared message table");

    ret_value = table;

done:
    /* Free buffer if allocated */
    if(buf)
        buf = H5FL_BLK_FREE(shared_mesg_cache, buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_load_table */ 



/*-------------------------------------------------------------------------
 * Function:	H5SM_clear_table
 *
 * Purpose:	Mark this table as no longer being dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_clear_table(H5F_t *f, H5SM_master_table_t *table, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_clear_table)

    /*
     * Check arguments.
     */
    HDassert(table);

    /* Reset the dirty flag.  */
    table->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5SM_dest_table(f, table) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to delete SOHM master table")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_clear_table */



/*-------------------------------------------------------------------------
 * Function:	H5SM_dest_table
 *
 * Purpose:	Frees memory used by the SOHM table.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_dest_table(H5F_t UNUSED *f, H5SM_master_table_t* table)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_dest_table)

    HDassert(table);
    HDassert(table->indexes);

    H5FL_ARR_FREE(H5SM_index_header_t, table->indexes);

    H5MM_free(table);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_dest_table */


/*-------------------------------------------------------------------------
 * Function:	H5SM_table_size
 *
 * Purpose:	Returns the size of the table encoded on disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_size(const H5F_t *f, const H5SM_master_table_t *table, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_table_size)

    /* check arguments */
    HDassert(f);
    HDassert(table);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5SM_TABLE_SIZE(f) + (table->num_indexes * H5SM_INDEX_HEADER_SIZE(f));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_table_size */



/*-------------------------------------------------------------------------
 * Function:	H5SM_flush_list
 *
 * Purpose:	Flush this list index.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_flush_list(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SM_list_t *list)
{
    uint8_t *buf=NULL;               /* Temporary buffer */
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5SM_flush_list)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(list);
    HDassert(list->header);

    if (list->cache_info.is_dirty) {
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;               /* Header size on disk */
        uint32_t computed_chksum;   /* Computed metadata checksum value */
        hsize_t x;
        hsize_t mesgs_written;

        size = H5SM_LIST_SIZE(f, list->header->num_messages);

        /* Allocate temporary buffer */
        if(NULL == (buf = H5FL_BLK_MALLOC(shared_mesg_cache, size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Encode the list */
        p = buf;

        /* Encode magic number */
        HDmemcpy(p, H5SM_LIST_MAGIC, (size_t)H5SM_LIST_SIZEOF_MAGIC);
        p += H5SM_LIST_SIZEOF_MAGIC;

        /* Write messages from the messages array to disk */
        mesgs_written = 0;
        for(x=0; x<list->header->list_max && mesgs_written < list->header->num_messages; x++) {
            if(list->messages[x].location != H5SM_NO_LOC) {
                if(H5SM_message_encode(f, p, &(list->messages[x]))< 0)
                    HGOTO_ERROR(H5E_SOHM, H5E_CANTFLUSH, FAIL, "unable to write shared message to disk")

                p+=H5SM_SOHM_ENTRY_SIZE(f);
                ++mesgs_written;
            }
        }

        HDassert(mesgs_written == list->header->num_messages);

        /* Compute checksum on buffer */
        computed_chksum = H5_checksum_metadata(buf, (size - H5SM_SIZEOF_CHECKSUM), 0);
        UINT32ENCODE(p, computed_chksum);

        /* Write the list to disk */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_SOHM_INDEX, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFLUSH, FAIL, "unable to save sohm table to disk")

        list->cache_info.is_dirty = FALSE;
    }

    if(destroy)
        if(H5SM_dest_list(f, list) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to destroy list")

done:
    /* Free buffer if allocated */
    if(buf)
        buf = H5FL_BLK_FREE(shared_mesg_cache, buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_flush_list */



/*-------------------------------------------------------------------------
 * Function:	H5SM_load_list
 *
 * Purpose:	Loads a list of SOHM messages.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5SM_list_t *
H5SM_load_list(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void *udata2)
{
    H5SM_list_t *list;              /* The SOHM list being read in */
    H5SM_index_header_t *header = (H5SM_index_header_t *) udata2;     /* Index header for this list */
    size_t size;                    /* Size of SOHM list on disk */
    uint8_t *buf = NULL;            /* Reading buffer */
    uint8_t *p;                     /* Pointer into input buffer */
    uint32_t stored_chksum;         /* Stored metadata checksum value */
    uint32_t computed_chksum;       /* Computed metadata checksum value */
    hsize_t x;                      /* Counter variable for messages in list */
    H5SM_list_t *ret_value=NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5SM_load_list)

    HDassert(header);

    /* Allocate space for the SOHM list data structure */
    if(NULL == (list = H5FL_MALLOC(H5SM_list_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&list->cache_info, 0, sizeof(H5AC_info_t));

    /* Allocate list in memory as an array*/
    if((list->messages = (H5SM_sohm_t *)H5FL_ARR_MALLOC(H5SM_sohm_t, header->list_max)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "file allocation failed for SOHM list")

    list->header = header;

    /* Compute the size of the SOHM list on disk */
    size = H5SM_LIST_SIZE(f, header->num_messages);

    /* Allocate temporary buffer */
    if(NULL == (buf = H5FL_BLK_MALLOC(shared_mesg_cache, size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read list from disk */
    if(H5F_block_read(f, H5FD_MEM_SOHM_INDEX, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_READERROR, NULL, "can't read SOHM list")
    p = buf;

    /* Check magic number */
    if(HDmemcmp(p, H5SM_LIST_MAGIC, (size_t)H5SM_LIST_SIZEOF_MAGIC))
        HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "bad SOHM list signature");
    p += H5SM_LIST_SIZEOF_MAGIC;

    /* Read messages into the list array */
    for(x = 0; x < header->num_messages; x++)
    {
        if(H5SM_message_decode(f, p, &(list->messages[x])) < 0)
            HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "can't decode shared message");
        p += H5SM_SOHM_ENTRY_SIZE(f);
    } /* end for */

    /* Read in checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == size);


    /* Compute checksum on entire header */
    computed_chksum = H5_checksum_metadata(buf, (size - H5SM_SIZEOF_CHECKSUM), 0);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_SOHM, H5E_BADVALUE, NULL, "incorrect metadata checksum for shared message list");


    /* Initialize the rest of the array */
    for(x = header->num_messages; x < header->list_max; x++)
        list->messages[x].location = H5SM_NO_LOC;

    ret_value = list;

done:
    /* Free buffer if allocated */
    if(buf)
        buf = H5FL_BLK_FREE(shared_mesg_cache, buf);

    if(ret_value == NULL) {
        if(list) {
            if(list->messages)
                H5FL_ARR_FREE(H5SM_sohm_t, list->messages);
            H5FL_FREE(H5SM_list_t, list);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_load_list */


/*-------------------------------------------------------------------------
 * Function:	H5SM_clear_list
 *
 * Purpose:	Marks a list as not dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_clear_list(H5F_t *f, H5SM_list_t *list, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_clear_list)

    /*
     * Check arguments.
     */
    HDassert(list);

    /* Reset the dirty flag.  */
    list->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5SM_dest_list(f, list) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to destroy SOHM list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end of H5SM_clear_list */


/*-------------------------------------------------------------------------
 * Function:	H5SM_dest_list
 *
 * Purpose:	Frees all memory used by the list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_dest_list(H5F_t UNUSED *f, H5SM_list_t* list)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_dest_list)

    HDassert(list);
    HDassert(list->messages);

    H5FL_ARR_FREE(H5SM_sohm_t, list->messages);

    H5FL_FREE(H5SM_list_t, list);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_dest_list */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_size
 *
 * Purpose:	Gets the size of a list on disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_size(const H5F_t UNUSED *f, const H5SM_list_t *list, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_list_size)

    /* check arguments */
    HDassert(f);
    HDassert(list);
    HDassert(list->header);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5SM_LIST_SIZE(f, list->header->list_max);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_list_size */



