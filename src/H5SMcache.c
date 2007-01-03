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
/* JAMES: these need to go first or else FILE isn't defined in H5Fpkg.h */
/* JAMES: which of these are really needed?  H5Fpkg.h, even? */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/

#include "H5Fpkg.h"		/* File access                          */
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5FOprivate.h"        /* File objects                         */
#include "H5HLprivate.h"	/* Local heaps				*/
#include "H5MFprivate.h"        /* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/
#include "H5SMpkg.h"            /* Shared object header messages        */
#include "H5FDprivate.h"	/* File drivers				*/

/****************/
/* Local Macros */
/****************/
#define H5F_TABLEBUF_SIZE  H5SM_TABLE_SIZEOF_MAGIC + 20 + (H5SM_MAX_NINDEXES * 26)
/* JAMES: should this change according to address size? */
#define H5F_LISTBUF_SIZE  H5SM_LIST_SIZEOF_MAGIC + H5SM_MAX_LIST_ELEMS * 16

#define H5SM_LIST_VERSION	0	/* Verion of Shared Object Header Message List Indexes */

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
    herr_t          ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_flush_table, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(table);

    if (table->cache_info.is_dirty) {
        uint8_t buf[H5F_TABLEBUF_SIZE];  /* Temporary buffer */   /* JAMES This is big. Do I need to use H5FL_BLK_MALLOC instead? */
        uint8_t  *p;                 /* Pointer into raw data buffer */
        size_t	 size;               /* Header size on disk */
        uint32_t computed_chksum;    /* Computed metadata checksum value */
        int      x;                  /* Counter variable */

        /* Encode the master table and all of the index headers as one big blob */
        size = H5SM_TABLE_SIZE(f) + (H5SM_INDEX_HEADER_SIZE(f) * table->num_indexes);

        /* Encode the master table */
        p = buf;

        /* Encode magic number */
        HDmemcpy(p, H5SM_TABLE_MAGIC, (size_t)H5SM_TABLE_SIZEOF_MAGIC);
        p += H5SM_TABLE_SIZEOF_MAGIC;

        *p++ = table->version; /* Version */

        /* Encode each index header */
        for(x=0; x<table->num_indexes; ++x) {
            *p++ = table->indexes[x].index_type;      /* Is message index a list or a B-tree? */

            UINT16ENCODE(p, table->indexes[x].mesg_types);    /* Type of messages in the index */
            UINT32ENCODE(p, table->indexes[x].min_mesg_size); /* Minimum size of message to share */
            UINT16ENCODE(p, table->indexes[x].list_to_btree);  /* List cutoff; fewer than this number and index becomes a list */
            UINT16ENCODE(p, table->indexes[x].btree_to_list);  /* B-tree cutoff; more than this number and index becomes a B-tree */
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
    }

    if(destroy)
        if(H5SM_dest_table(f, table) < 0)
	    HGOTO_ERROR(H5E_SOHM, H5E_CANTFREE, FAIL, "unable to destroy sohm table")

done:
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

    FUNC_ENTER_NOAPI(H5SM_load_table, NULL)

    /* Allocate space for the master table in memory */
    if(NULL == (table = H5MM_calloc(sizeof(H5SM_master_table_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read number of indexes and version from file superblock */
    table->num_indexes = f->shared->sohm_nindexes;
    table->version = f->shared->sohm_vers;

    HDassert(addr == f->shared->sohm_addr);
    HDassert(addr != HADDR_UNDEF);
    HDassert(table->num_indexes > 0);

    /* Compute the size of the SOHM table header on disk.  This is the "table" itself
     * plus each index within the table
     */
    table_size = H5SM_TABLE_SIZE(f) + (table->num_indexes * H5SM_INDEX_HEADER_SIZE(f));

    /* Allocate temporary buffer */
    if(NULL == (buf = HDmalloc(table_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_SOHM_TABLE, addr, table_size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_READERROR, NULL, "can't read SOHM table")

    p = buf;

    /* Check magic number */
    if(HDmemcmp(p, H5SM_TABLE_MAGIC, (size_t)H5SM_TABLE_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "bad SOHM table signature");
    p += H5SM_TABLE_SIZEOF_MAGIC;

    /* Version number */
    if (table->version != *p++)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "wrong SOHM table version number")

    /* Don't count the checksum in the table size yet, since it comes after
     * all of the index headers
     */
    HDassert((size_t)(p - buf) == H5SM_TABLE_SIZE(f) - H5SM_SIZEOF_CHECKSUM);

    /* Allocate space for the index headers in memory*/
    if(NULL == (table->indexes = H5FL_ARR_MALLOC(H5SM_index_header_t, (size_t)table->num_indexes)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for SOHM indexes")

    /* Read in the index headers */
    for(x=0; x<table->num_indexes; ++x) {
        table->indexes[x].index_type= *p++;  /* type of the index (list or B-tree) */

        UINT16DECODE(p, table->indexes[x].mesg_types);
        UINT32DECODE(p, table->indexes[x].min_mesg_size);
        UINT16DECODE(p, table->indexes[x].list_to_btree);
        UINT16DECODE(p, table->indexes[x].btree_to_list);
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
    /* Free buffer if it was allocated */
    if(buf)
        HDfree(buf);

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
    FUNC_ENTER_NOAPI_NOFUNC(H5SM_dest_table)

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
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5SM_flush_list, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(list);
    HDassert(list->header);

    if (list->cache_info.is_dirty) {
        uint8_t buf[H5F_LISTBUF_SIZE];  /* Temporary buffer */   /* JAMES Do I need to use H5FL_BLK_MALLOC instead? */
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;               /* Header size on disk */
        uint32_t computed_chksum;   /* Computed metadata checksum value */
        hsize_t x;

        /* JAMES: consider only writing as many messages as necessary, and then adding a
         * blank "end of list" message or something?
         */
        size = H5SM_LIST_SIZE(f, list->header->num_messages);

        /* Encode the list */
        p = buf;

        /* Encode magic number */
        HDmemcpy(p, H5SM_LIST_MAGIC, (size_t)H5SM_LIST_SIZEOF_MAGIC);
        p += H5SM_LIST_SIZEOF_MAGIC;

        /* Encode version */
        *p++ = H5SM_LIST_VERSION;

        /* Write messages from the messages array to disk */
        /* JAMES: we have to search the whole array.  not the best way to do it; could go until we've written
         * num_messages */
        for(x=0; x<list->header->list_to_btree; x++) {
            if(list->messages[x].fheap_id != 0 && list->messages[x].hash != H5O_HASH_UNDEF) {
              /* JAMES: use H5SM_message_encode here */
              UINT32ENCODE(p, list->messages[x].hash);  /* Read the hash value for this message */
              UINT32ENCODE(p, list->messages[x].ref_count);  /* Read the reference count for this message */
              UINT64ENCODE(p, list->messages[x].fheap_id); /* Get the heap ID for the message */
            }
        }

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

    FUNC_ENTER_NOAPI(H5SM_load_list, NULL)

    HDassert(header);

    /* Allocate space for the SOHM list data structure and initialize list JAMES don't need to initialize all of list */
    if(NULL == (list = H5FL_MALLOC(H5SM_list_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&list->cache_info, 0, sizeof(H5AC_info_t));

    /* Allocate list in memory as an array*/
    if((list->messages = H5FL_ARR_MALLOC(H5SM_sohm_t, header->list_to_btree)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "file allocation failed for SOHM list")

    list->header = header;

    /* Compute the size of the SOHM list on disk */
    size = H5SM_LIST_SIZE(f, header->num_messages);

    /* Allocate temporary buffer */
    /* JAMES: is BLK_MALLOC somehow better for this? */
    if(NULL == (buf = HDmalloc(size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read list from disk */
    if(H5F_block_read(f, H5FD_MEM_SOHM_INDEX, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_SOHM, H5E_READERROR, NULL, "can't read SOHM list")
    p = buf;

    /* Check magic number */
    if(HDmemcmp(p, H5SM_LIST_MAGIC, (size_t)H5SM_LIST_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "bad SOHM list signature");
    p += H5SM_LIST_SIZEOF_MAGIC;

    /* Check version JAMES: should be in master table, not list */
    if (H5SM_LIST_VERSION != *p++)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "wrong shared message list version number")

    /* Read messages into the list array */
    for(x=0; x<header->num_messages; x++)
    {
        UINT32DECODE(p, list->messages[x].hash);  /* Read the hash value for this message */
        UINT32DECODE(p, list->messages[x].ref_count);  /* Read the reference count for this message */
        UINT64DECODE(p, list->messages[x].fheap_id); /* Get the heap ID for the message */
    }

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
    for(x=header->num_messages; x<header->list_to_btree; x++)
    {
        list->messages[x].fheap_id = 0; /* JAMES: would making this one operation make it faster? */
        list->messages[x].ref_count = 0;
        list->messages[x].hash = H5O_HASH_UNDEF;
    }

    ret_value = list;
done:
    if(buf)
        HDfree(buf);

    if(ret_value == NULL) {
        if(list) {
            if(list->messages) {
                H5FL_ARR_FREE(H5SM_sohm_t, list->messages);
            }
            H5FL_FREE(H5SM_list_t, list);
        }
    }
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
    FUNC_ENTER_NOAPI_NOFUNC(H5SM_dest_list)

    HDassert(list);
    HDassert(list->messages);

    H5FL_ARR_FREE(H5SM_sohm_t, list->messages);

    H5FL_FREE(H5SM_list_t, list);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_dest_list */


/* JAMES: should this number be constant, or should it increase and decrease as
 * messages are added and removed? */
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
    *size_ptr = H5SM_LIST_SIZE(f, list->header->list_to_btree); /* JAMES: might want to have variable-sized lists */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_list_size */



