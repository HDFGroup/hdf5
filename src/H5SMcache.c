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

#define H5F_PACKAGE		/*suppress error about including H5Fpkg 	  */
#define H5SM_PACKAGE		/*suppress error about including H5SMpkg	  */


/***********/
/* Headers */
/***********/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access                          */
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5SMpkg.h"            /* Shared object header messages        */
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */

static herr_t H5SM_table_get_load_size(const void *_udata, size_t *image_len);
static void *H5SM_table_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5SM_table_image_len(const void *thing, size_t *image_len);
static herr_t H5SM_table_serialize(const H5F_t * f, hid_t dxpl_id, haddr_t addr,
    size_t len, void *image, void *thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image);
static herr_t H5SM_table_free_icr(void *thing);

static herr_t H5SM_list_get_load_size(const void *_udata, size_t *image_len);
static void *H5SM_list_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5SM_list_image_len(const void *thing, size_t *image_len);
static herr_t H5SM_list_serialize(const H5F_t * f, hid_t dxpl_id, haddr_t addr,
    size_t len, void *image, void *thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image);
static herr_t H5SM_list_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/
/* H5SM inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_SOHM_TABLE[1] = {{
    H5AC_SOHM_TABLE_ID,
    "shared object header message",
    H5FD_MEM_SOHM_TABLE,
    H5AC__CLASS_NO_FLAGS_SET,
    H5SM_table_get_load_size,
    H5SM_table_deserialize,
    H5SM_table_image_len,
    H5SM_table_serialize,
    H5SM_table_free_icr,
}};

const H5AC_class_t H5AC_SOHM_LIST[1] = {{
    H5AC_SOHM_LIST_ID,
    "shared object header message",
    H5FD_MEM_SOHM_INDEX,
    H5AC__CLASS_NO_FLAGS_SET,
    H5SM_list_get_load_size,
    H5SM_list_deserialize,
    H5SM_list_image_len,
    H5SM_list_serialize,
    H5SM_list_free_icr,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5SM_table_get_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              May 18, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_get_load_size(const void *_udata, size_t *image_len)
{
    const H5SM_table_cache_ud_t *udata = (const H5SM_table_cache_ud_t *)_udata; /* User data for callback */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_table_get_load_size)

    /* Check arguments */
    HDassert(udata);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = H5SM_TABLE_SIZE(udata->f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_table_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_table_deserialize
 *
 * Purpose:	Deserialize the data structure from disk.
 *
 * Return:	Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static void *
H5SM_table_deserialize(const void *image, size_t UNUSED len, void *_udata,
    hbool_t UNUSED *dirty)
{
    H5SM_master_table_t *table = NULL;
    H5SM_table_cache_ud_t *udata = (H5SM_table_cache_ud_t *)_udata; /* User data for callback */
    const uint8_t *p;                   /* Pointer into input buffer */
    uint32_t      stored_chksum;        /* Stored metadata checksum value */
    uint32_t      computed_chksum;      /* Computed metadata checksum value */
    size_t        x;                    /* Counter variable for index headers */
    H5SM_master_table_t *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5SM_table_deserialize)

    /* Verify that we're reading version 0 of the table; this is the only
     * version defined so far.
     */
    HDassert(image);
    HDassert(udata);
    HDassert(udata->f->shared->sohm_vers == HDF5_SHAREDHEADER_VERSION);

    /* Allocate space for the master table in memory */
    if(NULL == (table = H5FL_CALLOC(H5SM_master_table_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read number of indexes and version from file superblock */
    table->num_indexes = udata->f->shared->sohm_nindexes;
    table->table_size = H5SM_TABLE_SIZE(udata->f);

    HDassert(table->num_indexes > 0);

    /* Get temporary pointer to serialized table */
    p = (const uint8_t *)image;

    /* Check magic number */
    if(HDmemcmp(p, H5SM_TABLE_MAGIC, (size_t)H5SM_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "bad SOHM table signature")
    p += H5SM_SIZEOF_MAGIC;

    /* Allocate space for the index headers in memory*/
    if(NULL == (table->indexes = (H5SM_index_header_t *)H5FL_ARR_MALLOC(H5SM_index_header_t, (size_t)table->num_indexes)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for SOHM indexes")

    /* Read in the index headers */
    for(x = 0; x < table->num_indexes; ++x) {
        /* Verify correct version of index list */
        if(H5SM_LIST_VERSION != *p++)
            HGOTO_ERROR(H5E_FILE, H5E_VERSION, NULL, "bad shared message list version number")

        /* Type of the index (list or B-tree) */
        table->indexes[x].index_type= *p++;

        /* Type of messages in the index */
        UINT16DECODE(p, table->indexes[x].mesg_types);

        /* Minimum size of message to share */
        UINT32DECODE(p, table->indexes[x].min_mesg_size);

        /* List cutoff; fewer than this number and index becomes a list */
        UINT16DECODE(p, table->indexes[x].list_max);

        /* B-tree cutoff; more than this number and index becomes a B-tree */
        UINT16DECODE(p, table->indexes[x].btree_min);

        /* Number of messages shared */
        UINT16DECODE(p, table->indexes[x].num_messages);

        /* Address of the actual index */
        H5F_addr_decode(udata->f, &p, &(table->indexes[x].index_addr));

        /* Address of the index's heap */
        H5F_addr_decode(udata->f, &p, &(table->indexes[x].heap_addr));

        /* Compute the size of a list index for this SOHM index */
        table->indexes[x].list_size = H5SM_LIST_SIZE(udata->f, table->indexes[x].list_max);
    } /* end for */

    /* Read in checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == len);

    /* Compute checksum on entire header */
    computed_chksum = H5_checksum_metadata(image, (len - H5SM_SIZEOF_CHECKSUM), 0);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_SOHM, H5E_BADVALUE, NULL, "incorrect metadata checksum for shared message table")

    /* Set return value */
    ret_value = table;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

done:
    /* Release resources */
     if(!ret_value && table)
        (void)H5SM_table_free(table);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_table_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_table_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 20 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_image_len(const void *_thing, size_t *image_len)
{
    const H5SM_master_table_t *table = (const H5SM_master_table_t *)_thing;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_table_image_len)

    /* Check arguments */
    HDassert(table);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = table->table_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_table_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_table_serialize
 *
 * Purpose:	Serialize the data structure for writing to disk.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_serialize(const H5F_t * f, hid_t UNUSED dxlp_id, haddr_t UNUSED addr,
    size_t UNUSED len, void *image, void *_thing, unsigned *flags,
    haddr_t UNUSED *new_addr, size_t UNUSED *new_len, void UNUSED **new_image)
{
    H5SM_master_table_t *table = (H5SM_master_table_t *)_thing;
    uint8_t  *p;                 /* Pointer into raw data buffer */
    uint32_t computed_chksum;    /* Computed metadata checksum value */
    size_t   x;                  /* Counter variable */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_table_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(table);
    HDassert(H5F_addr_defined(addr));


    /* Verify that we're writing version 0 of the table; this is the only
     * version defined so far.
     */
    HDassert(f->shared->sohm_vers == HDF5_SHAREDHEADER_VERSION);

    /* Get temporary pointer to buffer for serialized table */
    p = (uint8_t *)image;

    /* Encode magic number */
    HDmemcpy(p, H5SM_TABLE_MAGIC, (size_t)H5SM_SIZEOF_MAGIC);
    p += H5SM_SIZEOF_MAGIC;

    /* Encode each index header */
    for(x = 0; x < table->num_indexes; ++x) {
        /* Version for this list. */
        *p++ = H5SM_LIST_VERSION;

        /* Is message index a list or a B-tree? */
        *p++ = table->indexes[x].index_type;

        /* Type of messages in the index */
        UINT16ENCODE(p, table->indexes[x].mesg_types);

        /* Minimum size of message to share */
        UINT32ENCODE(p, table->indexes[x].min_mesg_size);

        /* List cutoff; fewer than this number and index becomes a list */
        UINT16ENCODE(p, table->indexes[x].list_max);

        /* B-tree cutoff; more than this number and index becomes a B-tree */
        UINT16ENCODE(p, table->indexes[x].btree_min);

        /* Number of messages shared */
        UINT16ENCODE(p, table->indexes[x].num_messages);

        /* Address of the actual index */
        H5F_addr_encode(f, &p, table->indexes[x].index_addr);

        /* Address of the index's heap */
        H5F_addr_encode(f, &p, table->indexes[x].heap_addr);
    } /* end for */

    /* Compute checksum on buffer */
    computed_chksum = H5_checksum_metadata(image, (len - H5SM_SIZEOF_CHECKSUM), 0);
    UINT32ENCODE(p, computed_chksum);

    /* Reset the cache flags for this operation (metadata not resize or renamed) */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)(p - (uint8_t *)image) == len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_table_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_table_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data structure
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              Jun 03, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_table_free_icr(void *thing)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_table_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy Shared Object Header Message table */
    if(H5SM_table_free((H5SM_master_table_t *)thing) < 0)
        HGOTO_ERROR(H5E_SOHM, H5E_CANTRELEASE, FAIL, "unable to free shared message table")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_table_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_list_get_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              May 18, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_get_load_size(const void *_udata, size_t *image_len)
{
    const H5SM_list_cache_ud_t *udata = (const H5SM_list_cache_ud_t *)_udata; /* User data for callback */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_list_get_load_size)

    /* Check arguments */
    HDassert(udata);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = udata->header->list_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_list_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_deserialize
 *
 * Purpose:	Deserialize the data structure from disk.
 *
 * Return:	Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static void *
H5SM_list_deserialize(const void *image, size_t UNUSED len, void *_udata,
    hbool_t UNUSED *dirty)
{
    H5SM_list_t *list;          /* The SOHM list being read in */
    H5SM_list_cache_ud_t *udata = (H5SM_list_cache_ud_t *)_udata; /* User data for callback */
    const uint8_t *p;           /* Pointer into input buffer */
    uint32_t stored_chksum;     /* Stored metadata checksum value */
    uint32_t computed_chksum;   /* Computed metadata checksum value */
    size_t x;                   /* Counter variable for messages in list */
    H5SM_list_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_list_deserialize)

    /* Sanity check */
    HDassert(udata->header);

    /* Allocate space for the SOHM list data structure */
    if(NULL == (list = H5FL_MALLOC(H5SM_list_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&list->cache_info, 0, sizeof(H5AC_info_t));

    /* Allocate list in memory as an array*/
    if((list->messages = (H5SM_sohm_t *)H5FL_ARR_MALLOC(H5SM_sohm_t, udata->header->list_max)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "file allocation failed for SOHM list")

    list->header = udata->header;

    /* Get temporary pointer to serialized list index */
    p = (const uint8_t *)image;

    /* Check magic number */
    if(HDmemcmp(p, H5SM_LIST_MAGIC, (size_t)H5SM_SIZEOF_MAGIC))
        HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "bad SOHM list signature")
    p += H5SM_SIZEOF_MAGIC;

    /* Read messages into the list array */
    for(x = 0; x < udata->header->num_messages; x++) {
        if(H5SM_message_decode(udata->f, p, &(list->messages[x])) < 0)
            HGOTO_ERROR(H5E_SOHM, H5E_CANTLOAD, NULL, "can't decode shared message")
        p += H5SM_SOHM_ENTRY_SIZE(udata->f);
    } /* end for */

    /* Read in checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) <= len);

    /* Compute checksum on entire header */
    computed_chksum = H5_checksum_metadata(image, ((size_t)(p - (const uint8_t *)image) - H5SM_SIZEOF_CHECKSUM), 0);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_SOHM, H5E_BADVALUE, NULL, "incorrect metadata checksum for shared message list")

    /* Initialize the rest of the array */
    for(x = udata->header->num_messages; x < udata->header->list_max; x++)
        list->messages[x].location = H5SM_NO_LOC;

    /* Set return value */
    ret_value = list;

done:
    /* Release resources */
    if(!ret_value && list) {
        if(list->messages)
            list->messages = (H5SM_sohm_t *)H5FL_ARR_FREE(H5SM_sohm_t, list->messages);
        list = H5FL_FREE(H5SM_list_t, list);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_list_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 20 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_image_len(const void *_thing, size_t *image_len)
{
    const H5SM_list_t *list = (const H5SM_list_t *)_thing;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SM_list_image_len)

    /* Check arguments */
    HDassert(list);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = list->header->list_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SM_list_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5SM_list_serialize
 *
 * Purpose:	Serialize the data structure for writing to disk.
 *
 * Return:	Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:	James Laird
 *		November 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_serialize(const H5F_t * f, hid_t UNUSED dxpl_id, haddr_t UNUSED addr,
    size_t UNUSED len, void *image, void *_thing, unsigned *flags,
    haddr_t UNUSED *new_addr, size_t UNUSED *new_len, void UNUSED **new_image)
{
    H5SM_list_t *list = (H5SM_list_t *)_thing;
    uint8_t *p;                 /* Pointer into raw data buffer */
    uint32_t computed_chksum;   /* Computed metadata checksum value */
    size_t mesgs_written;       /* Number of messages written to list */
    size_t x;                   /* Local index variable */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_list_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(list);
    HDassert(list->header);

    /* Get temporary pointer to buffer for serialized list index */
    p = (uint8_t *)image;

    /* Encode magic number */
    HDmemcpy(p, H5SM_LIST_MAGIC, (size_t)H5SM_SIZEOF_MAGIC);
    p += H5SM_SIZEOF_MAGIC;

    /* Write messages from the messages array to disk */
    mesgs_written = 0;
    for(x = 0; x < list->header->list_max && mesgs_written < list->header->num_messages; x++) {
        if(list->messages[x].location != H5SM_NO_LOC) {
            if(H5SM_message_encode(f, p, &(list->messages[x])) < 0)
                HGOTO_ERROR(H5E_SOHM, H5E_CANTFLUSH, FAIL, "unable to write shared message to disk")

            p+=H5SM_SOHM_ENTRY_SIZE(f);
            ++mesgs_written;
        } /* end if */
    } /* end for */
    HDassert(mesgs_written == list->header->num_messages);

    /* Compute checksum on buffer */
    computed_chksum = H5_checksum_metadata(image, (size_t)(p - (uint8_t *)image), 0);
    UINT32ENCODE(p, computed_chksum);

    /* Reset the cache flags for this operation (metadata not resize or renamed) */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)(p - (uint8_t *)image) <= len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_list_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5SM_list_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data structure
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              Jun 03, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SM_list_free_icr(void *thing)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SM_list_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy Shared Object Header Message list */
    if(H5SM_list_free((H5SM_list_t *)thing) < 0)
        HGOTO_ERROR(H5E_SOHM, H5E_CANTRELEASE, FAIL, "unable to free shared message list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SM_list_free_icr() */
