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
 * Created:		H5FScache.c
 *			May  2 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement file free space metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5FS_PACKAGE		/*suppress error about including H5FSpkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FSpkg.h"		/* File free space			*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/

/* File free space format version #'s */
#define H5FS_HDR_VERSION        0               /* Header */
#define H5FS_SINFO_VERSION      0               /* Serialized sections */


/******************/
/* Local Typedefs */
/******************/

/* User data for skip list iterator callback for iterating over section size nodes when syncing */
typedef struct {
    H5FS_sinfo_t *sinfo;        /* Free space section info */
    uint8_t **p;                /* Pointer to address of buffer pointer to serialize with */
    unsigned sect_cnt_size;     /* # of bytes to encode section size counts in */
} H5FS_iter_ud_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Section info routines */
static herr_t H5FS_sinfo_free_sect_cb(void *item, void *key, void *op_data);
static herr_t H5FS_sinfo_free_node_cb(void *item, void *key, void *op_data);
static herr_t H5FS_sinfo_serialize_sect_cb(void *_item, void UNUSED *key, void *_udata);
static herr_t H5FS_sinfo_serialize_node_cb(void *_item, void UNUSED *key, void *_udata);

/* Metadata cache callbacks */
static H5FS_t *H5FS_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5FS_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FS_t *fspace, unsigned UNUSED * flags_ptr);
static herr_t H5FS_cache_hdr_clear(H5F_t *f, H5FS_t *fspace, hbool_t destroy);
static herr_t H5FS_cache_hdr_size(const H5F_t *f, const H5FS_t *fspace, size_t *size_ptr);
static H5FS_sinfo_t *H5FS_cache_sinfo_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5FS_cache_sinfo_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FS_sinfo_t *sinfo, unsigned UNUSED * flags_ptr);
static herr_t H5FS_cache_sinfo_clear(H5F_t *f, H5FS_sinfo_t *sinfo, hbool_t destroy);
static herr_t H5FS_cache_sinfo_size(const H5F_t *f, const H5FS_sinfo_t *sinfo, size_t *size_ptr);

/*********************/
/* Package Variables */
/*********************/

/* H5FS header inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FSPACE_HDR[1] = {{
    H5AC_FSPACE_HDR_ID,
    (H5AC_load_func_t)H5FS_cache_hdr_load,
    (H5AC_flush_func_t)H5FS_cache_hdr_flush,
    (H5AC_dest_func_t)H5FS_cache_hdr_dest,
    (H5AC_clear_func_t)H5FS_cache_hdr_clear,
    (H5AC_size_func_t)H5FS_cache_hdr_size,
}};

/* H5FS serialized sections inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_FSPACE_SINFO[1] = {{
    H5AC_FSPACE_SINFO_ID,
    (H5AC_load_func_t)H5FS_cache_sinfo_load,
    (H5AC_flush_func_t)H5FS_cache_sinfo_flush,
    (H5AC_dest_func_t)H5FS_cache_sinfo_dest,
    (H5AC_clear_func_t)H5FS_cache_sinfo_clear,
    (H5AC_size_func_t)H5FS_cache_sinfo_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage free space header data to/from disk */
H5FL_BLK_DEFINE_STATIC(header_block);

/* Declare a free list to manage free space section data to/from disk */
H5FL_BLK_DEFINE_STATIC(sect_block);



/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_load
 *
 * Purpose:	Loads a free space manager header from the disk.
 *
 * Return:	Success:	Pointer to a new free space header
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_t *
H5FS_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_fs_prot, void UNUSED *udata2)
{
    H5FS_t		*fspace = NULL; /* Free space header info */
    const H5FS_prot_t   *fs_prot = (const H5FS_prot_t *)_fs_prot;       /* User data for protecting */
    size_t		size;           /* Header size */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    unsigned            nclasses;       /* Number of section classes */
    H5FS_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_hdr_load)
#ifdef QAK
HDfprintf(stderr, "%s: Load free space header, addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(fs_prot);

    /* Allocate a new free space manager */
    if(NULL == (fspace = H5FS_new(fs_prot->nclasses, fs_prot->classes, fs_prot->cls_init_udata)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set free space manager's internal information */
    fspace->addr = addr;

    /* Compute the size of the free space header on disk */
    size = H5FS_HEADER_SIZE(f);

    /* Allocate temporary buffer */
    if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_FSPACE_HDR, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_READERROR, NULL, "can't read free space header")

    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5FS_HDR_MAGIC, (size_t)H5FS_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "wrong free space header signature")
    p += H5FS_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5FS_HDR_VERSION)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "wrong free space header version")

    /* Client ID */
    fspace->client = *p++;
    if(fspace->client >= H5FS_NUM_CLIENT_ID)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "unknown client ID in free space header")

    /* Total space tracked */
    H5F_DECODE_LENGTH(f, p, fspace->tot_space);

    /* Total # of free space sections tracked */
    H5F_DECODE_LENGTH(f, p, fspace->tot_sect_count);

    /* # of serializable free space sections tracked */
    H5F_DECODE_LENGTH(f, p, fspace->serial_sect_count);

    /* # of ghost free space sections tracked */
    H5F_DECODE_LENGTH(f, p, fspace->ghost_sect_count);

    /* # of section classes */
    /* (only check if we actually have some classes) */
    UINT16DECODE(p, nclasses);
    if(fspace->nclasses > 0 && fspace->nclasses != nclasses)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "section class count mismatch")

    /* Shrink percent */
    UINT16DECODE(p, fspace->shrink_percent);

    /* Expand percent */
    UINT16DECODE(p, fspace->expand_percent);

    /* Size of address space free space sections are within (log2 of actual value) */
    UINT16DECODE(p, fspace->max_sect_addr);

    /* Max. size of section to track */
    H5F_DECODE_LENGTH(f, p, fspace->max_sect_size);

    /* Address of serialized free space sections */
    H5F_addr_decode(f, &p, &fspace->sect_addr);

    /* Size of serialized free space sections */
    H5F_DECODE_LENGTH(f, p, fspace->sect_size);

    /* Allocated size of serialized free space sections */
    H5F_DECODE_LENGTH(f, p, fspace->alloc_sect_size);

    /* Compute checksum on indirect block */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    HDassert((size_t)(p - buf) == size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap indirect block")

    /* Set return value */
    ret_value = fspace;

done:
    if(buf)
        H5FL_BLK_FREE(header_block, buf);
    if(!ret_value && fspace)
        (void)H5FS_cache_hdr_dest(f, fspace);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_hdr_load() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_flush
 *
 * Purpose:	Flushes a dirty free space header to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FS_t *fspace, unsigned UNUSED * flags_ptr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_hdr_flush)
#ifdef QAK
HDfprintf(stderr, "%s: Flushing free space header, addr = %a, destroy = %u\n", FUNC, addr, (unsigned)destroy);
#endif /* QAK */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(fspace);

    if(fspace->cache_info.is_dirty) {
        uint8_t	*buf = NULL;        /* Temporary raw data buffer */
        uint8_t *p;                 /* Pointer into raw data buffer */
        uint32_t metadata_chksum;       /* Computed metadata checksum value */
        size_t	size;               /* Header size on disk */

        /* Compute the size of the free space header on disk */
        size = H5FS_HEADER_SIZE(f);

        /* Allocate temporary buffer */
        if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* Magic number */
        HDmemcpy(p, H5FS_HDR_MAGIC, (size_t)H5FS_SIZEOF_MAGIC);
        p += H5FS_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5FS_HDR_VERSION;

        /* Client ID */
        *p++ = fspace->client;

        /* Total space tracked */
        H5F_ENCODE_LENGTH(f, p, fspace->tot_space);

        /* Total # of free space sections tracked */
        H5F_ENCODE_LENGTH(f, p, fspace->tot_sect_count);

        /* # of serializable free space sections tracked */
        H5F_ENCODE_LENGTH(f, p, fspace->serial_sect_count);

        /* # of ghost free space sections tracked */
        H5F_ENCODE_LENGTH(f, p, fspace->ghost_sect_count);

        /* # of section classes */
        UINT16ENCODE(p, fspace->nclasses);

        /* Shrink percent */
        UINT16ENCODE(p, fspace->shrink_percent);

        /* Expand percent */
        UINT16ENCODE(p, fspace->expand_percent);

        /* Size of address space free space sections are within (log2 of actual value) */
        UINT16ENCODE(p, fspace->max_sect_addr);

        /* Max. size of section to track */
        H5F_ENCODE_LENGTH(f, p, fspace->max_sect_size);

        /* Address of serialized free space sections */
        H5F_addr_encode(f, &p, fspace->sect_addr);

        /* Size of serialized free space sections */
        H5F_ENCODE_LENGTH(f, p, fspace->sect_size);

        /* Allocated size of serialized free space sections */
        H5F_ENCODE_LENGTH(f, p, fspace->alloc_sect_size);

        /* Compute checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

	/* Write the free space header. */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_FSPACE_HDR, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFLUSH, FAIL, "unable to save free space header to disk")

        H5FL_BLK_FREE(header_block, buf);

	fspace->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5FS_cache_hdr_dest(f, fspace) < 0)
	    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to destroy free space header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_cache_hdr_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_dest
 *
 * Purpose:	Destroys a free space header in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5FS_cache_hdr_dest(H5F_t UNUSED *f, H5FS_t *fspace)
{
    unsigned u;                 /* Local index variable */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_hdr_dest)

    /*
     * Check arguments.
     */
    HDassert(fspace);

    /* Terminate the section classes for this free space list */
    for(u = 0; u < fspace->nclasses ; u++) {
        /* Call the class termination routine, if there is one */
        if(fspace->sect_cls[u].term_cls)
            if((fspace->sect_cls[u].term_cls)(&fspace->sect_cls[u]) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "unable to finalize section class")
    } /* end for */

    /* Release the memory for the free space section classes */
    if(fspace->sect_cls)
        fspace->sect_cls = H5FL_SEQ_FREE(H5FS_section_class_t, fspace->sect_cls);

    /* Free free space info */
    H5FL_FREE(H5FS_t, fspace);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_hdr_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_clear
 *
 * Purpose:	Mark a free space header in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_hdr_clear(H5F_t *f, H5FS_t *fspace, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_hdr_clear)

    /*
     * Check arguments.
     */
    HDassert(fspace);

    /* Reset the dirty flag.  */
    fspace->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5FS_cache_hdr_dest(f, fspace) < 0)
	    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to destroy free space header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_hdr_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_size
 *
 * Purpose:	Compute the size in bytes of a free space header
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_hdr_size(const H5F_t *f, const H5FS_t UNUSED *fspace, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_cache_hdr_size)

    /* check arguments */
    HDassert(f);
    HDassert(fspace);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5FS_HEADER_SIZE(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_cache_hdr_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_sinfo_load
 *
 * Purpose:	Loads free space sections from the disk.
 *
 * Return:	Success:	Pointer to a new free space section info
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 31 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_sinfo_t *
H5FS_cache_sinfo_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void *_fspace)
{
    H5FS_sinfo_t	*sinfo = NULL;  /* Free space section info */
    H5FS_t              *fspace = (H5FS_t *)_fspace;       /* User data for protecting */
    haddr_t             fs_addr;        /* Free space header address */
    size_t              old_sect_size;  /* Old section size */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    H5FS_sinfo_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_sinfo_load)
#ifdef QAK
HDfprintf(stderr, "%s: Load free space sections, addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(fspace);

    /* Allocate a new free space section info */
    if(NULL == (sinfo = H5FS_sinfo_new(f, fspace)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    
    /* Link free space manager to section info */
    /* (for deserializing sections) */
    HDassert(fspace->sinfo == NULL);
    fspace->sinfo = sinfo;

    /* Sanity check address */
    if(H5F_addr_ne(addr, fspace->sect_addr))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "incorrect address for free space sections")

    /* Allocate space for the buffer to serialize the sections into */
    old_sect_size = fspace->sect_size;
#ifdef QAK
HDfprintf(stderr, "%s: fspace->sect_size = %Hu\n", FUNC, fspace->sect_size);
#endif /* QAK */
    if(NULL == (buf = H5FL_BLK_MALLOC(sect_block, (size_t)fspace->sect_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read buffer from disk */
    if(H5F_block_read(f, H5FD_MEM_FSPACE_SINFO, fspace->sect_addr, (size_t)fspace->sect_size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_READERROR, NULL, "can't read free space sections")

    /* Deserialize free sections from buffer available */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5FS_SINFO_MAGIC, (size_t)H5FS_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "wrong free space sections signature")
    p += H5FS_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5FS_SINFO_VERSION)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "wrong free space sections version")

    /* Address of free space header for these sections */
    H5F_addr_decode(f, &p, &fs_addr);
#ifdef QAK
HDfprintf(stderr, "%s: fspace->addr = %a, fs_addr = %a\n", FUNC, fspace->addr, fs_addr);
#endif /* QAK */
    if(H5F_addr_ne(fs_addr, fspace->addr))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "incorrect header address for free space sections")

    /* Check for any serialized sections */
    if(fspace->serial_sect_count > 0) {
        hsize_t old_tot_sect_count;     /* Total section count from header */
        hsize_t old_serial_sect_count;  /* Total serializable section count from header */
        hsize_t old_ghost_sect_count;   /* Total ghost section count from header */
        hsize_t old_tot_space;          /* Total space managed from header */
        unsigned sect_cnt_size;         /* The size of the section size counts */

        /* Compute the size of the section counts */
        sect_cnt_size = MAX(1, (H5V_log2_gen(fspace->serial_sect_count) + 7) / 8);
#ifdef QAK
HDfprintf(stderr, "%s: sect_cnt_size = %u\n", FUNC, sect_cnt_size);
HDfprintf(stderr, "%s: fspace->sect_len_size = %u\n", FUNC, fspace->sect_len_size);
#endif /* QAK */

        /* Reset the section count, the "add" routine will update it */
        old_tot_sect_count = fspace->tot_sect_count;
        old_serial_sect_count = fspace->serial_sect_count;
        old_ghost_sect_count = fspace->ghost_sect_count;
        old_tot_space = fspace->tot_space;
#ifdef QAK
HDfprintf(stderr, "%s: fspace->tot_sect_count = %Hu\n", FUNC, fspace->tot_sect_count);
HDfprintf(stderr, "%s: fspace->serial_sect_count = %Hu\n", FUNC, fspace->serial_sect_count);
HDfprintf(stderr, "%s: fspace->ghost_sect_count = %Hu\n", FUNC, fspace->ghost_sect_count);
HDfprintf(stderr, "%s: fspace->tot_space = %Hu\n", FUNC, fspace->tot_space);
#endif /* QAK */
        fspace->tot_sect_count = 0;
        fspace->serial_sect_count = 0;
        fspace->ghost_sect_count = 0;
        fspace->tot_space = 0;

        /* Walk through the buffer, deserializing sections */
        do {
            hsize_t sect_size;      /* Current section size */
            size_t node_count;      /* # of sections of this size */
            size_t u;               /* Local index variable */

            /* The number of sections of this node's size */
            UINT64DECODE_VAR(p, node_count, sect_cnt_size);
#ifdef QAK
HDfprintf(stderr, "%s: node_count = %Zu\n", FUNC, node_count);
#endif /* QAK */
            HDassert(node_count);

            /* The size of the sections for this node */
            UINT64DECODE_VAR(p, sect_size, sinfo->sect_len_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect_size = %Hu\n", FUNC, sect_size);
#endif /* QAK */
            HDassert(sect_size);

            /* Loop over nodes of this size */
            for(u = 0; u < node_count; u++) {
                H5FS_section_info_t *new_sect;  /* Section that was deserialized */
                haddr_t sect_addr;      /* Address of free space section in the address space */
                unsigned sect_type;     /* Type of free space section */
                unsigned des_flags;     /* Flags from deserialize callback */

                /* The address of the section */
                UINT64DECODE_VAR(p, sect_addr, sinfo->sect_off_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect_addr = %a\n", FUNC, sect_addr);
#endif /* QAK */

                /* The type of this section */
                sect_type = *p++;
#ifdef QAK
HDfprintf(stderr, "%s: sect_type = %u\n", FUNC, sect_type);
#endif /* QAK */

                /* Call 'deserialize' callback for this section */
                des_flags = 0;
                HDassert(fspace->sect_cls[sect_type].deserialize);
                if(NULL == (new_sect = (*fspace->sect_cls[sect_type].deserialize)(&fspace->sect_cls[sect_type], dxpl_id, p, sect_addr, sect_size, &des_flags)))
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTDECODE, NULL, "can't deserialize section")

                /* Update offset in serialization buffer */
                p += fspace->sect_cls[sect_type].serial_size;
#ifdef QAK
HDfprintf(stderr, "%s: fspace->sect_cls[%u].serial_size = %Zu\n", FUNC, sect_type, fspace->sect_cls[sect_type].serial_size);
#endif /* QAK */

                /* Insert section in free space manager, unless requested not to */
                if(!(des_flags & H5FS_DESERIALIZE_NO_ADD))
                    if(H5FS_sect_add(f, dxpl_id, fspace, new_sect, H5FS_ADD_DESERIALIZING, NULL) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, NULL, "can't add section to free space manager")
            } /* end for */
        } while(p < ((buf + old_sect_size) - H5FS_SIZEOF_CHKSUM));

        /* Sanity check */
        HDassert((size_t)(p - buf) == (old_sect_size - H5FS_SIZEOF_CHKSUM));
        HDassert(old_sect_size == fspace->sect_size);
        HDassert(old_tot_sect_count == fspace->tot_sect_count);
        HDassert(old_serial_sect_count == fspace->serial_sect_count);
        HDassert(old_ghost_sect_count == fspace->ghost_sect_count);
        HDassert(old_tot_space == fspace->tot_space);
    } /* end if */

    /* Compute checksum on indirect block */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == old_sect_size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap indirect block")

    /* Set return value */
    ret_value = sinfo;

done:
    if(buf)
        H5FL_BLK_FREE(sect_block, buf);
    if(!ret_value && sinfo)
        (void)H5FS_cache_sinfo_dest(f, sinfo);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_sinfo_load() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sinfo_serialize_sect_cb
 *
 * Purpose:	Skip list iterator callback to serialize free space sections
 *              of a particular size
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sinfo_serialize_sect_cb(void *_item, void UNUSED *key, void *_udata)
{
    H5FS_section_class_t *sect_cls;     /* Class of section */
    H5FS_section_info_t *sect= (H5FS_section_info_t *)_item;   /* Free space section to work on */
    H5FS_iter_ud_t *udata = (H5FS_iter_ud_t *)_udata; /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sinfo_serialize_sect_cb)

    /* Check arguments. */
    HDassert(sect);
    HDassert(udata->sinfo);
    HDassert(udata->p);

    /* Get section's class */
    sect_cls = &udata->sinfo->fspace->sect_cls[sect->type];

    /* Check if this section should be serialized (i.e. is not a ghost section) */
    if(!(sect_cls->flags & H5FS_CLS_GHOST_OBJ)) {
        /* The address of the section */
        UINT64ENCODE_VAR(*udata->p, sect->addr, udata->sinfo->sect_off_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect->addr = %a\n", FUNC, sect->addr);
#endif /* QAK */

        /* The type of this section */
        *(*udata->p)++ = (uint8_t)sect->type;
#ifdef QAK
HDfprintf(stderr, "%s: sect->type = %u\n", FUNC, (unsigned)sect->type);
#endif /* QAK */

        /* Call 'serialize' callback for this section */
        if(sect_cls->serialize) {
            if((*sect_cls->serialize)(sect_cls, sect, *udata->p) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTSERIALIZE, FAIL, "can't syncronize section")

            /* Update offset in serialization buffer */
            (*udata->p) += sect_cls->serial_size;
        } /* end if */
        else
            HDassert(sect_cls->serial_size == 0);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sinfo_serialize_sect_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sinfo_serialize_node_cb
 *
 * Purpose:	Skip list iterator callback to serialize free space sections
 *              in a bin
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sinfo_serialize_node_cb(void *_item, void UNUSED *key, void *_udata)
{
    H5FS_node_t *fspace_node = (H5FS_node_t *)_item;   /* Free space size node to work on */
    H5FS_iter_ud_t *udata = (H5FS_iter_ud_t *)_udata; /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sinfo_serialize_node_cb)

    /* Check arguments. */
    HDassert(fspace_node);
    HDassert(udata->sinfo);
    HDassert(udata->p);

    /* Check if this node has any serializable sections */
    if(fspace_node->serial_count > 0) {
        /* The number of serializable sections of this node's size */
        UINT64ENCODE_VAR(*udata->p, fspace_node->serial_count, udata->sect_cnt_size);
#ifdef QAK
HDfprintf(stderr, "%s: fspace_node->serial_count = %Zu\n", FUNC, fspace_node->serial_count);
#endif /* QAK */

        /* The size of the sections for this node */
        UINT64ENCODE_VAR(*udata->p, fspace_node->sect_size, udata->sinfo->sect_len_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect_size = %Hu\n", FUNC, fspace_node->sect_size);
#endif /* QAK */

        /* Iterate through all the sections of this size */
        HDassert(fspace_node->sect_list);
        if(H5SL_iterate(fspace_node->sect_list, H5FS_sinfo_serialize_sect_cb, udata) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section nodes")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sinfo_serialize_node_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_sinfo_flush
 *
 * Purpose:	Flushes a dirty free space section info to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 31 2006
 *
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_sinfo_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FS_sinfo_t *sinfo, unsigned UNUSED * flags_ptr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_sinfo_flush)
#ifdef QAK
HDfprintf(stderr, "%s: Flushing free space header, addr = %a, destroy = %u\n", FUNC, addr, (unsigned)destroy);
#endif /* QAK */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(sinfo);

    if(sinfo->cache_info.is_dirty) {
        H5FS_iter_ud_t udata;       /* User data for callbacks */
        uint8_t	*buf = NULL;        /* Temporary raw data buffer */
        uint8_t *p;                 /* Pointer into raw data buffer */
        uint32_t metadata_chksum;   /* Computed metadata checksum value */
        unsigned bin;               /* Current bin we are on */

        /* Sanity check address */
        if(H5F_addr_ne(addr, sinfo->fspace->sect_addr))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, FAIL, "incorrect address for free space sections")

        /* Allocate temporary buffer */
        if((buf = H5FL_BLK_MALLOC(sect_block, (size_t)sinfo->fspace->sect_size)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* Magic number */
        HDmemcpy(p, H5FS_SINFO_MAGIC, (size_t)H5FS_SIZEOF_MAGIC);
        p += H5FS_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5FS_SINFO_VERSION;

        /* Address of free space header for these sections */
#ifdef QAK
HDfprintf(stderr, "%s: sinfo->fspace->addr = %a\n", FUNC, sinfo->fspace->addr);
#endif /* QAK */
        H5F_addr_encode(f, &p, sinfo->fspace->addr);

        /* Set up user data for iterator */
        udata.sinfo = sinfo;
        udata.p = &p;
        udata.sect_cnt_size = MAX(1, (H5V_log2_gen(sinfo->fspace->serial_sect_count) + 7) / 8);
#ifdef QAK
HDfprintf(stderr, "%s: udata.sect_cnt_size = %u\n", FUNC, udata.sect_cnt_size);
#endif /* QAK */

        /* Iterate over all the bins */
#ifdef QAK
HDfprintf(stderr, "%s: Serializing section bins\n", FUNC);
#endif /* QAK */
        for(bin = 0; bin < sinfo->nbins; bin++) {
            /* Check if there are any sections in this bin */
            if(sinfo->bins[bin].bin_list) {
                /* Iterate over list of section size nodes for bin */
                if(H5SL_iterate(sinfo->bins[bin].bin_list, H5FS_sinfo_serialize_node_cb, &udata) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section size nodes")
            } /* end if */
        } /* end for */

        /* Compute checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

        /* Sanity check */
        HDassert((size_t)(p - buf) == sinfo->fspace->sect_size);
        HDassert(sinfo->fspace->sect_size <= sinfo->fspace->alloc_sect_size);
#ifdef QAK
HDfprintf(stderr, "%s: sinfo->fspace->sect_size = %Hu\n", FUNC, sinfo->fspace->sect_size);
HDfprintf(stderr, "%s: sinfo->fspace->alloc_sect_size = %Hu\n", FUNC, sinfo->fspace->alloc_sect_size);
#endif /* QAK */

        /* Write buffer to disk */
        if(H5F_block_write(f, H5FD_MEM_FSPACE_SINFO, sinfo->fspace->sect_addr, (size_t)sinfo->fspace->sect_size, dxpl_id, buf) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTFLUSH, FAIL, "unable to save free space sections to disk")

        H5FL_BLK_FREE(sect_block, buf);

	sinfo->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5FS_cache_sinfo_dest(f, sinfo) < 0)
	    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to destroy free space section info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_cache_sinfo_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sinfo_free_sect_cb
 *
 * Purpose:	Free a size-tracking node for a bin
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sinfo_free_sect_cb(void *_sect, void UNUSED *key, void *op_data)
{
    H5FS_section_info_t *sect = (H5FS_section_info_t *)_sect;   /* Section to free */
    const H5FS_sinfo_t *sinfo = (const H5FS_sinfo_t *)op_data;     /* Free space manager for section */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_sinfo_free_sect_cb)

    HDassert(sect);
    HDassert(sinfo);

    /* Call the section's class 'free' method on the section */
    (*sinfo->fspace->sect_cls[sect->type].free)(sect);

    FUNC_LEAVE_NOAPI(0)
}   /* H5FS_sinfo_free_sect_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sinfo_free_node_cb
 *
 * Purpose:	Free a size-tracking node for a bin
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sinfo_free_node_cb(void *item, void UNUSED *key, void *op_data)
{
    H5FS_node_t *fspace_node = (H5FS_node_t *)item;       /* Temporary pointer to free space list node */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_sinfo_free_node_cb)

    HDassert(fspace_node);
    HDassert(op_data);

    /* Release the skip list for sections of this size */
    H5SL_destroy(fspace_node->sect_list, H5FS_sinfo_free_sect_cb, op_data);

    /* Release free space list node */
    H5FL_FREE(H5FS_node_t, fspace_node);

    FUNC_LEAVE_NOAPI(0)
}   /* H5FS_sinfo_free_node_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_sinfo_dest
 *
 * Purpose:	Destroys a free space section info in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 31 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5FS_cache_sinfo_dest(H5F_t *f, H5FS_sinfo_t *sinfo)
{
    unsigned u;                 /* Local index variable */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_sinfo_dest)

    /*
     * Check arguments.
     */
    HDassert(sinfo);
    HDassert(sinfo->fspace);
    HDassert(sinfo->bins);

    /* Clear out lists of nodes */
    for(u = 0; u < sinfo->nbins; u++)
        if(sinfo->bins[u].bin_list) {
            H5SL_destroy(sinfo->bins[u].bin_list, H5FS_sinfo_free_node_cb, sinfo);
            sinfo->bins[u].bin_list = NULL;
        } /* end if */

    /* Release bins for skip lists */
    sinfo->bins = H5FL_SEQ_FREE(H5FS_bin_t, sinfo->bins);

    /* Release skip list for merging sections */
    if(sinfo->merge_list)
        if(H5SL_close(sinfo->merge_list) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCLOSEOBJ, FAIL, "can't destroy section merging skip list")

    /* Unpin the free space header in the cache */
    /* (make certain this is last action with section info, to allow for header
     *  disappearing immediately)
     */
    if(H5AC_unpin_entry(f, sinfo->fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPIN, FAIL, "unable to unpin free space header")

    /* Release free space section info */
    H5FL_FREE(H5FS_sinfo_t, sinfo);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_sinfo_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_sinfo_clear
 *
 * Purpose:	Mark a free space section info in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 31 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_sinfo_clear(H5F_t *f, H5FS_sinfo_t *sinfo, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_sinfo_clear)

    /*
     * Check arguments.
     */
    HDassert(sinfo);

    /* Reset the dirty flag.  */
    sinfo->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5FS_cache_sinfo_dest(f, sinfo) < 0)
	    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to destroy free space section info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_sinfo_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_sinfo_size
 *
 * Purpose:	Compute the size in bytes of a free space section info
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 31 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_sinfo_size(const H5F_t UNUSED *f, const H5FS_sinfo_t *sinfo, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_cache_sinfo_size)

    /* check arguments */
    HDassert(sinfo);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = sinfo->fspace->sect_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_cache_sinfo_size() */

