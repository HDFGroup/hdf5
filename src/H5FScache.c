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
#include "H5WBprivate.h"        /* Wrapped Buffers                      */
#include "H5AC2private.h"       /* Metadata cache                       */

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
static herr_t H5FS_sinfo_serialize_sect_cb(void *_item, void UNUSED *key, void *_udata);
static herr_t H5FS_sinfo_serialize_node_cb(void *_item, void UNUSED *key, void *_udata);

/* Metadata cache callbacks */
static void *H5FS_cache_hdr_deserialize(haddr_t addr, size_t len,
    const void *image, void *udata, hbool_t *dirty);
static herr_t H5FS_cache_hdr_serialize(const H5F_t *f, hid_t dxpl_id, 
    haddr_t addr, size_t len, void *image, void *thing, unsigned *flags, 
    haddr_t *new_addr, size_t *new_len, void **new_image);
static herr_t H5FS_cache_hdr_free_icr(haddr_t addr, size_t len, void *thing);

static void *H5FS_cache_sinfo_deserialize(haddr_t addr, size_t len,
    const void *image, void *udata, hbool_t *dirty);
static herr_t H5FS_cache_sinfo_serialize(const H5F_t *f, hid_t dxpl_id,
    haddr_t addr, size_t len, void *image, void *thing, unsigned *flags, 
    haddr_t *new_addr, size_t *new_len, void **new_image);
static herr_t H5FS_cache_sinfo_free_icr(haddr_t addr, size_t len, void *thing);

/*********************/
/* Package Variables */
/*********************/

/* H5FS header inherits cache-like properties from H5AC2 */
const H5AC2_class_t H5AC2_FSPACE_HDR[1] = {{
    H5AC2_FSPACE_HDR_ID,
    "Free space header",
    H5FD_MEM_FSPACE_HDR,
    H5FS_cache_hdr_deserialize,
    NULL, /* H5FS_cache_hdr_image_len, */
    H5FS_cache_hdr_serialize,
    H5FS_cache_hdr_free_icr,
    NULL, /* H5FS_cache_hdr_clear_dirty_bits, */
}};

/* H5FS serialized sections inherit cache-like properties from H5AC2 */
const H5AC2_class_t H5AC2_FSPACE_SINFO[1] = {{
    H5AC2_FSPACE_SINFO_ID,
    "Free space section info",
    H5FD_MEM_FSPACE_SINFO,
    H5FS_cache_sinfo_deserialize,
    NULL,
    H5FS_cache_sinfo_serialize,
    H5FS_cache_sinfo_free_icr,
    NULL,
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5FS_cache_hdr_deserialize
 *
 * Purpose:     Deserialize the data structure from disk.
 *
 * Return:      Success:        Pointer to a new free space header
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              May  2 2006
 *
 * Changes:     Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              May 28 2008
 *              Converted from H5FS_cache_hdr_load
 *
 *-------------------------------------------------------------------------
 */

static void *
H5FS_cache_hdr_deserialize(haddr_t UNUSED addr, size_t UNUSED len, 
    const void *image, void *_udata, hbool_t UNUSED *dirty)
{
    H5FS_t		*fspace = NULL; /* Free space header info */
    H5FS_hdr_cache_ud_t *udata = (H5FS_hdr_cache_ud_t *)_udata; /* user data for callback */
    size_t		size;           /* Header size */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    unsigned            nclasses;       /* Number of section classes */
    H5FS_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_hdr_deserialize)
#ifdef QAK
HDfprintf(stderr, "%s: Deserialize free space header, addr = %a\n", FUNC, addr);
#endif

    /* Check arguments */
    HDassert(image);
    HDassert(udata);

    /* Allocate a new free space manager */
    if(NULL == (fspace = H5FS_new(udata->fs_prot->nclasses, udata->fs_prot->classes, udata->fs_prot->cls_init_udata)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set free space manager's internal information */
    fspace->addr = addr;

    /* Compute the size of the free space header on disk */
    size = H5FS_HEADER_SIZE(udata->f);

    p = image;

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
    H5F_DECODE_LENGTH(udata->f, p, fspace->tot_space);

    /* Total # of free space sections tracked */
    H5F_DECODE_LENGTH(udata->f, p, fspace->tot_sect_count);

    /* # of serializable free space sections tracked */
    H5F_DECODE_LENGTH(udata->f, p, fspace->serial_sect_count);

    /* # of ghost free space sections tracked */
    H5F_DECODE_LENGTH(udata->f, p, fspace->ghost_sect_count);

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
    H5F_DECODE_LENGTH(udata->f, p, fspace->max_sect_size);

    /* Address of serialized free space sections */
    H5F_addr_decode(udata->f, &p, &fspace->sect_addr);

    /* Size of serialized free space sections */
    H5F_DECODE_LENGTH(udata->f, p, fspace->sect_size);

    /* Allocated size of serialized free space sections */
    H5F_DECODE_LENGTH(udata->f, p, fspace->alloc_sect_size);

    /* Compute checksum on indirect block */
    computed_chksum = H5_checksum_metadata(image, (size_t)((const uint8_t *)p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap indirect block")

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Set return value */
    ret_value = fspace;

done:
    /* Release resources */
    if(!ret_value && fspace)
        (void)H5FS_cache_hdr_dest(fspace);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_hdr_deserialize() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:    H5FS_cache_hdr_serialize
 *
 * Purpose:     Serializes the data structure for writing to disk.
 *
 * Return:      Success:         SUCCEED
 *              Failure:         FAIL
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              May  2 2006
 *
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *              Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              May 28, 2008
 *              Converted from H5FS_cache_hdr_flush
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_hdr_serialize(const H5F_t *f, hid_t UNUSED dxpl_id, 
    haddr_t UNUSED addr, size_t UNUSED len, void *image, void *_thing, 
    unsigned *flags, haddr_t UNUSED *new_addr, size_t UNUSED *new_len, 
    void UNUSED **new_image)
{
    H5FS_t *fspace = (H5FS_t *)_thing;    /* Pointer to free space header */
    uint8_t *p;                           /* Pointer into raw data buffer */
    uint32_t metadata_chksum;             /* Computed metadata checksum value */
    size_t	size;                     /* Header size on disk */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_cache_hdr_serialize)
#ifdef QAK
HDfprintf(stderr, "%s: Serialize free space header, addr = %a\n", FUNC, addr);
#endif

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(fspace);
    HDassert(flags);

    /* Compute the size of the free space header on disk */
    size = H5FS_HEADER_SIZE(f);

    /* Get temporary pointer to header */
    p = image;

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
    metadata_chksum = H5_checksum_metadata(image, (size_t)((const uint8_t *)p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Reset the cache flags for this operation */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_cache_hdr_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_cache_hdr_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data structure
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              May 29, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_hdr_free_icr(haddr_t UNUSED addr, size_t UNUSED len, void *thing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_cache_hdr_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy free space header */
    H5FS_cache_hdr_dest(thing);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_cache_hdr_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_cache_sinfo_deserialize
 *
 * Purpose:     Deserialize the data structure from disk.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              July 31 2006
 *
 * Modified:    Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              May 29, 2008
 *              Converted from H5FS_cache_sinfo_load
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FS_cache_sinfo_deserialize(haddr_t UNUSED addr, size_t UNUSED len, 
    const void *image, void *_udata, hbool_t UNUSED *dirty)
{
    H5FS_sinfo_t	*sinfo = NULL;  /* Free space section info */
    H5FS_sinfo_cache_ud_t *udata = (H5FS_sinfo_cache_ud_t *)_udata; /* user data for callback */
    haddr_t             fs_addr;        /* Free space header address */
    size_t              old_sect_size;  /* Old section size */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    H5FS_sinfo_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_sinfo_deserialize)
#ifdef QAK
HDfprintf(stderr, "%s: Deserialize free space sections, addr = %a\n", FUNC, addr);
#endif

    /* Check arguments */
    HDassert(image);
    HDassert(udata);

    /* Allocate a new free space section info */
    if(NULL == (sinfo = H5FS_sinfo_new(udata->f, udata->fspace)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    
    /* Link free space manager to section info */
    /* (for deserializing sections) */
    HDassert(udata->fspace->sinfo == NULL);
    udata->fspace->sinfo = sinfo;

    /* Sanity check address */
    if(H5F_addr_ne(addr, udata->fspace->sect_addr))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "incorrect address for free space sections")

    /* Allocate space for the buffer to serialize the sections into */
    H5_ASSIGN_OVERFLOW(/* To: */ old_sect_size, /* From: */ udata->fspace->sect_size, /* From: */ hsize_t, /* To: */ size_t);
#ifdef QAK
HDfprintf(stderr, "%s: udata->fspace->sect_size = %Hu\n", FUNC, udata->fspace->sect_size);
#endif /* QAK */

    /* Deserialize free sections from buffer available */
    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5FS_SINFO_MAGIC, (size_t)H5FS_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "wrong free space sections signature")
    p += H5FS_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5FS_SINFO_VERSION)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "wrong free space sections version")

    /* Address of free space header for these sections */
    H5F_addr_decode(udata->f, &p, &fs_addr);
#ifdef QAK
HDfprintf(stderr, "%s: udata->fspace->addr = %a, fs_addr = %a\n", FUNC, udata->fspace->addr, fs_addr);
#endif /* QAK */
    if(H5F_addr_ne(fs_addr, udata->fspace->addr))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "incorrect header address for free space sections")

    /* Check for any serialized sections */
    if(udata->fspace->serial_sect_count > 0) {
        hsize_t old_tot_sect_count;     /* Total section count from header */
        hsize_t old_serial_sect_count;  /* Total serializable section count from header */
        hsize_t old_ghost_sect_count;   /* Total ghost section count from header */
        hsize_t old_tot_space;          /* Total space managed from header */
        unsigned sect_cnt_size;         /* The size of the section size counts */

        /* Compute the size of the section counts */
        sect_cnt_size = H5V_limit_enc_size((uint64_t)udata->fspace->serial_sect_count);
#ifdef QAK
HDfprintf(stderr, "%s: sect_cnt_size = %u\n", FUNC, sect_cnt_size);
HDfprintf(stderr, "%s: udata->fspace->sect_len_size = %u\n", FUNC, udata->fspace->sect_len_size);
#endif /* QAK */

        /* Reset the section count, the "add" routine will update it */
        old_tot_sect_count = udata->fspace->tot_sect_count;
        old_serial_sect_count = udata->fspace->serial_sect_count;
        old_ghost_sect_count = udata->fspace->ghost_sect_count;
        old_tot_space = udata->fspace->tot_space;
#ifdef QAK
HDfprintf(stderr, "%s: udata->fspace->tot_sect_count = %Hu\n", FUNC, udata->fspace->tot_sect_count);
HDfprintf(stderr, "%s: udata->fspace->serial_sect_count = %Hu\n", FUNC, udata->fspace->serial_sect_count);
HDfprintf(stderr, "%s: udata->fspace->ghost_sect_count = %Hu\n", FUNC, udata->fspace->ghost_sect_count);
HDfprintf(stderr, "%s: udata->fspace->tot_space = %Hu\n", FUNC, udata->fspace->tot_space);
#endif /* QAK */
        udata->fspace->tot_sect_count = 0;
        udata->fspace->serial_sect_count = 0;
        udata->fspace->ghost_sect_count = 0;
        udata->fspace->tot_space = 0;

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
                HDassert(udata->fspace->sect_cls[sect_type].deserialize);
                if(NULL == (new_sect = (*udata->fspace->sect_cls[sect_type].deserialize)(&udata->fspace->sect_cls[sect_type], udata->dxpl_id, p, sect_addr, sect_size, &des_flags)))
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTDECODE, NULL, "can't deserialize section")

                /* Update offset in serialization buffer */
                p += udata->fspace->sect_cls[sect_type].serial_size;
#ifdef QAK
HDfprintf(stderr, "%s: udata->fspace->sect_cls[%u].serial_size = %Zu\n", FUNC, sect_type, udata->fspace->sect_cls[sect_type].serial_size);
#endif /* QAK */

                /* Insert section in free space manager, unless requested not to */
                if(!(des_flags & H5FS_DESERIALIZE_NO_ADD))
                    if(H5FS_sect_add(udata->f, udata->dxpl_id, udata->fspace, new_sect, H5FS_ADD_DESERIALIZING, NULL) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, NULL, "can't add section to free space manager")
            } /* end for */
        } while(p < (((const uint8_t *)image + old_sect_size) - H5FS_SIZEOF_CHKSUM));

        /* Sanity check */
        HDassert((size_t)(p - (const uint8_t *)image) == (old_sect_size - H5FS_SIZEOF_CHKSUM));
        HDassert(old_sect_size == udata->fspace->sect_size);
        HDassert(old_tot_sect_count == udata->fspace->tot_sect_count);
        HDassert(old_serial_sect_count == udata->fspace->serial_sect_count);
        HDassert(old_ghost_sect_count == udata->fspace->ghost_sect_count);
        HDassert(old_tot_space == udata->fspace->tot_space);
    } /* end if */

    /* Compute checksum on indirect block */
    computed_chksum = H5_checksum_metadata(image, (size_t)((const uint8_t *)p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap indirect block")

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Set return value */
    ret_value = sinfo;

done:
    if(!ret_value && sinfo)
        (void)H5FS_cache_sinfo_dest(sinfo);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_sinfo_deserialize() */ /*lint !e818 Can't make udata a pointer to const */


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
 * Function:    H5FS_cache_sinfo_serialize
 *
 * Purpose:     Serialize the data structure for writing to disk.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              July 31 2006
 *
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *              Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              May 29, 2008
 *              Converted from H5FS_cache_sinfo_flush
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_sinfo_serialize(const H5F_t * f, hid_t dxpl_id, haddr_t UNUSED addr, 
    size_t UNUSED len, void *image, void *_thing, unsigned *flags, 
    haddr_t UNUSED *new_addr, size_t UNUSED *new_len, void UNUSED **new_image)
{
    herr_t ret_value = SUCCEED;         /* Return value */
    H5FS_sinfo_t * sinfo = (H5FS_sinfo_t *)_thing;
        H5FS_iter_ud_t udata;       /* User data for callbacks */
        uint8_t *p;                 /* Pointer into raw data buffer */
        uint32_t metadata_chksum;   /* Computed metadata checksum value */
        unsigned bin;               /* Current bin we are on */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_sinfo_serialize)
#ifdef QAK
HDFprintf(stderr, "%s: Serialize free space header, addr = %a\n", FUNC, addr);
#endif

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(sinfo);
    HDassert(sinfo->fspace);
    HDassert(sinfo->fspace->sect_cls);
    HDassert(flags);

    /* Sanity check address */
    if(H5F_addr_ne(addr, sinfo->fspace->sect_addr))
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, FAIL, "incorrect address for free space sections")

    /* Allocate temporary buffer */

    p = image;

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
    udata.sect_cnt_size = H5V_limit_enc_size((uint64_t)sinfo->fspace->serial_sect_count);
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
    metadata_chksum = H5_checksum_metadata(image, (size_t)((const uint8_t *)p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == sinfo->fspace->sect_size);
    HDassert(sinfo->fspace->sect_size <= sinfo->fspace->alloc_sect_size);
#ifdef QAK
HDfprintf(stderr, "%s: sinfo->fspace->sect_size = %Hu\n", FUNC, sinfo->fspace->sect_size);
HDfprintf(stderr, "%s: sinfo->fspace->alloc_sect_size = %Hu\n", FUNC, sinfo->fspace->alloc_sect_size);
#endif /* QAK */

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Reset the cache flags for this operation */
    *flags = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_cache_sinfo_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_cache_sinfo_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data structure
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              May 29, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_sinfo_free_icr(haddr_t UNUSED addr, size_t UNUSED len, void *thing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_cache_sinfo_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy B-tree node */
    H5FS_cache_sinfo_dest(thing);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_cache_sinfo_free_icr() */

