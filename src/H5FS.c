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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, May  2, 2006
 *
 * Purpose:	File free space functions.
 *
 * Note:	(Used to be in the H5HFflist.c file, prior to the date above)
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5FS_PACKAGE		/*suppress error about including H5FSpkg  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5FS_init_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FSpkg.h"		/* File free space			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/

/* File free space format version #'s */
#define H5FS_SECTS_VERSION      0               /* Serialized sections */

/* Default starting size of section buffer */
#define H5FS_SECT_SIZE_DEFAULT  64

/* Max. height of the skip list holding free list nodes */
#define H5FS_DEFAULT_SKIPLIST_HEIGHT     16

/* Size of the free space serialized sections on disk */
#define H5FS_SECTS_PREFIX_SIZE(f) (                                           \
    /* General metadata fields */                                             \
    H5FS_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Free space serialized sections specific fields */                      \
    + H5F_SIZEOF_ADDR(f) /* Address of free space header for these sections */ \
    )

/******************/
/* Local Typedefs */
/******************/

/* Free space node for free space sections of the same size */
typedef struct H5FS_node_t {
    hsize_t sect_size;          /* Size of all sections on list */
    H5SL_t *sect_list;          /* Skip list to hold pointers to actual free list section node */
} H5FS_node_t;

/* Free space section bin info */
typedef struct H5FS_bin_t {
    size_t sect_count;          /* Total # of sections in this bin */
    H5SL_t *bin_list;           /* Skip list of differently sized sections */
} H5FS_bin_t;

/* User data for skip list iterator callback for syncing section info */
typedef struct {
    H5F_t *f;                   /* Pointer to the file */
    hid_t dxpl_id;              /* Dataset transfer property list */
} H5FS_iter_ud1_t;

/* User data for skip list iterator callback for iterating over section size nodes when syncing */
typedef struct {
    H5FS_t *fspace;             /* Free space manager info */
    uint8_t **p;                /* Pointer to address of buffer pointer to serialize with */
    unsigned sect_cnt_size;     /* # of bytes to encode section size counts in */
} H5FS_iter_ud2_t;

/* User data for skip list iterator callback for iterating over section size nodes */
typedef struct {
    H5FS_t *fspace;             /* Free space manager info */
    H5FS_operator_t op;         /* Operator for the iteration */
    void *op_data;              /* Information to pass to the operator */
} H5FS_iter_ud3_t;


/********************/
/* Package Typedefs */
/********************/

/* Main free space info */
struct H5FS_t {
    /* Stored values (from header) */
    H5FS_hdr_t *hdr;            /* Pointer to header info                     */

    /* Computed/cached values */
    haddr_t addr;               /* Address of free space header on disk       */
    unsigned nbins;             /* Number of bins                             */
    size_t serial_size;         /* Total serialized size of all section nodes */
    size_t size_count;          /* Number of differently sized sections       */
    unsigned sect_prefix_size;  /* Size of the section serialization prefix (in bytes) */
    unsigned sect_off_size;     /* Size of a section offset (in bytes)        */
    unsigned sect_len_size;     /* Size of a section length (in bytes)        */
    hbool_t dirty;              /* Space information is dirty                 */

    /* Memory data structures (not stored directly) */
    H5FS_section_class_t *sect_cls; /* Array of section classes for this free list */
    H5FS_section_info_t *single; /* Section information when free list has only one free section */
    H5SL_t *merge_list;         /* Skip list to hold sections for detecting merges */
    H5FS_bin_t *bins;           /* Array of lists of lists of free sections   */
};


/********************/
/* Local Prototypes */
/********************/
static herr_t H5FS_open_add(H5FS_t *fspace);
static herr_t H5FS_open_remove(H5FS_t *fspace);
static herr_t H5FS_init(H5FS_t *fspace);
static herr_t H5FS_sect_free_cb(void *item, void *key, void *op_data);
static herr_t H5FS_node_free_cb(void *item, void *key, void *op_data);
static herr_t H5FS_sect_increase(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace);
static herr_t H5FS_sect_decrease(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace);
static herr_t H5FS_add_bin_node(H5FS_t *fspace, H5FS_section_info_t *node);
static htri_t H5FS_find_bin_node(H5FS_t *fspace, hsize_t request, H5FS_section_info_t **node);
static herr_t H5FS_serialize_sect_cb(void *_item, void UNUSED *key, void *_udata);
static herr_t H5FS_serialize_node_cb(void *_item, void UNUSED *key, void *_udata);
static size_t H5FS_serialize_size(H5FS_t *fspace);
static herr_t H5FS_serialize_bins(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace);
static herr_t H5FS_deserialize_bins(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace);
static herr_t H5FS_flush_cb(void *_item, void *key, void *_udata);


/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5FS_section_class_t sequence information */
H5FL_SEQ_DEFINE(H5FS_section_class_t);

/* Declare a free list to manage the H5FS_hdr_t struct */
H5FL_DEFINE(H5FS_hdr_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Skip list to track open free space managers */
H5SL_t *H5FS_open_g = NULL;

/* Declare a free list to manage the H5FS_t struct */
H5FL_DEFINE_STATIC(H5FS_t);

/* Declare a free list to manage the H5FS_node_t struct */
H5FL_DEFINE_STATIC(H5FS_node_t);

/* Declare a free list to manage the H5FS_bin_t sequence information */
H5FL_SEQ_DEFINE_STATIC(H5FS_bin_t);

/* Declare a free list to manage free space section data to/from disk */
H5FL_BLK_DEFINE_STATIC(sect_block);



/*-------------------------------------------------------------------------
 * Function:	H5FS_init_interface
 *
 * Purpose:	Initialize static free space memory structures
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_init_interface(void)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_init_interface)

    /* Create the skip list to track open free space managers */
    HDassert(H5FS_open_g == NULL);
    if(NULL == (H5FS_open_g = H5SL_create(H5SL_TYPE_HADDR, 0.5, H5FS_DEFAULT_SKIPLIST_HEIGHT)))
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for tracking open free space managers")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_term_interface
 *
 * Purpose:	Terminate this interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				affect other interfaces; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 8, 2006
 *
 *-------------------------------------------------------------------------
 */
int
H5FS_term_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_term_interface)

    if(H5_interface_initialize_g) {
        /* Release the open free space manager list */
        HDassert(H5FS_open_g);

        /* All the free space managers should be shut down by now */
        HDassert(H5SL_count(H5FS_open_g) == 0);

        /* Close the skip list to track the open free space managers */
        H5SL_close(H5FS_open_g);
        H5FS_open_g = NULL;

        /* Interface has been shut down */
        H5_interface_initialize_g = 0;
    } /* end if */

    FUNC_LEAVE_NOAPI(0)
} /* end H5FS_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_open_add
 *
 * Purpose:	Add a free space manager to the list of open ones
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_open_add(H5FS_t *fspace)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_open_add)

    /* Check arguments. */
    HDassert(fspace);

    /* Add the free space manager to the list of open managers */
    if(H5SL_insert(H5FS_open_g, fspace, &fspace->addr) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space manager into skip list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_open_add() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_open_remove
 *
 * Purpose:	Remove a free space manager from the list of open ones
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_open_remove(H5FS_t *fspace)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_open_remove)

    /* Check arguments. */
    HDassert(fspace);

    /* Remove the free space manager from the list of open managers */
    if(NULL == H5SL_remove(H5FS_open_g, &fspace->addr))
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space manager from skip list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_open_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_init
 *
 * Purpose:	Initialize free space memory structures
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_init(H5FS_t *fspace)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_init)

    /* Check arguments. */
    HDassert(fspace);

    /* Initialize free space memory structures */
    fspace->single = NULL;
    fspace->bins = NULL;
    fspace->merge_list = NULL;
    fspace->serial_size = 0;
    fspace->size_count = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_init() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_create
 *
 * Purpose:	Allocate & initialize file free space info
 *
 * Return:	Success:	Pointer to free space structure
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
H5FS_t *
H5FS_create(H5F_t *f, hid_t dxpl_id, haddr_t *fs_addr, const H5FS_create_t *fs_create,
    size_t nclasses, H5FS_section_class_t *classes, const void *cls_init_udata)
{
    H5FS_t *fspace = NULL;      /* New free space structure */
    H5FS_hdr_t *fs_hdr = NULL;  /* New free space header */
    size_t u;                   /* Local index variable */
    H5FS_t *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5FS_create, NULL)

    /* Check arguments. */
    HDassert(fs_addr);
    HDassert(fs_create->shrink_percent);
    HDassert(fs_create->shrink_percent < fs_create->expand_percent);
    HDassert(fs_create->max_sect_size);
    HDassert(nclasses == 0 || classes);

    /*
     * Allocate free space structure
     */
    if(NULL == (fspace = H5FL_CALLOC(H5FS_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for free space free list")

    /* Set immutable free list parameters */
    fspace->sect_cls = classes;

    /* Initialize the section classes for this free space list */
    for(u = 0; u < nclasses; u++) {
        /* Call the class initialization routine, if there is one */
        if(fspace->sect_cls[u].init_cls)
            if((fspace->sect_cls[u].init_cls)(&fspace->sect_cls[u], cls_init_udata) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "unable to initialize section class")
    } /* end for */

    /* Allocate space for the free space header */
    if(HADDR_UNDEF == (fspace->addr = H5MF_alloc(f, H5FD_MEM_FSPACE_HDR, dxpl_id, (hsize_t)H5FS_HEADER_SIZE(f))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "file allocation failed for free space header")
    *fs_addr = fspace->addr;

    /* Construct the free space header */
    if(NULL == (fs_hdr = H5FL_MALLOC(H5FS_hdr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&fs_hdr->cache_info, 0, sizeof(H5AC_info_t));

    /* Initialize information for header */
    fs_hdr->tot_space = 0;
    fs_hdr->sect_count = 0;
    fs_hdr->nclasses = nclasses;
    fs_hdr->client = fs_create->client;
    fs_hdr->shrink_percent = fs_create->shrink_percent;
    fs_hdr->expand_percent = fs_create->expand_percent;
    fs_hdr->max_sect_addr = fs_create->max_sect_addr;
    fs_hdr->max_sect_size = fs_create->max_sect_size;
    fs_hdr->alloc_sect_size = 0;
    fs_hdr->sect_addr = HADDR_UNDEF;

    /* Cache the new free space header */
    if(H5AC_set(f, dxpl_id, H5AC_FSPACE_HDR, fspace->addr, fs_hdr, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, NULL, "can't add free space header to cache")
    fs_hdr = NULL;

    /* Lock the free space  header into memory */
    if(NULL == (fs_hdr = H5AC_protect(f, dxpl_id, H5AC_FSPACE_HDR, fspace->addr, NULL, NULL, H5AC_WRITE)))
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTPROTECT, NULL, "unable to load free space header")

    /* Point free space wrapper at header and pin it in the cache */
    fspace->hdr = fs_hdr;
    if(H5AC_pin_protected_entry(f, fs_hdr) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTPIN, NULL, "unable to pin free space header")

    /* Unlock free space header, now pinned */
    if(H5AC_unprotect(f, dxpl_id, H5AC_FSPACE_HDR, fspace->addr, fs_hdr, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPROTECT, NULL, "unable to release free space header")
    fs_hdr = NULL;

    /* Set modifiable free space parameters */
    fspace->nbins = H5V_log2_gen(fspace->hdr->max_sect_size);
    fspace->sect_prefix_size = H5FS_SECTS_PREFIX_SIZE(f);
    fspace->sect_off_size = (fspace->hdr->max_sect_addr + 7) / 8;
    fspace->sect_len_size = (H5V_log2_gen(fspace->hdr->max_sect_size) + 7) / 8;
    H5FS_init(fspace);
#ifdef QAK
HDfprintf(stderr, "%s: fspace->nbins = %u\n", FUNC, fspace->nbins);
HDfprintf(stderr, "%s: fspace->sect_off_size = %u, fspace->sect_len_size = %u\n", FUNC, fspace->sect_off_size, fspace->sect_len_size);
#endif /* QAK */

    /* Set current space used for free space sections (for no sections) */
    fspace->hdr->sect_size = H5FS_serialize_size(fspace);
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_size = %Hu\n", FUNC, fspace->hdr->sect_size);
#endif /* QAK */

    /* Flag the free space as dirty */
    fspace->dirty = TRUE;

    /* Add the free space manager to the list of open free space managers */
    if(H5FS_open_add(fspace) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, NULL, "can't add free space header to open list")

    /* Set return value */
    ret_value = fspace;

done:
    if(!ret_value) {
        if(fspace)
            if(H5FS_close(f, dxpl_id, fspace) < 0)
                HDONE_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "unable to release free space info")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_create() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_open
 *
 * Purpose:	Open an existing file free space info structure on disk
 *
 * Return:	Success:	Pointer to free space structure
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May  2, 2006
 *
 *-------------------------------------------------------------------------
 */
H5FS_t *
H5FS_open(H5F_t *f, hid_t dxpl_id, haddr_t fs_addr, size_t nclasses,
    H5FS_section_class_t *classes, const void *cls_init_udata)
{
    H5FS_hdr_t *fs_hdr = NULL;  /* Free space header loaded from file */
    H5FS_t *fspace = NULL;      /* New free space structure */
    size_t u;                   /* Local index variable */
    H5FS_t *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5FS_open, NULL)
#ifdef QAK
HDfprintf(stderr, "%s: Opening free space manager\n", FUNC);
#endif /* QAK */

    /* Check arguments. */
    HDassert(H5F_addr_defined(fs_addr));
    HDassert(nclasses == 0 || classes);

    /* Allocate free space structure */
    if(NULL == (fspace = H5FL_MALLOC(H5FS_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for free space free list")

    /* Protect the free space header */
    if(NULL == (fs_hdr = H5AC_protect(f, dxpl_id, H5AC_FSPACE_HDR, fs_addr, NULL, NULL, H5AC_READ)))
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTPROTECT, NULL, "unable to protect free space header")

    /* Point free space wrapper at header and pin it in the cache */
    fspace->hdr = fs_hdr;
    if(H5AC_pin_protected_entry(f, fs_hdr) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTPIN, NULL, "unable to pin free space header")

    /* Release the free space header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_FSPACE_HDR, fs_addr, fs_hdr, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPROTECT, NULL, "unable to release free space header")
    fs_hdr = NULL;

    /* Set immutable free list parameters */
    fspace->addr = fs_addr;
    HDassert(fspace->hdr->nclasses == nclasses);
    fspace->sect_cls = classes;

    /* Initialize the section classes for this free space list */
    for(u = 0; u < nclasses; u++) {
        /* Call the class initialization routine, if there is one */
        if(fspace->sect_cls[u].init_cls)
            if((fspace->sect_cls[u].init_cls)(&fspace->sect_cls[u], cls_init_udata) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "unable to initialize section class")
    } /* end for */

    /* Set modifiable free space parameters */
    fspace->nbins = H5V_log2_gen(fspace->hdr->max_sect_size);
    fspace->sect_prefix_size = H5FS_SECTS_PREFIX_SIZE(f);
    fspace->sect_off_size = (fspace->hdr->max_sect_addr + 7) / 8;
    fspace->sect_len_size = (H5V_log2_gen(fspace->hdr->max_sect_size) + 7) / 8;
    H5FS_init(fspace);
#ifdef QAK
HDfprintf(stderr, "%s: fspace->nbins = %u\n", FUNC, fspace->nbins);
HDfprintf(stderr, "%s: fspace->sect_off_size = %u, fspace->sect_len_size = %u\n", FUNC, fspace->sect_off_size, fspace->sect_len_size);
#endif /* QAK */

    /* The free space is clean, currently */
    fspace->dirty = FALSE;

    /* Go get all the sections, if there are any */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_addr = %a\n", FUNC, fspace->hdr->sect_addr);
#endif /* QAK */
    if(fspace->hdr->sect_count > 0) {
        HDassert(H5F_addr_defined(fspace->hdr->sect_addr));
        HDassert(fspace->hdr->sect_size > 0);
        if(H5FS_deserialize_bins(f, dxpl_id, fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTDECODE, NULL, "can't deserialize sections")
    } /* end if */

    /* Add the free space manager to the list of open free space managers */
    if(H5FS_open_add(fspace) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, NULL, "can't add free space manager to open list")

    /* Set return value */
    ret_value = fspace;

done:
    if(!ret_value) {
        if(fspace)
            if(H5FS_close(f, dxpl_id, fspace) < 0)
                HDONE_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "unable to release free space info")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_open() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_increase
 *
 * Purpose:	Increase the size of the serialized free space section info
 *              on disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_increase(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_increase)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(fspace->hdr);

    /* Increment # of sections on free space list */
    fspace->hdr->sect_count++;

    /* Update the free space sections' serialized size */
    fspace->hdr->sect_size = H5FS_serialize_size(fspace);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_increase() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_decrease
 *
 * Purpose:	Decrease the size of the serialized free space section info
 *              on disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_decrease(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_decrease)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(fspace->hdr);

    /* Decrement total # of sections in free space manager */
    fspace->hdr->sect_count--;

    /* Drop back to using a "single" node when there's only one section */
    if(fspace->hdr->sect_count == 1) {
        H5FS_node_t *fspace_node;       /* Free list size node */
        H5FS_section_info_t *sect;      /* Section to move to 'single' info */
        unsigned bin;                   /* Bin with node */
        unsigned u;                     /* Local index variable */

        /* Sanity check */
        HDassert(fspace->single == NULL);
        HDassert(fspace->size_count == 1);

        /* Search for the bin with the node */
        for(u = 0; u < fspace->nbins; u++)
            if(fspace->bins[u].sect_count) {
                HDassert(fspace->bins[u].sect_count == 1);
                bin = u;
                break;
            } /* end if */

#ifndef NDEBUG
        /* Sanity check rest of bins */
        for(u++ ; u < fspace->nbins; u++)
            HDassert(fspace->bins[u].sect_count == 0);
#endif /* NDEBUG */

        /* Sanity check section size count for bin */
        HDassert(H5SL_count(fspace->bins[bin].bin_list) == 1);
        HDassert(fspace->bins[bin].sect_count == 1);

        /* Remove the free space section size node from the bin list */
        if(NULL == (fspace_node = H5SL_remove_first(fspace->bins[bin].bin_list)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space section size node from skip list")

        /* Decrement # of section sizes in bin */
        fspace->bins[bin].sect_count = 0;

        /* Make certain there's only one section of this size */
        HDassert(H5SL_count(fspace_node->sect_list) == 1);

        /* Remove the free space section from the section size list */
        if(NULL == (sect = H5SL_remove_first(fspace_node->sect_list)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space section from skip list")

        /* Destroy skip list for size tracking node */
        if(H5SL_close(fspace_node->sect_list) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCLOSEOBJ, FAIL, "can't destroy size tracking node's skip list")

        /* Release free space list node */
        H5FL_FREE(H5FS_node_t, fspace_node);

        /* Capture single section's information */
        fspace->single = sect;
    } /* end if */

    /* Update the free space sections' serialized size */
    if(fspace->hdr->sect_count > 0)
        fspace->hdr->sect_size = H5FS_serialize_size(fspace);
    else
        fspace->hdr->sect_size = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_decrease() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_remove_size_node
 *
 * Purpose:	Remove a section size node from size tracking data structures for
 *              a free space manager
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_size_node_decr(H5FS_t *fspace, H5FS_node_t *fspace_node, unsigned bin)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_size_node_decr)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace_node);

    /* Check for no more nodes on list of that size */
    if(H5SL_count(fspace_node->sect_list) == 0) {
        H5FS_node_t *tmp_fspace_node;       /* Free space list size node */

        /* Remove size tracking list from bin */
        tmp_fspace_node = H5SL_remove(fspace->bins[bin].bin_list, &fspace_node->sect_size);
        if(tmp_fspace_node == NULL || tmp_fspace_node != fspace_node)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space node from skip list")

        /* Destroy skip list for size tracking node */
        if(H5SL_close(fspace_node->sect_list) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCLOSEOBJ, FAIL, "can't destroy size tracking node's skip list")

        /* Release free space list node */
        H5FL_FREE(H5FS_node_t, fspace_node);

        /* Decrement number of section sizes */
        fspace->size_count--;
    } /* end if */

    /* Decrement the # of sections in this bin */
    /* (Different from the # of items in the bin's skiplist, since each node on
     *  the bin's skiplist is also a skiplist...)
     */
    fspace->bins[bin].sect_count--;
#ifdef QAK
HDfprintf(stderr, "%s: fspace->bins[%u].sect_count = %Zu\n", FUNC, bin, fspace->bins[bin].sect_count);
#endif /* QAK */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_size_node_decr() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_unlink_size
 *
 * Purpose:	Remove a section node from size tracking data structures for
 *              a free space manager
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_unlink_size(H5FS_t *fspace, H5FS_section_info_t *sect)
{
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_unlink_size)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(sect);

    /* Check for only a single section */
    if(fspace->single) {
        /* Verify that single section is correct */
        if(H5F_addr_ne(fspace->single->addr, sect->addr))
            HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't remove single section")

        /* Reset 'single' section pointer */
        fspace->single = NULL;

        /* Decrement number of section sizes */
        fspace->size_count--;
    } /* end if */
    else {
        H5FS_node_t *fspace_node;       /* Free list size node */
        H5FS_section_info_t *tmp_sect_node; /* Temporary section node */
        unsigned bin;                   /* Bin to put the free space section in */

        /* Sanity check */
        HDassert(fspace->bins);

        /* Determine correct bin which holds items of at least the section's size */
        bin = H5V_log2_gen(sect->size);
        HDassert(bin < fspace->nbins);
        if(fspace->bins[bin].bin_list == NULL)
            HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "node's bin is empty?")

        /* Find space node for section's size */
        if((fspace_node = H5SL_search(fspace->bins[bin].bin_list, &sect->size)) == NULL)
            HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section size node")

        /* Remove the section's node from the list */
        tmp_sect_node = H5SL_remove(fspace_node->sect_list, &sect->addr);
        if(tmp_sect_node == NULL || tmp_sect_node != sect)
            HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section node on size list")

        /* Decrement # of sections in section size node */
        if(H5FS_size_node_decr(fspace, fspace_node, bin) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space size node from skip list")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_unlink_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_unlink_rest
 *
 * Purpose:	Finish unlinking a section from the rest of the free space
 *              manager's data structures, after the section has been removed
 *              from the size tracking data structures
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_unlink_rest(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, H5FS_section_info_t *sect)
{
    H5FS_section_info_t *tmp_sect_node; /* Temporary section node */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_unlink_rest)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(sect);

    /* Remove node from merge list */
    tmp_sect_node = H5SL_remove(fspace->merge_list, &sect->addr);
    if(tmp_sect_node == NULL || tmp_sect_node != sect)
        HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section node on size list")

    /* Decrement amount of space required to serialize all sections */
    fspace->serial_size -= fspace->sect_cls[sect->cls->type].serial_size;
#ifdef QAK
HDfprintf(stderr, "%s: fspace->serial_size = %Zu\n", FUNC, fspace->serial_size);
HDfprintf(stderr, "%s: fspace->sect_cls[(*node)->cls->type].serial_size = %Zu\n", FUNC, fspace->sect_cls[(*node)->cls->type].serial_size);
#endif /* QAK */

    /* Update section info & check if we need less room for the serialized free space sections */
    if(H5FS_sect_decrease(f, dxpl_id, fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't increase free space section size on disk")

    /* Decrement amount of free space managed */
    fspace->hdr->tot_space -= sect->size;

    /* Mark free space sections as changed */
    fspace->dirty = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_unlink_rest() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_unlink
 *
 * Purpose:	Unlink a section from the internal data structures
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_unlink(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, H5FS_section_info_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_unlink)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(sect);

    /* Remove node from size tracked data structures */
    if(H5FS_sect_unlink_size(fspace, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from size tracking data structures")

    /* Update rest of free space manager data structures for node removal */
    if(H5FS_sect_unlink_rest(f, dxpl_id, fspace, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from non-size tracking data structures")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_unlink() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_link_size_bin
 *
 * Purpose:	Add a section of free space to the free list bins
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_link_size_bin(H5FS_t *fspace, H5FS_section_info_t *sect)
{
    H5FS_node_t *fspace_node = NULL;     /* Pointer to free space node of the correct size */
    unsigned bin;                       /* Bin to put the free space section in */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_link_size_bin)
#ifdef QAK
HDfprintf(stderr, "%s: sect->size = %Hu, sect->addr = %a\n", FUNC, sect->size, sect->addr);
#endif /* QAK */

    /* Check arguments. */
    HDassert(fspace);
    HDassert(sect);
    HDassert(H5F_addr_defined(sect->addr));
    HDassert(sect->size);

    /* Determine correct bin which holds items of the section's size */
    bin = H5V_log2_gen(sect->size);
    HDassert(bin < fspace->nbins);
    if(fspace->bins[bin].bin_list == NULL) {
        if(NULL == (fspace->bins[bin].bin_list = H5SL_create(H5SL_TYPE_HSIZE, 0.5, H5FS_DEFAULT_SKIPLIST_HEIGHT)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for free space nodes")
    } /* end if */
    else {
        /* Check for node list of the correct size already */
        fspace_node = H5SL_search(fspace->bins[bin].bin_list, &sect->size);
    } /* end else */

    /* Check if we need to create a new skip list for nodes of this size */
    if(fspace_node == NULL) {
        /* Allocate new free list size node */
        if(NULL == (fspace_node = H5FL_MALLOC(H5FS_node_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for free space node")

        /* Initialize the free list size node */
        fspace_node->sect_size = sect->size;
        if(NULL == (fspace_node->sect_list = H5SL_create(H5SL_TYPE_HADDR, 0.5, H5FS_DEFAULT_SKIPLIST_HEIGHT)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for free space nodes")

        /* Insert new free space size node into bin's list */
        if(H5SL_insert(fspace->bins[bin].bin_list, fspace_node, &fspace_node->sect_size) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into skip list")

        /* Increment number of section sizes */
        fspace->size_count++;
    } /* end if */

    /* Increment # of section in bin */
    /* (Different from the # of items in the bin's skiplist, since each node on
     *  the bin's skiplist is also a skiplist...)
     */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->bins[%u].sect_count = %Zu\n", FUNC, bin, fspace->bins[bin].sect_count);
#endif /* QAK */
    fspace->bins[bin].sect_count++;

    /* Insert free space node into correct skip list */
    if(H5SL_insert(fspace_node->sect_list, sect, &sect->addr) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into skip list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_link_size_bin() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_link_size
 *
 * Purpose:	Link a section into size tracking data structures
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_link_size(H5FS_t *fspace, H5FS_section_info_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_link_size)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(sect);

    /* Check for special cases of # of sections on free list */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_count = %Hu\n", FUNC, fspace->hdr->sect_count);
#endif /* QAK */
    if(fspace->hdr->sect_count == 0) {
        HDassert(fspace->single == NULL);

        /* Capture single section's information */
        fspace->single = sect;

        /* Increment number of section sizes */
        fspace->size_count++;
    } /* end if */
    else {
        /* Have a single section, put it into the bins */
         if(fspace->single) {
            /* Check if we should allocate the bins */
            if(fspace->bins == NULL)
                /* Allocate the bins for free space sizes */
                if(NULL == (fspace->bins = H5FL_SEQ_CALLOC(H5FS_bin_t, fspace->nbins)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for free space bins")

            /* Insert the current single section into the bins */
            if(H5FS_sect_link_size_bin(fspace, fspace->single) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into skip list")
            fspace->single = NULL;

            /* Decrement number of section sizes */
            /* (from increment in H5FS_sect_link_size_bin for inserting the single section) */
            fspace->size_count--;
        } /* end if */
        HDassert(fspace->single == NULL);

        /* Put new section into bins */
        if(H5FS_sect_link_size_bin(fspace, sect) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into skip list")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_link_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_link_rest
 *
 * Purpose:	Link a section into the rest of the non-size tracking
 *              free space manager data structures
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_link_rest(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, H5FS_section_info_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_link_rest)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(sect);

    /* Add section to the address-ordered list of sections */
    if(fspace->merge_list == NULL) {
        if(NULL == (fspace->merge_list = H5SL_create(H5SL_TYPE_HADDR, 0.5, H5FS_DEFAULT_SKIPLIST_HEIGHT)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for merging free space sections")
    } /* end if */
    if(H5SL_insert(fspace->merge_list, sect, &sect->addr) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into merging skip list")

    /* Increment amount of space required to serialize all sections */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->serial_size = %Zu\n", FUNC, fspace->serial_size);
HDfprintf(stderr, "%s: fspace->sect_cls[sect->cls->type].serial_size = %Zu\n", FUNC, fspace->sect_cls[sect->cls->type].serial_size);
#endif /* QAK */
    fspace->serial_size += fspace->sect_cls[sect->cls->type].serial_size;

    /* Update section info & check if we need more room for the serialized free space sections */
    if(H5FS_sect_increase(f, dxpl_id, fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't increase free space section size on disk")

    /* Increment amount of free space managed */
    fspace->hdr->tot_space += sect->size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_link_rest() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_link
 *
 * Purpose:	Link a section into the internal data structures
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_link(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, H5FS_section_info_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_link)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(sect);

    /* Add node to size tracked data structures */
    if(H5FS_sect_link_size(fspace, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't add section to size tracking data structures")

    /* Update rest of free space manager data structures for section addition */
    if(H5FS_sect_link_rest(f, dxpl_id, fspace, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't add section to non-size tracking data structures")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_link() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_merge
 *
 * Purpose:	Attempt to merge a returned free space section with existing
 *              free space.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_merge(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    H5FS_section_info_t **sect, void *op_data)
{
    H5FS_section_info_t *tmp_sect_node; /* Temporary free space section */
    hbool_t merged;                     /* Flag to indicate merge occurred */
    htri_t status;                      /* Status value */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_merge)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->merge_list);
    HDassert(*sect);
    HDassert(H5F_addr_defined((*sect)->addr));
    HDassert((*sect)->size);

    /* Loop until no more merging */
    do {
        /* Reset 'merge occurred' flag */
        merged = FALSE;

        /* Look for neighboring section before new section */
        tmp_sect_node = H5SL_less(fspace->merge_list, &(*sect)->addr);

        /* Check for node before new node able to merge with new node */
        if(tmp_sect_node && tmp_sect_node->cls->can_merge) {
            /* Determine if the sections can merge */
            if((status = (*tmp_sect_node->cls->can_merge)(tmp_sect_node, *sect, op_data)) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't check for merging sections")
            if(status > 0) {
                /* Sanity check */
                HDassert((*sect)->cls->merge);

                /* Remove 'less than' node from data structures */
                if(H5FS_sect_unlink(f, dxpl_id, fspace, tmp_sect_node) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")

                /* Merge the two sections together */
                if((*tmp_sect_node->cls->merge)(tmp_sect_node, *sect, op_data) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't merge two sections")

                /* Retarget section pointer to 'less than' node that was merged into */
                *sect = tmp_sect_node;

                /* Indicate successful merge occurred */
                merged = TRUE;
            } /* end if */
        } /* end if */

        /* Look for section after new (or merged) section */
        tmp_sect_node = H5SL_greater(fspace->merge_list, &(*sect)->addr);

        /* Check for node after new node able to merge with new node */
        if(tmp_sect_node && tmp_sect_node->cls->can_merge) {
            /* Determine if the sections can merge */
            if((status = (*tmp_sect_node->cls->can_merge)(*sect, tmp_sect_node, op_data)) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't check for merging sections")
            if(status > 0) {
                /* Sanity check */
                HDassert((*sect)->cls->merge);

                /* Remove 'greater than' node from data structures */
                if(H5FS_sect_unlink(f, dxpl_id, fspace, tmp_sect_node) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")

                /* Merge the two sections together */
                if((*(*sect)->cls->merge)(*sect, tmp_sect_node, op_data) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't merge two sections")

                /* Indicate successful merge occurred */
                merged = TRUE;
            } /* end if */
        } /* end if */
    } while(merged);

    /* Check for (possibly merged) section able to shrink the size of the container */
    if((*sect)->cls->can_shrink) {
        if((status = (*(*sect)->cls->can_shrink)(*sect, op_data)) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSHRINK, FAIL, "can't check for shrinking container")
        if(status > 0) {
#ifdef QAK
HDfprintf(stderr, "%s: Can shrink!\n", FUNC);
#endif /* QAK */
            /* Shrink the container */
            /* (callback can indicate that it has discarded the section by setting *sect to NULL) */
            if((*(*sect)->cls->shrink)(sect, op_data) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't shrink free space container")
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_add
 *
 * Purpose:	Add a section of free space to the free list
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_add(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, H5FS_section_info_t *sect,
    unsigned flags, void *op_data)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5FS_add, FAIL)

#ifdef QAK
HDfprintf(stderr, "%s: sect->size = %Hu, sect->addr = %a, sect->cls->type = %u\n", FUNC, sect->size, sect->addr, sect->cls->type);
#endif /* QAK */

    /* Check arguments. */
    HDassert(fspace);
    HDassert(sect);
    HDassert(H5F_addr_defined(sect->addr));
    HDassert(sect->size);

    /* Check for merging returned space with existing section node */
    if(flags & H5FS_ADD_RETURNED_SPACE && fspace->hdr->sect_count > 0) {
#ifdef QAK
HDfprintf(stderr, "%s: Returning space\n", FUNC);
#endif /* QAK */
        
        /* Attempt to merge returned section with existing sections */
        if(H5FS_sect_merge(f, dxpl_id, fspace, &sect, op_data) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't merge sections")
    } /* end if */

    /* Add new (possibly merged or even discarded) node to free sections */
    if(sect)
        if(H5FS_sect_link(f, dxpl_id, fspace, sect) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space section into skip list")

    /* Mark free space sections as changed */
    /* (if we're not deserializing all the sections) */
    if(!(flags & H5FS_ADD_DESERIALIZING))
        fspace->dirty = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_add() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_find_bin_node
 *
 * Purpose:	Locate a section of free space (in existing free space list
 *              bins) that is large enough to fulfill request.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5FS_find_bin_node(H5FS_t *fspace, hsize_t request, H5FS_section_info_t **node)
{
    H5FS_node_t *fspace_node;        /* Free list size node */
    unsigned bin;                   /* Bin to put the free space section in */
    htri_t ret_value = FALSE;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_find_bin_node)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->bins);
    HDassert(request > 0);
    HDassert(node);

    /* Determine correct bin which holds items of at least the section's size */
    bin = H5V_log2_gen(request);
    HDassert(bin < fspace->nbins);
    while(bin < fspace->nbins && fspace->bins[bin].bin_list == NULL)
        bin++;

    /* Find the first free space section that is large enough to fulfill request */
    /* (Since the bins use skip lists to track the sizes of the address-ordered
     *  lists, this is actually a "best fit" algorithm)
     */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->nbins = %u\n", FUNC, fspace->nbins);
HDfprintf(stderr, "%s: bin = %u\n", FUNC, bin);
#endif /* QAK */
    if(bin < fspace->nbins)
        do {
            /* Look for large enough free space section in this bin */
            if(fspace->bins[bin].bin_list)
                /* Check for large enough list of sections on list */
                if((fspace_node = H5SL_greater(fspace->bins[bin].bin_list, &request))) {
                    /* Take first node off of the list (ie. node w/lowest address) */
                    if(NULL == (*node = H5SL_remove_first(fspace_node->sect_list)))
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space node from skip list")

                    /* Decrement # of sections in section size node */
                    if(H5FS_size_node_decr(fspace, fspace_node, bin) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space size node from skip list")

                    /* Indicate that we found a node for the request */
                    HGOTO_DONE(TRUE)
                } /* end if */

            /* Advance to next larger bin */
            bin++;
        } while(bin < fspace->nbins);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_find_bin_node() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_find
 *
 * Purpose:	Locate a section of free space (in existing free space list) that
 *              is large enough to fulfill request.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FS_find(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, hsize_t request, H5FS_section_info_t **node)
{
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI(H5FS_find, FAIL)

#ifdef QAK
HDfprintf(stderr, "%s: request = %Hu\n", FUNC, request);
#endif /* QAK */

    /* Check arguments. */
    HDassert(fspace);
    HDassert(request);
    HDassert(node);

    /* Check for any sections on free space list */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_count = %Hu\n", FUNC, fspace->hdr->sect_count);
#endif /* QAK */
    if(fspace->hdr->sect_count > 0) {
        /* Check for single section */
        if(fspace->single) {
            /* See if single section is large enough */
            if(fspace->single->size >= request) {
                /* 'single' section fulfills request */
                *node = fspace->single;
                fspace->single = NULL;

                /* Decrement number of section sizes */
                fspace->size_count--;

                /* Found a good section */
                ret_value = TRUE;
            } /* end if */
            else
                HGOTO_DONE(FALSE)
        } /* end if */
        else {
            /* Look for node in bins */
            if((ret_value = H5FS_find_bin_node(fspace, request, node)) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from bins")
        } /* end else */

        /* Decrement # of sections on free list, if we found an object */
        if(ret_value > 0) {
            /* Update rest of free space manager data structures for node removal */
            if(H5FS_sect_unlink_rest(f, dxpl_id, fspace, *node) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from non-size tracking data structures")
#ifdef QAK
HDfprintf(stderr, "%s: (*node)->size = %Hu, (*node)->addr = %a, (*node)->cls->type = %u\n", FUNC, (*node)->size, (*node)->addr, (*node)->cls->type);
#endif /* QAK */
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_find() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_free_cb
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
H5FS_sect_free_cb(void *_sect, void UNUSED *key, void UNUSED *op_data)
{
    H5FS_section_info_t *sect = (H5FS_section_info_t *)_sect;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_sect_free_cb)

    HDassert(sect);

    /* Call the 'free' method for the section's class */
    (*sect->cls->free)(sect);

    FUNC_LEAVE_NOAPI(0)
}   /* H5FS_sect_free_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_node_free_cb
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
H5FS_node_free_cb(void *item, void UNUSED *key, void UNUSED *op_data)
{
    H5FS_node_t *fspace_node = (H5FS_node_t *)item;       /* Temporary pointer to free space list node */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_node_free_cb)

    HDassert(fspace_node);

    /* Release the skip list for sections of this size */
    H5SL_destroy(fspace_node->sect_list, H5FS_sect_free_cb, NULL);

    /* Release free space list node */
    H5FL_FREE(H5FS_node_t, fspace_node);

    FUNC_LEAVE_NOAPI(0)
}   /* H5FS_node_free_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_serialize_sect_cb
 *
 * Purpose:	Skip list iterator callback to serialize free space sections
 *              of a particular size
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_serialize_sect_cb(void *_item, void UNUSED *key, void *_udata)
{
    H5FS_section_info_t *sect_info = (H5FS_section_info_t *)_item;   /* Free space section to work on */
    H5FS_iter_ud2_t *udata = (H5FS_iter_ud2_t *)_udata; /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_serialize_sect_cb)

    /* Check arguments. */
    HDassert(sect_info);
    HDassert(udata->fspace);
    HDassert(udata->p);

    /* The address of the section */
    UINT64ENCODE_VAR(*udata->p, sect_info->addr, udata->fspace->sect_off_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect_info->addr = %a\n", FUNC, sect_info->addr);
#endif /* QAK */

    /* The type of this section */
    *(*udata->p)++ = (uint8_t)sect_info->cls->type;
#ifdef QAK
HDfprintf(stderr, "%s: sect_info->cls->type = %u\n", FUNC, (unsigned)sect_info->cls->type);
#endif /* QAK */

    /* Call 'serialize' callback for this section */
    if(sect_info->cls->serialize) {
        if((*sect_info->cls->serialize)(sect_info, *udata->p) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSERIALIZE, FAIL, "can't syncronize section")

        /* Update offset in serialization buffer */
        (*udata->p) += sect_info->cls->serial_size;
    } /* end if */
    else
        HDassert(sect_info->cls->serial_size == 0);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_serialize_sect_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_serialize_node_cb
 *
 * Purpose:	Skip list iterator callback to serialize free space sections
 *              in a bin
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_serialize_node_cb(void *_item, void UNUSED *key, void *_udata)
{
    H5FS_node_t *fspace_node = (H5FS_node_t *)_item;   /* Free space size node to work on */
    H5FS_iter_ud2_t *udata = (H5FS_iter_ud2_t *)_udata; /* Callback info */
    size_t node_count;                  /* Number of sections of this size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_serialize_node_cb)

    /* Check arguments. */
    HDassert(fspace_node);
    HDassert(udata->fspace);
    HDassert(udata->p);

    /* The number of sections of this node's size */
    node_count = H5SL_count(fspace_node->sect_list);
    HDassert(node_count);
    UINT64ENCODE_VAR(*udata->p, node_count, udata->sect_cnt_size);
#ifdef QAK
HDfprintf(stderr, "%s: node_count = %Zu\n", FUNC, node_count);
#endif /* QAK */

    /* The size of the sections for this node */
    UINT64ENCODE_VAR(*udata->p, fspace_node->sect_size, udata->fspace->sect_len_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect_size = %Hu\n", FUNC, fspace_node->sect_size);
#endif /* QAK */

    /* Iterate through all the sections of this size */
    HDassert(fspace_node->sect_list);
    if(H5SL_iterate(fspace_node->sect_list, H5FS_serialize_sect_cb, udata) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section nodes")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_serialize_node_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_serialize_size
 *
 * Purpose:	Determine serialized size of all sections in free space manager
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	(can't fail)
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5FS_serialize_size(H5FS_t *fspace)
{
    size_t sect_buf_size;               /* Section buffer size */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_serialize_size)

    /* Check arguments. */
    HDassert(fspace);

    /* Compute the size of the buffer required to serialize all the sections */

    /* Serialized sections prefix */
    sect_buf_size = fspace->sect_prefix_size;

    /* Count for each differently sized section */
    sect_buf_size += fspace->size_count * MAX(1, ((H5V_log2_gen(fspace->hdr->sect_count) + 7) / 8));

    /* Size for each differently sized section */
    sect_buf_size += fspace->size_count * fspace->sect_len_size;

    /* Offsets of each section in address space */
    sect_buf_size += fspace->hdr->sect_count * fspace->sect_off_size;

    /* Class of each section */
    sect_buf_size += fspace->hdr->sect_count * 1;

    /* Extra space required to serialize each section */
    sect_buf_size += fspace->serial_size;

    FUNC_LEAVE_NOAPI(sect_buf_size)
} /* H5FS_serialize_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_serialize_bins
 *
 * Purpose:	Serialize all bins into proper form on disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_serialize_bins(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace)
{
    H5FS_iter_ud2_t udata;              /* User data for callbacks */
    uint8_t *sect_buf = NULL;           /* Buffer for sections */
    uint8_t *p;                         /* Pointer into raw data buffer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_serialize_bins)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(fspace->dirty);

#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_addr = %a\n", FUNC, fspace->hdr->sect_addr);
HDfprintf(stderr, "%s: fspace->hdr->sect_size = %Hu\n", FUNC, fspace->hdr->sect_size);
HDfprintf(stderr, "%s: fspace->hdr->sect_count = %Hu\n", FUNC, fspace->hdr->sect_count);
HDfprintf(stderr, "%s: fspace->serial_size = %Zu\n", FUNC, fspace->serial_size);
#endif /* QAK */

    /* Check for no free sections */
    if(fspace->hdr->sect_count == 0) {
        /* Check for existing serialized sections on disk to release */
        if(H5F_addr_defined(fspace->hdr->sect_addr)) {
            /* Free previous serialized sections disk space */
            if(H5MF_xfree(f, H5FD_MEM_FSPACE_SECTS, dxpl_id, fspace->hdr->sect_addr, (hsize_t)fspace->hdr->alloc_sect_size)<0)
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTFREE, FAIL, "unable to release free space sections")

            /* Reset address and size of serialized sections on disk */
            fspace->hdr->sect_addr = HADDR_UNDEF;
            fspace->hdr->sect_size = 0;
        } /* end if */
    } /* end if */
    else {
        /* Check for no space on disk allocated for the serialized sections */
        if(!H5F_addr_defined(fspace->hdr->sect_addr)) {
            /* Compute size to store sections on disk */
            fspace->hdr->alloc_sect_size = (size_t)fspace->hdr->sect_size * (double)fspace->hdr->expand_percent / 100.0;

            /* Allocate space for the new serialized sections on disk */
            if(HADDR_UNDEF == (fspace->hdr->sect_addr = H5MF_alloc(f, H5FD_MEM_FSPACE_SECTS, dxpl_id, (hsize_t)fspace->hdr->alloc_sect_size)))
                HGOTO_ERROR(H5E_STORAGE, H5E_NOSPACE, FAIL, "file allocation failed for free space sections")
        } /* end if */
        else {
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_size = %Hu\n", FUNC, fspace->hdr->sect_size);
HDfprintf(stderr, "%s: fspace->hdr->alloc_sect_size = %Hu\n", FUNC, fspace->hdr->alloc_sect_size);
#endif /* QAK */
            if(fspace->hdr->sect_size > fspace->hdr->alloc_sect_size) {
                size_t new_size;                    /* New size of space for serialized sections */
/* Currently, the old block data is "thrown away" after the space is reallocated,
 * so avoid data copy in H5MF_realloc() call by just free'ing the space and
 * allocating new space.
 *
 * This also keeps the file smaller, by freeing the space and then
 * allocating new space, instead of vice versa (in H5MF_realloc).
 *
 * QAK - 5/ 8/2006
 */
                /* Free previous serialized sections disk space */
                if(H5MF_xfree(f, H5FD_MEM_FSPACE_SECTS, dxpl_id, fspace->hdr->sect_addr, (hsize_t)fspace->hdr->alloc_sect_size)<0)
                    HGOTO_ERROR(H5E_STORAGE, H5E_CANTFREE, FAIL, "unable to free free space sections")

                /* Compute new size */
                new_size = fspace->hdr->alloc_sect_size;
                while(new_size < fspace->hdr->sect_size)
                    new_size *= (double)fspace->hdr->expand_percent / 100.0;
                fspace->hdr->alloc_sect_size = new_size;

                /* Allocate space for the new serialized sections on disk */
                if(HADDR_UNDEF == (fspace->hdr->sect_addr = H5MF_alloc(f, H5FD_MEM_FSPACE_SECTS, dxpl_id, (hsize_t)fspace->hdr->alloc_sect_size)))
                    HGOTO_ERROR(H5E_STORAGE, H5E_NOSPACE, FAIL, "file allocation failed for free space sections")
            } /* end if */
            else {
                size_t decrease_threshold;          /* Size threshold for decreasing serialized section size */
                hsize_t new_size;                   /* New size of space for serialized sections */

                /* Compute the threshold for decreasing the sections' serialized size */
                decrease_threshold = ((size_t)fspace->hdr->alloc_sect_size * (double)fspace->hdr->shrink_percent) / 100.0;

#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_size = %Hu\n", FUNC, fspace->hdr->sect_size);
HDfprintf(stderr, "%s: fspace->hdr->alloc_sect_size = %Hu\n", FUNC, fspace->hdr->alloc_sect_size);
#endif /* QAK */
                if(fspace->hdr->alloc_sect_size > H5FS_SECT_SIZE_DEFAULT &&
                        fspace->hdr->sect_size < decrease_threshold) {
/* Currently, the old block data is "thrown away" after the space is reallocated,
 * so avoid data copy in H5MF_realloc() call by just free'ing the space and
 * allocating new space.
 *
 * This also keeps the file smaller, by freeing the space and then
 * allocating new space, instead of vice versa (in H5MF_realloc).
 *
 * QAK - 5/ 8/2006
 */
                    /* Free previous serialized sections disk space */
                    if(H5MF_xfree(f, H5FD_MEM_FSPACE_SECTS, dxpl_id, fspace->hdr->sect_addr, (hsize_t)fspace->hdr->alloc_sect_size)<0)
                        HGOTO_ERROR(H5E_STORAGE, H5E_CANTFREE, FAIL, "unable to free free space sections")

                    /* Compute new size */
                    while(fspace->hdr->sect_size < decrease_threshold) {
                        new_size = decrease_threshold;

                        decrease_threshold *= (double)fspace->hdr->shrink_percent / 100.0;
                    } /* end while */
                    if(new_size < H5FS_SECT_SIZE_DEFAULT)
                        new_size = H5FS_SECT_SIZE_DEFAULT;
                    fspace->hdr->alloc_sect_size = new_size;

                    /* Allocate space for the new serialized sections on disk */
                    if(HADDR_UNDEF == (fspace->hdr->sect_addr = H5MF_alloc(f, H5FD_MEM_FSPACE_SECTS, dxpl_id, (hsize_t)fspace->hdr->alloc_sect_size)))
                        HGOTO_ERROR(H5E_STORAGE, H5E_NOSPACE, FAIL, "file allocation failed for free space sections")
                } /* end if */
            } /* end else */
        } /* end else */

        /* Allocate space for the buffer to serialize the sections into */
        if(NULL == (sect_buf = H5FL_BLK_MALLOC(sect_block, (size_t)fspace->hdr->sect_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Serialize free sections into buffer available */
        p = sect_buf;

        /* Magic number */
        HDmemcpy(p, H5FS_SECTS_MAGIC, H5FS_SIZEOF_MAGIC);
        p += H5FS_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5FS_SECTS_VERSION;

        /* Metadata status flags */
    /* XXX: Set this? */
        *p++ = 0;

        /* Metadata checksum */
    /* XXX: Set this!  (After all the metadata is in the buffer) */
        HDmemset(p, 0, 4);
        p += 4;

        /* Address of free space header for these sections */
        H5F_addr_encode(f, &p, fspace->addr);

        /* Set up user data for iterator */
        udata.fspace = fspace;
        udata.p = &p;
        udata.sect_cnt_size = MAX(1, (H5V_log2_gen(fspace->hdr->sect_count) + 7) / 8);
#ifdef QAK
HDfprintf(stderr, "%s: udata.sect_cnt_size = %u\n", FUNC, udata.sect_cnt_size);
#endif /* QAK */

        /* Check for whether to serialize a single section */
        if(fspace->single) {
#ifdef QAK
HDfprintf(stderr, "%s: Serializing single section\n", FUNC);
#endif /* QAK */
            /* The number of sections */
            UINT64ENCODE_VAR(p, 1, udata.sect_cnt_size);

            /* The size of the section */
            UINT64ENCODE_VAR(p, fspace->single->size, fspace->sect_len_size);

            /* Serialize the single node */
            if(H5FS_serialize_sect_cb(fspace->single, NULL, &udata) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTSERIALIZE, FAIL, "can't syncronize single section")
        } /* end if */
        else {
            unsigned bin;           /* Current bin we are on */

            /* Iterate over all the bins */
#ifdef QAK
HDfprintf(stderr, "%s: Serializing section bins\n", FUNC);
#endif /* QAK */
            for(bin = 0; bin < fspace->nbins; bin++) {
                /* Check if there are any sections in this bin */
                if(fspace->bins[bin].bin_list) {
                    /* Iterate over list of section size nodes for bin */
                    if(H5SL_iterate(fspace->bins[bin].bin_list, H5FS_serialize_node_cb, &udata) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section size nodes")
                } /* end if */
            } /* end for */
        } /* end else */

        /* Sanity check */
        HDassert((size_t)(p - sect_buf) == fspace->hdr->sect_size);
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_size = %Hu\n", FUNC, fspace->hdr->sect_size);
#endif /* QAK */

        /* Write buffer to disk */
        HDassert(fspace->hdr->sect_size <= fspace->hdr->alloc_sect_size);
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->alloc_sect_size = %Hu\n", FUNC, fspace->hdr->alloc_sect_size);
#endif /* QAK */
        if(H5F_block_write(f, H5FD_MEM_FSPACE_SECTS, fspace->hdr->sect_addr, (size_t)fspace->hdr->sect_size, dxpl_id, sect_buf) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTFLUSH, FAIL, "unable to save free space sections to disk")
    } /* end else */

    /* Mark free space as clean now */
    fspace->dirty = FALSE;

done:
    if(sect_buf)
        H5FL_BLK_FREE(sect_block, sect_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_serialize_bins() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_deserialize_bins
 *
 * Purpose:	Deserialize all bins from disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_deserialize_bins(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace)
{
    haddr_t fs_addr;                    /* Free space header address */
    uint32_t metadata_chksum;           /* Metadata checksum value */
    uint8_t *sect_buf = NULL;           /* Buffer for sections */
    const uint8_t *p;                   /* Pointer into raw data buffer */
    size_t old_sect_size;               /* Section size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_deserialize_bins)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);

    /* Allocate space for the buffer to serialize the sections into */
    old_sect_size = fspace->hdr->sect_size;
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_size = %Hu\n", FUNC, fspace->hdr->sect_size);
#endif /* QAK */
    if(NULL == (sect_buf = H5FL_BLK_MALLOC(sect_block, (size_t)fspace->hdr->sect_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Read buffer from disk */
    if(H5F_block_read(f, H5FD_MEM_FSPACE_SECTS, fspace->hdr->sect_addr, (size_t)fspace->hdr->sect_size, dxpl_id, sect_buf) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_READERROR, FAIL, "can't read free space sections")

    /* Deserialize free sections from buffer available */
    p = sect_buf;

    /* Magic number */
    if(HDmemcmp(p, H5FS_SECTS_MAGIC, H5FS_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, FAIL, "wrong free space sections signature")
    p += H5FS_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5FS_SECTS_VERSION)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, FAIL, "wrong free space sections version")

    /* Metadata flags (unused, currently) */
/* XXX: Plan out metadata flags (including "read-only duplicate" feature) */
    if(*p++ != 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, FAIL, "unknown metadata flag in free space sections")

    /* Metadata checksum (unused, currently) */
    UINT32DECODE(p, metadata_chksum);
/* XXX: Verify checksum */
    if(metadata_chksum != 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, FAIL, "incorrect metadata checksum for free space sections")

    /* Address of free space header for these sections */
    H5F_addr_decode(f, &p, &fs_addr);
    if(H5F_addr_ne(fs_addr, fspace->addr))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, FAIL, "incorrect header address for free space sections")

    /* Check for any serialized sections */
    if(fspace->hdr->sect_count > 0) {
        hsize_t old_sect_count;             /* Section count from header */
        unsigned sect_cnt_size;               /* The size of the section size counts */

        /* Compute the size of the section counts */
        sect_cnt_size = MAX(1, (H5V_log2_gen(fspace->hdr->sect_count) + 7) / 8);
#ifdef QAK
HDfprintf(stderr, "%s: sect_cnt_size = %u\n", FUNC, sect_cnt_size);
HDfprintf(stderr, "%s: fspace->sect_len_size = %u\n", FUNC, fspace->sect_len_size);
#endif /* QAK */

        /* Reset the section count, the "add" routine will update it */
        old_sect_count = fspace->hdr->sect_count;
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_count = %Hu\n", FUNC, fspace->hdr->sect_count);
#endif /* QAK */
        fspace->hdr->sect_count = 0;

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
            UINT64DECODE_VAR(p, sect_size, fspace->sect_len_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect_size = %Hu\n", FUNC, sect_size);
#endif /* QAK */
            HDassert(sect_size);

            /* Loop over nodes of this size */
            for(u = 0; u < node_count; u++) {
                H5FS_section_info_t *new_sect;  /* Section that was deserialized */
                haddr_t sect_addr;      /* Address of free space section in the address space */
                unsigned sect_type;     /* Type of free space section */

                /* The address of the section */
                UINT64DECODE_VAR(p, sect_addr, fspace->sect_off_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect_addr = %a\n", FUNC, sect_addr);
#endif /* QAK */

                /* The type of this section */
                sect_type = *p++;
#ifdef QAK
HDfprintf(stderr, "%s: sect_type = %u\n", FUNC, sect_type);
#endif /* QAK */

                /* Call 'deserialize' callback for this section */
                HDassert(fspace->sect_cls[sect_type].deserialize);
                if((*fspace->sect_cls[sect_type].deserialize)(fspace->sect_cls, p, sect_addr, sect_size, &new_sect) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTDECODE, FAIL, "can't deserialize section")
                HDassert(new_sect);

                /* Update offset in serialization buffer */
                p += new_sect->cls->serial_size;

                /* Insert section in free space manager */
                if(H5FS_add(f, dxpl_id, fspace, new_sect, H5FS_ADD_DESERIALIZING, NULL) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't add section to free space manager")
            } /* end for */
        } while(p < (sect_buf + old_sect_size));

        /* Sanity check */
        HDassert((size_t)(p - sect_buf) == old_sect_size);
        HDassert(old_sect_size == fspace->hdr->sect_size);
        HDassert(old_sect_count == fspace->hdr->sect_count);
    } /* end if */

done:
    if(sect_buf)
        H5FL_BLK_FREE(sect_block, sect_buf);

#ifdef QAK
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif /* QAK */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_deserialize_bins() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_iterate_sect_cb
 *
 * Purpose:	Skip list iterator callback to iterate over free space sections
 *              of a particular size
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_iterate_sect_cb(void *_item, void UNUSED *key, void *_udata)
{
    H5FS_section_info_t *sect_info = (H5FS_section_info_t *)_item;   /* Free space section to work on */
    H5FS_iter_ud3_t *udata = (H5FS_iter_ud3_t *)_udata; /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_iterate_sect_cb)

    /* Check arguments. */
    HDassert(sect_info);
    HDassert(udata->fspace);
    HDassert(udata->op);

    /* Make callback for this section */
    if((*udata->op)(sect_info, udata->op_data) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "iteration callback failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_iterate_sect_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_iterate_node_cb
 *
 * Purpose:	Skip list iterator callback to iterate over free space sections
 *              in a bin
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_iterate_node_cb(void *_item, void UNUSED *key, void *_udata)
{
    H5FS_node_t *fspace_node = (H5FS_node_t *)_item;   /* Free space size node to work on */
    H5FS_iter_ud3_t *udata = (H5FS_iter_ud3_t *)_udata; /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_iterate_node_cb)

    /* Check arguments. */
    HDassert(fspace_node);
    HDassert(udata->fspace);
    HDassert(udata->op);

    /* Iterate through all the sections of this size */
    HDassert(fspace_node->sect_list);
    if(H5SL_iterate(fspace_node->sect_list, H5FS_iterate_sect_cb, udata) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section nodes")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_iterate_node_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_iterate
 *
 * Purpose:	Iterate over all the sections managed
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_iterate(H5FS_t *fspace, H5FS_operator_t op, void *op_data)
{
    H5FS_iter_ud3_t udata;              /* User data for callbacks */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_iterate)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(op);

#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_count = %Hu\n", FUNC, fspace->hdr->sect_count);
#endif /* QAK */

    /* Set up user data for iterator */
    udata.fspace = fspace;
    udata.op = op;
    udata.op_data = op_data;

    /* Iterate over sections, if there are any */
    if(fspace->hdr->sect_count) {
        /* Check for whether to iterate over a single section */
        if(fspace->single) {
#ifdef QAK
HDfprintf(stderr, "%s: 'Iterating' over a single section\n", FUNC);
#endif /* QAK */
            /* "Iterate" over the single node */
            if(H5FS_iterate_sect_cb(fspace->single, NULL, &udata) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't 'iterate' over single section")
        } /* end if */
        else {
            unsigned bin;           /* Current bin we are on */

            /* Iterate over all the bins */
#ifdef QAK
HDfprintf(stderr, "%s: Iterate over section bins\n", FUNC);
#endif /* QAK */
            for(bin = 0; bin < fspace->nbins; bin++) {
                /* Check if there are any sections in this bin */
                if(fspace->bins[bin].bin_list) {
                    /* Iterate over list of section size nodes for bin */
                    if(H5SL_iterate(fspace->bins[bin].bin_list, H5FS_iterate_node_cb, &udata) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section size nodes")
                } /* end if */
            } /* end for */
        } /* end else */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_flush_cb
 *
 * Purpose:	Skip list iterator callback to syncronize free space sections
 *              in a free space manager with their serialized form for the
 *              metadata cache
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_flush_cb(void *_item, void UNUSED *key, void *_udata)
{
    H5FS_t *fspace = (H5FS_t *)_item;   /* Free space manager to syncronize */
    H5FS_iter_ud1_t *udata = (H5FS_iter_ud1_t *)_udata; /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5FS_flush_cb, FAIL)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(udata->f);

    /* Serialize the bins for this free space manager, if they are dirty */
    if(fspace->dirty)
        if(H5FS_serialize_bins(udata->f, udata->dxpl_id, fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSERIALIZE, FAIL, "can't syncronize bins")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_flush_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_flush
 *
 * Purpose:	Syncronize free space sections in all the free space
 *              managers with their serialized form for the metadata cache
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_flush(H5F_t *f, hid_t dxpl_id, unsigned UNUSED flags)
{
    H5FS_iter_ud1_t udata;              /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5FS_flush, FAIL)

    /* Check arguments. */
    HDassert(f);

    /* Set up callback information */
    udata.f = f;
    udata.dxpl_id = dxpl_id;

    /* Iterate over open free space managers, to syncronize their section information */
    HDassert(H5FS_open_g);
    if(H5SL_iterate(H5FS_open_g, H5FS_flush_cb, &udata) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't syncronize section info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_close
 *
 * Purpose:	Destroy & deallocate free list structure, serializing sections
 *              in the bins
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_close(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace)
{
    unsigned u;                 /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5FS_close, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);

    /* Remove the free space manager from the list of open free space managers */
    if(H5FS_open_remove(fspace) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove free space header from open list")

    /* Serialize the sections in the bins, if necessary */
    if(fspace->dirty)
        if(H5FS_serialize_bins(f, dxpl_id, fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSERIALIZE, FAIL, "can't syncronize bins")

    /* Check for single section to free */
    if(fspace->single) {
        fspace->single->cls->free(fspace->single);
        fspace->single = NULL;
    } /* end if */
    HDassert(fspace->single == NULL);

    /* Release bins for skip lists */
    if(fspace->bins) {
        /* Clear out lists of nodes */
        for(u = 0; u < fspace->nbins; u++)
            if(fspace->bins[u].bin_list) {
                H5SL_destroy(fspace->bins[u].bin_list, H5FS_node_free_cb, NULL);
                fspace->bins[u].bin_list = NULL;
            } /* end if */

        H5FL_SEQ_FREE(H5FS_bin_t, fspace->bins);
    } /* end if */

    /* Release skip list for merging sections */
    if(fspace->merge_list) {
        if(H5SL_close(fspace->merge_list) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCLOSEOBJ, FAIL, "can't destroy section merging skip list")
    } /* end if */

    /* Unpin the free space header in the cache */
    if(H5AC_unpin_entry(f, fspace->hdr) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPIN, FAIL, "unable to unpin free space header")

    /* Free free space info */
    H5FL_FREE(H5FS_t, fspace);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_close() */

