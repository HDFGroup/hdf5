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

/*
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, May  2, 2006
 *
 * Purpose:	Free space tracking functions.
 *
 * Note:	(Used to be in the H5HFflist.c file, prior to the date above)
 *
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
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5AC2private.h"       /* Metadata cache                       */

/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Section info routines */
static herr_t H5FS_sinfo_free_sect_cb(void *item, void *key, void *op_data);
static herr_t H5FS_sinfo_free_node_cb(void *item, void *key, void *op_data);


/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5FS_section_class_t sequence information */
H5FL_SEQ_DEFINE(H5FS_section_class_t);

/* Declare a free list to manage the H5FS_t struct */
H5FL_DEFINE(H5FS_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



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
    size_t nclasses, const H5FS_section_class_t *classes[], void *cls_init_udata)
{
    H5FS_t *fspace = NULL;      /* New free space structure */
    H5FS_t *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5FS_create, NULL)
#ifdef QAK
HDfprintf(stderr, "%s: Creating free space manager, nclasses = %Zu\n", FUNC, nclasses);
#endif /* QAK */

    /* Check arguments. */
    HDassert(fs_addr);
    HDassert(fs_create->shrink_percent);
    HDassert(fs_create->shrink_percent < fs_create->expand_percent);
    HDassert(fs_create->max_sect_size);
    HDassert(nclasses == 0 || classes);

    /*
     * Allocate free space structure
     */
    if(NULL == (fspace = H5FS_new(nclasses, classes, cls_init_udata)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for free space free list")

    /* Allocate space for the free space header */
    if(HADDR_UNDEF == (fspace->addr = H5MF_alloc(f, H5FD_MEM_FSPACE_HDR, dxpl_id, (hsize_t)H5FS_HEADER_SIZE(f))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "file allocation failed for free space header")
    *fs_addr = fspace->addr;

    /* Initialize creation information for free space manager */
    fspace->client = fs_create->client;
    fspace->shrink_percent = fs_create->shrink_percent;
    fspace->expand_percent = fs_create->expand_percent;
    fspace->max_sect_addr = fs_create->max_sect_addr;
    fspace->max_sect_size = fs_create->max_sect_size;

    /* Cache the new free space header (pinned) */
    if(H5AC2_set(f, dxpl_id, H5AC2_FSPACE_HDR, fspace->addr, H5FS_HEADER_SIZE(f), fspace, H5AC2__PIN_ENTRY_FLAG) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, NULL, "can't add free space header to cache")

    /* Set the return value */
    ret_value = fspace;

done:
    if(!ret_value && fspace)
        (void)H5FS_cache_hdr_dest(fspace);

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
    const H5FS_section_class_t *classes[], void *cls_init_udata)
{
    H5FS_t *fspace = NULL;      /* New free space structure */
    H5FS_prot_t fs_prot;        /* Information for protecting free space manager */
    unsigned fspace_status = 0; /* Free space header's status in the metadata cache */
    H5FS_t *ret_value;          /* Return value */
    H5FS_hdr_cache_ud_t cache_udata; /* User-data for metadata cache callback */

    FUNC_ENTER_NOAPI(H5FS_open, NULL)
#ifdef QAK
HDfprintf(stderr, "%s: Opening free space manager, nclasses = %Zu\n", FUNC, nclasses);
#endif /* QAK */

    /* Check arguments. */
    HDassert(H5F_addr_defined(fs_addr));
    HDassert(nclasses);
    HDassert(classes);

    /* Initialize user data for protecting the free space manager */
    fs_prot.nclasses = nclasses;
    fs_prot.classes = classes;
    fs_prot.cls_init_udata = cls_init_udata;
    cache_udata.fs_prot = &fs_prot;
    cache_udata.f = f;

    /* Protect the free space header */
    if(NULL == (fspace = H5AC2_protect(f, dxpl_id, H5AC2_FSPACE_HDR, fs_addr, H5FS_HEADER_SIZE(f), &cache_udata, H5AC2_READ)))
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTPROTECT, NULL, "unable to load free space header")
#ifdef QAK
HDfprintf(stderr, "%s: fspace->sect_addr = %a\n", FUNC, fspace->sect_addr);
HDfprintf(stderr, "%s: fspace->sect_size = %Hu\n", FUNC, fspace->sect_size);
HDfprintf(stderr, "%s: fspace->alloc_sect_size = %Hu\n", FUNC, fspace->alloc_sect_size);
HDfprintf(stderr, "%s: fspace->sinfo = %p\n", FUNC, fspace->sinfo);
#endif /* QAK */

    /* Check the free space header's status in the metadata cache */
    if(H5AC2_get_entry_status(f, fs_addr, &fspace_status) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, NULL, "unable to check metadata cache status for free space header")

    /* If the free space header isn't already pinned, pin it now */
    /* (could still be pinned from it's section info still hanging around in the cache) */
    if(!(fspace_status & H5AC2_ES__IS_PINNED)) {
        /* Pin free space header in the cache */
        if(H5AC2_pin_protected_entry(fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTPIN, NULL, "unable to pin free space header")
    } /* end if */

    /* Unlock free space header, now pinned */
    if(H5AC2_unprotect(f, dxpl_id, H5AC2_FSPACE_HDR, fs_addr, (size_t)0, fspace, H5AC2__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPROTECT, NULL, "unable to release free space header")

    /* Set return value */
    ret_value = fspace;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_open() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_delete
 *
 * Purpose:	Delete a free space manager on disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 30, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_delete(H5F_t *f, hid_t dxpl_id, haddr_t fs_addr)
{
    H5FS_t *fspace = NULL;              /* Free space header loaded from file */
    H5FS_prot_t fs_prot;                /* Temporary information for protecting free space header */
    herr_t ret_value = SUCCEED;         /* Return value */
    H5FS_hdr_cache_ud_t cache_udata; /* User-data for metadata cache callback */

    FUNC_ENTER_NOAPI(H5FS_delete, FAIL)
#ifdef QAK
HDfprintf(stderr, "%s: Deleting free space manager\n", FUNC);
#endif /* QAK */

    /* Check arguments. */
    HDassert(f);
    HDassert(H5F_addr_defined(fs_addr));

    /* Initialize user data for protecting the free space manager */
    /* (no class information necessary for delete) */
    fs_prot.nclasses = 0;
    fs_prot.classes = NULL;
    fs_prot.cls_init_udata = NULL;
    cache_udata.fs_prot = &fs_prot;
    cache_udata.f = f;

    /* Protect the free space header */
    if(NULL == (fspace = H5AC2_protect(f, dxpl_id, H5AC2_FSPACE_HDR, fs_addr, H5FS_HEADER_SIZE(f), &cache_udata, H5AC2_WRITE)))
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTPROTECT, FAIL, "unable to protect free space header")

    /* Delete serialized section storage, if there are any */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->sect_addr = %a\n", FUNC, fspace->sect_addr);
#endif /* QAK */
    if(fspace->serial_sect_count > 0) {
        unsigned sinfo_status = 0;      /* Free space section info's status in the metadata cache */

        /* Sanity check */
        HDassert(H5F_addr_defined(fspace->sect_addr));
        HDassert(fspace->sect_size > 0);

        /* Check the free space section info's status in the metadata cache */
        if(H5AC2_get_entry_status(f, fspace->sect_addr, &sinfo_status) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to check metadata cache status for free space section info")

        /* If the free space section info is in the cache, expunge it now */
        if(sinfo_status & H5AC2_ES__IN_CACHE) {
            /* Sanity checks on direct block */
            HDassert(!(sinfo_status & H5AC2_ES__IS_PINNED));
            HDassert(!(sinfo_status & H5AC2_ES__IS_PROTECTED));

#ifdef QAK
HDfprintf(stderr, "%s: Expunging free space section info from cache\n", FUNC);
#endif /* QAK */
            /* Evict the free space section info from the metadata cache */
            if(H5AC2_expunge_entry(f, dxpl_id, H5AC2_FSPACE_SINFO, fspace->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTREMOVE, FAIL, "unable to remove free space section info from cache")
#ifdef QAK
HDfprintf(stderr, "%s: Done expunging free space section info from cache\n", FUNC);
#endif /* QAK */
        } /* end if */

        /* Release the space in the file */
        if(H5MF_xfree(f, H5FD_MEM_FSPACE_SINFO, dxpl_id, fspace->sect_addr, fspace->alloc_sect_size) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to release free space sections")
    } /* end if */

    /* Release header's disk space */
    if(H5MF_xfree(f, H5FD_MEM_FSPACE_HDR, dxpl_id, fs_addr, (hsize_t)H5FS_HEADER_SIZE(f))<0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to release free space header")

    /* Release the free space header */
    if(H5AC2_unprotect(f, dxpl_id, H5AC2_FSPACE_HDR, fs_addr, (size_t)0, fspace, H5AC2__DELETED_FLAG) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPROTECT, FAIL, "unable to release free space header")
    fspace = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_delete() */


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
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5FS_close, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
#ifdef QAK
HDfprintf(stderr, "%s: Entering, fspace = %p, fspace->sinfo = %p\n", FUNC, fspace, fspace->sinfo);
#endif /* QAK */

    /* Check if section info is valid */
    if(fspace->sinfo) {
        HDassert(H5F_addr_defined(fspace->sect_addr));

        /* Unpin the free space section info in the cache */
        if(H5AC2_unpin_entry(fspace->sinfo) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPIN, FAIL, "unable to unpin free space section info")

        /* If there aren't any sections being managed, free the space for the sections */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->tot_sect_count = %Hu\n", FUNC, fspace->tot_sect_count);
#endif /* QAK */
        if(fspace->tot_sect_count == 0) {
            haddr_t old_addr;           /* Old section info address */

            HDassert(fspace->serial_sect_count == 0);
            HDassert(fspace->ghost_sect_count == 0);

            /* Free previous serialized sections disk space */
            old_addr = fspace->sect_addr;
            if(H5MF_xfree(f, H5FD_MEM_FSPACE_SINFO, dxpl_id, old_addr, fspace->alloc_sect_size) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to release free space sections")

            /* Reset section info */
            fspace->sect_addr = HADDR_UNDEF;
            fspace->alloc_sect_size = fspace->sect_size = 0;

            /* Mark free space header as dirty */
            if(H5AC2_mark_pinned_or_protected_entry_dirty(fspace) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space header as dirty")

            /* Evict the section info from the metadata cache */
            if(H5AC2_expunge_entry(f, dxpl_id, H5AC2_FSPACE_SINFO, old_addr) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "unable to remove free space section info from cache")
        } /* end if */
    } /* end if */
    else {
        unsigned sect_status = 0;       /* Free space section's status in the metadata cache */

        /* Check if we've allocated any section info in the file & if it's still in the cache */
        if(H5F_addr_defined(fspace->sect_addr) && H5AC2_get_entry_status(f, fspace->sect_addr, &sect_status) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "unable to check metadata cache status for free space header")

        /* If this free list header's section info exists and is still in the
         *      cache, don't unpin the header - let the section info do it,
         *      when the section info is evicted from the cache. -QAK
         */
        if(!(sect_status & H5AC2_ES__IN_CACHE)) {
            /* Unpin the free space header in the cache */
            /* (the section info destructor would unpin it if the section info existed) */
            if(H5AC2_unpin_entry(fspace) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPIN, FAIL, "unable to unpin free space header")
        } /* end if */
    } /* end else */

    /* Reset the header's pointer to the section info, so it will get pinned again
     *  if the free space header is still in the metadata cache when the free
     *  space manager is re-opened.
     */
    fspace->sinfo = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_close() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_new
 *
 * Purpose:	Create new free space manager structure
 *
 * Return:	Success:	non-NULL, pointer to new free space manager struct
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 31, 2006
 *
 *-------------------------------------------------------------------------
 */
H5FS_t *
H5FS_new(size_t nclasses, const H5FS_section_class_t *classes[],
    void *cls_init_udata)
{
    H5FS_t *fspace;             /* Free space manager */
    size_t u;                   /* Local index variable */
    H5FS_t *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_new)

    /* Check arguments. */
    HDassert(nclasses == 0 || (nclasses > 0 && classes));

    /*
     * Allocate free space structure
     */
    if(NULL == (fspace = H5FL_CALLOC(H5FS_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for free space free list")

    /* Set immutable free list parameters */
    fspace->nclasses = nclasses;
    if(nclasses > 0) {
        if(NULL == (fspace->sect_cls = H5FL_SEQ_MALLOC(H5FS_section_class_t, nclasses)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for free space section class array")

        /* Initialize the section classes for this free space list */
        for(u = 0; u < nclasses; u++) {
            /* Make certain that section class type can be used as an array index into this array */
            HDassert(u == classes[u]->type);

            /* Copy the class information into the free space manager */
            HDmemcpy(&fspace->sect_cls[u], classes[u], sizeof(H5FS_section_class_t));

            /* Call the class initialization routine, if there is one */
            if(fspace->sect_cls[u].init_cls)
                if((fspace->sect_cls[u].init_cls)(&fspace->sect_cls[u], cls_init_udata) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "unable to initialize section class")
        } /* end for */
    } /* end if */

    /* Initialize non-zero information for new free space manager */
    fspace->addr = HADDR_UNDEF;
    fspace->sect_addr = HADDR_UNDEF;

    /* Set return value */
    ret_value = fspace;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_new() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_size
 *
 * Purpose:     Collect meta storage info used by the free space manager
 *
 * Return:      Success:        non-negative 
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi
 *              June 19, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_size(const H5F_t *f, const H5FS_t *fspace, hsize_t *meta_size)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5FS_size)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(fspace);
    HDassert(meta_size);

    /* Get the free space size info */
    *meta_size += H5FS_HEADER_SIZE(f) + fspace->alloc_sect_size;
 
    FUNC_LEAVE_NOAPI(SUCCEED) 
} /* end H5FS_size() */


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
H5FS_cache_hdr_dest(H5FS_t *fspace)
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
H5FS_cache_sinfo_dest(H5FS_sinfo_t *sinfo)
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
    if(H5AC2_unpin_entry(sinfo->fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPIN, FAIL, "unable to unpin free space header")

    /* Release free space section info */
    H5FL_FREE(H5FS_sinfo_t, sinfo);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_sinfo_dest() */

#ifdef H5FS_DEBUG

/*-------------------------------------------------------------------------
 * Function:	H5FS_assert
 *
 * Purpose:	Verify that the free space manager is mostly sane
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jul 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_assert(const H5FS_t *fspace)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_assert)
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->tot_sect_count = %Hu\n", "H5FS_assert", fspace->hdr->tot_sect_count);
#endif /* QAK */

    /* Sanity check sections */
    H5FS_sect_assert(fspace);

    /* General assumptions about the section size counts */
    HDassert(fspace->sinfo->tot_size_count >= fspace->sinfo->serial_size_count);
    HDassert(fspace->sinfo->tot_size_count >= fspace->sinfo->ghost_size_count);

    /* General assumptions about the section counts */
    HDassert(fspace->tot_sect_count >= fspace->serial_sect_count);
    HDassert(fspace->tot_sect_count >= fspace->ghost_sect_count);
    HDassert(fspace->tot_sect_count == (fspace->serial_sect_count + fspace->ghost_sect_count));
#ifdef QAK
    HDassert(fspace->serial_sect_count > 0 || fspace->ghost_sect_count == 0);
#endif /* QAK */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FS_assert() */
#endif /* H5FS_DEBUG */

