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

/*
 * Programmer:	Quincey Koziol <koziol@hdfgroup.org>
 *              Monday, July 31, 2006
 *
 * Purpose:	Free space tracking functions.
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
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/

/* Default starting size of section buffer */
#define H5FS_SINFO_SIZE_DEFAULT  64

/* Max. height of the skip list holding free list nodes */
#define H5FS_DEFAULT_SKIPLIST_HEIGHT     16


/******************/
/* Local Typedefs */
/******************/

/* User data for skip list iterator callback for iterating over section size nodes */
typedef struct {
    H5FS_t *fspace;             /* Free space manager info */
    H5FS_operator_t op;         /* Operator for the iteration */
    void *op_data;              /* Information to pass to the operator */
} H5FS_iter_ud_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static herr_t H5FS_sect_increase(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    const H5FS_section_class_t *cls, unsigned flags);
static herr_t H5FS_sect_decrease(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    const H5FS_section_class_t *cls);
static herr_t H5FS_size_node_decr(H5FS_sinfo_t *sinfo, unsigned bin, H5FS_node_t *fspace_node,
    const H5FS_section_class_t *cls);
static herr_t H5FS_sect_unlink_size(H5FS_sinfo_t *sinfo, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect);
static herr_t H5FS_sect_unlink_rest(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    const H5FS_section_class_t *cls, H5FS_section_info_t *sect);
static herr_t H5FS_sect_link_size(H5FS_sinfo_t *sinfo, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect);
static herr_t H5FS_sect_link_rest(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    const H5FS_section_class_t *cls, H5FS_section_info_t *sect, unsigned flags);
static herr_t H5FS_sect_link(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    H5FS_section_info_t *sect, unsigned flags);
static herr_t H5FS_sect_merge(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    H5FS_section_info_t **sect, void *op_data);
static htri_t H5FS_sect_find_node(H5FS_t *fspace, hsize_t request, H5FS_section_info_t **node);
static herr_t H5FS_sect_serialize_size(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace);


/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5FS_node_t struct */
H5FL_DEFINE(H5FS_node_t);

/* Declare a free list to manage the H5FS_bin_t sequence information */
H5FL_SEQ_DEFINE(H5FS_bin_t);

/* Declare a free list to manage the H5FS_sinfo_t struct */
H5FL_DEFINE(H5FS_sinfo_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5FS_sinfo_new
 *
 * Purpose:	Create new section info structure
 *
 * Return:	Success:	non-NULL, pointer to new section info struct
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 31, 2006
 *
 *-------------------------------------------------------------------------
 */
H5FS_sinfo_t *
H5FS_sinfo_new(H5F_t *f, H5FS_t *fspace)
{
    H5FS_sinfo_t *sinfo = NULL; /* Section information struct created */
    H5FS_sinfo_t *ret_value;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sinfo_new)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);

    /* Allocate the free space header */
    if(NULL == (sinfo = H5FL_CALLOC(H5FS_sinfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set non-zero values */
    sinfo->nbins = H5V_log2_gen(fspace->max_sect_size);
    sinfo->sect_prefix_size = H5FS_SINFO_PREFIX_SIZE(f);
    sinfo->sect_off_size = (fspace->max_sect_addr + 7) / 8;
    sinfo->sect_len_size = (H5V_log2_gen(fspace->max_sect_size) + 7) / 8;
    sinfo->fspace = fspace;
#ifdef QAK
HDfprintf(stderr, "%s: sinfo->nbins = %u\n", FUNC, sinfo->nbins);
HDfprintf(stderr, "%s: sinfo->sect_off_size = %u, sinfo->sect_len_size = %u\n", FUNC, sinfo->sect_off_size, sinfo->sect_len_size);
#endif /* QAK */

    /* Allocate space for the section size bins */
    if(NULL == (sinfo->bins = H5FL_SEQ_CALLOC(H5FS_bin_t, (size_t)sinfo->nbins)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for free space section bin array")

    /* Set return value */
    ret_value = sinfo;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sinfo_new() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sinfo_pin
 *
 * Purpose:	Pin the section info for the free space manager in memory
 *              Either loads section info from disk, or creates new section info
 *
 * Return:	Success:	non-NULL, pointer to section info struct
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 31, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_sinfo_t *
H5FS_sinfo_pin(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace)
{
    H5FS_sinfo_t *sinfo;        /* Section information struct created */
    H5FS_sinfo_t *ret_value;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sinfo_pin)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);

    /* Create new section info, if it doesn't exist yet */
    if(!H5F_addr_defined(fspace->sect_addr)) {
        /* Sanity check */
        HDassert(fspace->tot_sect_count == 0);
        HDassert(fspace->serial_sect_count == 0);
        HDassert(fspace->ghost_sect_count == 0);

        /* Allocate and initialize free space section info */
        if(NULL == (sinfo = H5FS_sinfo_new(f, fspace)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, NULL, "can't create section info")

        /* Allocate space for the section info */
        fspace->sect_size = H5FS_SINFO_SIZE_DEFAULT;
        fspace->alloc_sect_size = (size_t)fspace->sect_size;
        if(HADDR_UNDEF == (fspace->sect_addr = H5MF_alloc(f, H5FD_MEM_FSPACE_SINFO, dxpl_id, fspace->alloc_sect_size)))
            HGOTO_ERROR(H5E_STORAGE, H5E_NOSPACE, NULL, "file allocation failed for free space sections")

        /* Cache the new free space section info (pinned) */
        if(H5AC_set(f, dxpl_id, H5AC_FSPACE_SINFO, fspace->sect_addr, sinfo, H5AC__PIN_ENTRY_FLAG) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, NULL, "can't add free space sections to cache")

        /* Mark free space header as dirty */
        if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, NULL, "unable to mark free space header as dirty")
    } /* end if */
    else {
#ifdef QAK
HDfprintf(stderr, "%s: Reading in existing sections, fspace->sect_addr = %a\n", FUNC, fspace->sect_addr);
#endif /* QAK */
        /* Protect the free space sections */
        if(NULL == (sinfo = H5AC_protect(f, dxpl_id, H5AC_FSPACE_SINFO, fspace->sect_addr, NULL, fspace, H5AC_WRITE)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTPROTECT, NULL, "unable to load free space sections")

        /* Pin them in the cache */
        if(H5AC_pin_protected_entry(f, sinfo) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTPIN, NULL, "unable to pin free space sections")

        /* Unlock free space sections, now pinned */
        if(H5AC_unprotect(f, dxpl_id, H5AC_FSPACE_SINFO, fspace->sect_addr, sinfo, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPROTECT, NULL, "unable to release free space sections")
    } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: sinfo->serial_size_count = %Zu\n", FUNC, sinfo->serial_size_count);
#endif /* QAK */

    /* Update pointer to free space header for section info */
    sinfo->fspace = fspace;

    /* Set return value */
    ret_value = sinfo;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sinfo_pin() */


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
H5FS_sect_increase(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    const H5FS_section_class_t *cls, unsigned flags)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_increase)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(cls);

    /* Increment total # of sections on free space list */
    fspace->tot_sect_count++;

    /* Check for serializable or 'ghost' section */
    if(cls->flags & H5FS_CLS_GHOST_OBJ) {
        /* Sanity check */
        HDassert(cls->serial_size == 0);

        /* Increment # of ghost sections */
        fspace->ghost_sect_count++;
    } /* end if */
    else {
        /* Increment # of serializable sections */
        fspace->serial_sect_count++;

        /* Increment amount of space required to serialize all sections */
#ifdef QAK
HDfprintf(stderr, "%s: sinfo->serial_size = %Zu\n", FUNC, sinfo->serial_size);
HDfprintf(stderr, "%s: cls->serial_size = %Zu\n", FUNC, cls->serial_size);
#endif /* QAK */
        fspace->sinfo->serial_size += cls->serial_size;

        /* Update the free space sections' serialized size */
        /* (if we're not deserializing the sections from disk) */
        if(!(flags & H5FS_ADD_DESERIALIZING)) {
            if(H5FS_sect_serialize_size(f, dxpl_id, fspace) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTCOMPUTE, FAIL, "can't adjust free space section size on disk")
        } /* end if */
    } /* end else */

    /* Mark free space header as dirty */
    if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space header as dirty")

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
H5FS_sect_decrease(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, const H5FS_section_class_t *cls)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_decrease)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(cls);

    /* Decrement total # of sections in free space manager */
    fspace->tot_sect_count--;

    /* Check for serializable or 'ghost' section */
    if(cls->flags & H5FS_CLS_GHOST_OBJ) {
        /* Sanity check */
        HDassert(cls->serial_size == 0);

        /* Decrement # of ghost sections */
        fspace->ghost_sect_count--;
    } /* end if */
    else {
        /* Decrement # of serializable sections */
        fspace->serial_sect_count--;

        /* Decrement amount of space required to serialize all sections */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->serial_size = %Zu\n", FUNC, fspace->serial_size);
HDfprintf(stderr, "%s: cls->serial_size = %Zu\n", FUNC, cls->serial_size);
#endif /* QAK */
        fspace->sinfo->serial_size -= cls->serial_size;

        /* Update the free space sections' serialized size */
        if(H5FS_sect_serialize_size(f, dxpl_id, fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCOMPUTE, FAIL, "can't adjust free space section size on disk")
    } /* end else */

    /* Mark free space header as dirty */
    if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_decrease() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_size_node_decr
 *
 * Purpose:	Decrement the number of sections of a particular size
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_size_node_decr(H5FS_sinfo_t *sinfo, unsigned bin, H5FS_node_t *fspace_node,
    const H5FS_section_class_t *cls)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_size_node_decr)

    /* Check arguments. */
    HDassert(sinfo);
    HDassert(fspace_node);
    HDassert(cls);

    /* Decrement the # of sections in this bin */
    /* (Different from the # of items in the bin's skiplist, since each node on
     *  the bin's skiplist is also a skiplist...)
     */
    sinfo->bins[bin].tot_sect_count--;
#ifdef QAK
HDfprintf(stderr, "%s: sinfo->bins[%u].sect_count = %Zu\n", FUNC, bin, sinfo->bins[bin].sect_count);
#endif /* QAK */

    /* Check for 'ghost' or 'serializable' section */
    if(cls->flags & H5FS_CLS_GHOST_OBJ) {
        /* Decrement node's ghost section count */
        fspace_node->ghost_count--;

        /* Decrement bin's ghost section count */
        sinfo->bins[bin].ghost_sect_count--;

        /* If the node has no more ghost sections, decrement number of ghost section sizes managed */
        if(fspace_node->ghost_count == 0)
            sinfo->ghost_size_count--;
    } /* end if */
    else {
        /* Decrement node's serializable section count */
        fspace_node->serial_count--;

        /* Decrement bin's serializable section count */
        sinfo->bins[bin].serial_sect_count--;

        /* If the node has no more serializable sections, decrement number of serializable section sizes managed */
        if(fspace_node->serial_count == 0)
            sinfo->serial_size_count--;
    } /* end else */

    /* Check for no more nodes on list of that size */
    if(H5SL_count(fspace_node->sect_list) == 0) {
        H5FS_node_t *tmp_fspace_node;       /* Free space list size node */

        /* Sanity checks */
        HDassert(fspace_node->ghost_count == 0);
        HDassert(fspace_node->serial_count == 0);

        /* Remove size tracking list from bin */
        tmp_fspace_node = H5SL_remove(sinfo->bins[bin].bin_list, &fspace_node->sect_size);
        if(tmp_fspace_node == NULL || tmp_fspace_node != fspace_node)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space node from skip list")

        /* Destroy skip list for size tracking node */
        if(H5SL_close(fspace_node->sect_list) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCLOSEOBJ, FAIL, "can't destroy size tracking node's skip list")

        /* Release free space list node */
        H5FL_FREE(H5FS_node_t, fspace_node);

        /* Decrement total number of section sizes managed */
        sinfo->tot_size_count--;
    } /* end if */

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
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_unlink_size(H5FS_sinfo_t *sinfo, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect)
{
    H5FS_node_t *fspace_node;       /* Free list size node */
    H5FS_section_info_t *tmp_sect_node; /* Temporary section node */
    unsigned bin;                   /* Bin to put the free space section in */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_unlink_size)

    /* Check arguments. */
    HDassert(sinfo);
    HDassert(sinfo->bins);
    HDassert(sect);
    HDassert(cls);

    /* Determine correct bin which holds items of at least the section's size */
    bin = H5V_log2_gen(sect->size);
    HDassert(bin < sinfo->nbins);
    if(sinfo->bins[bin].bin_list == NULL)
        HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "node's bin is empty?")

    /* Find space node for section's size */
    if((fspace_node = H5SL_search(sinfo->bins[bin].bin_list, &sect->size)) == NULL)
        HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section size node")

    /* Remove the section's node from the list */
    tmp_sect_node = H5SL_remove(fspace_node->sect_list, &sect->addr);
    if(tmp_sect_node == NULL || tmp_sect_node != sect)
        HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section node on size list")

    /* Decrement # of sections in section size node */
    if(H5FS_size_node_decr(sinfo, bin, fspace_node, cls) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space size node from skip list")

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
H5FS_sect_unlink_rest(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    const H5FS_section_class_t *cls, H5FS_section_info_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_unlink_rest)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(cls);
    HDassert(sect);

    /* Remove node from merge list, if it was entered there */
    if(!(cls->flags & H5FS_CLS_SEPAR_OBJ)) {
        H5FS_section_info_t *tmp_sect_node; /* Temporary section node */

#ifdef QAK
HDfprintf(stderr, "%s: removing object from merge list, sect->type = %u\n", FUNC, (unsigned)sect->type);
#endif /* QAK */
        tmp_sect_node = H5SL_remove(fspace->sinfo->merge_list, &sect->addr);
        if(tmp_sect_node == NULL || tmp_sect_node != sect)
            HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section node on size list")
    } /* end if */

    /* Update section info & check if we need less room for the serialized free space sections */
    if(H5FS_sect_decrease(f, dxpl_id, fspace, cls) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't increase free space section size on disk")

    /* Decrement amount of free space managed */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->tot_space = %Hu\n", FUNC, fspace->tot_space);
#endif /* QAK */
    fspace->tot_space -= sect->size;

    /* Mark free space sections as changed */
    if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace->sinfo) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space sections as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_unlink_rest() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_remove
 *
 * Purpose:	Remove a section from the free space manager
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_sect_remove(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    H5FS_section_info_t *sect)
{
    const H5FS_section_class_t *cls;    /* Class of section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_remove)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(sect);

    /* Get section's class */
    cls = &fspace->sect_cls[sect->type];

    /* Remove node from size tracked data structures */
    if(H5FS_sect_unlink_size(fspace->sinfo, cls, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from size tracking data structures")

    /* Update rest of free space manager data structures for node removal */
    if(H5FS_sect_unlink_rest(f, dxpl_id, fspace, cls, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from non-size tracking data structures")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_link_size
 *
 * Purpose:	Add a section of free space to the free list bins
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_sect_link_size(H5FS_sinfo_t *sinfo, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect)
{
    H5FS_node_t *fspace_node = NULL;     /* Pointer to free space node of the correct size */
    unsigned bin;                       /* Bin to put the free space section in */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_link_size)
#ifdef QAK
HDfprintf(stderr, "%s: sect->size = %Hu, sect->addr = %a\n", FUNC, sect->size, sect->addr);
#endif /* QAK */

    /* Check arguments. */
    HDassert(sinfo);
    HDassert(sect);
    HDassert(H5F_addr_defined(sect->addr));
    HDassert(sect->size);

    /* Determine correct bin which holds items of the section's size */
    bin = H5V_log2_gen(sect->size);
    HDassert(bin < sinfo->nbins);
    if(sinfo->bins[bin].bin_list == NULL) {
        if(NULL == (sinfo->bins[bin].bin_list = H5SL_create(H5SL_TYPE_HSIZE, 0.5, (size_t)H5FS_DEFAULT_SKIPLIST_HEIGHT)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for free space nodes")
    } /* end if */
    else {
        /* Check for node list of the correct size already */
        fspace_node = H5SL_search(sinfo->bins[bin].bin_list, &sect->size);
    } /* end else */

    /* Check if we need to create a new skip list for nodes of this size */
    if(fspace_node == NULL) {
        /* Allocate new free list size node */
        if(NULL == (fspace_node = H5FL_MALLOC(H5FS_node_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for free space node")

        /* Initialize the free list size node */
        fspace_node->sect_size = sect->size;
        fspace_node->serial_count = fspace_node->ghost_count = 0;
        if(NULL == (fspace_node->sect_list = H5SL_create(H5SL_TYPE_HADDR, 0.5, (size_t)H5FS_DEFAULT_SKIPLIST_HEIGHT)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for free space nodes")

        /* Insert new free space size node into bin's list */
        if(H5SL_insert(sinfo->bins[bin].bin_list, fspace_node, &fspace_node->sect_size) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into skip list")

        /* Increment number of section sizes */
        sinfo->tot_size_count++;
    } /* end if */

    /* Increment # of section in bin */
    /* (Different from the # of items in the bin's skiplist, since each node on
     *  the bin's skiplist is also a skiplist...)
     */
#ifdef QAK
HDfprintf(stderr, "%s: sinfo->bins[%u].sect_count = %Zu\n", FUNC, bin, sinfo->bins[bin].sect_count);
#endif /* QAK */
    sinfo->bins[bin].tot_sect_count++;
    if(cls->flags & H5FS_CLS_GHOST_OBJ) {
        sinfo->bins[bin].ghost_sect_count++;
        fspace_node->ghost_count++;

        /* Check for first ghost section in node */
        if(fspace_node->ghost_count == 1)
            sinfo->ghost_size_count++;
    } /* end if */
    else {
        sinfo->bins[bin].serial_sect_count++;
        fspace_node->serial_count++;

        /* Check for first serializable section in node */
        if(fspace_node->serial_count == 1)
            sinfo->serial_size_count++;
    } /* end else */

    /* Insert free space node into correct skip list */
    if(H5SL_insert(fspace_node->sect_list, sect, &sect->addr) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into skip list")

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
H5FS_sect_link_rest(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect, unsigned flags)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_link_rest)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(sect);

    /* Add section to the address-ordered list of sections, if allowed */
    if(!(cls->flags & H5FS_CLS_SEPAR_OBJ)) {
#ifdef QAK
HDfprintf(stderr, "%s: inserting object into merge list, sect->type = %u\n", FUNC, (unsigned)sect->type);
#endif /* QAK */
        if(fspace->sinfo->merge_list == NULL)
            if(NULL == (fspace->sinfo->merge_list = H5SL_create(H5SL_TYPE_HADDR, 0.5, (size_t)H5FS_DEFAULT_SKIPLIST_HEIGHT)))
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for merging free space sections")
        if(H5SL_insert(fspace->sinfo->merge_list, sect, &sect->addr) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into merging skip list")
    } /* end if */

    /* Update section info & check if we need more room for the serialized free space sections */
    if(H5FS_sect_increase(f, dxpl_id, fspace, cls, flags) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't increase free space section size on disk")

    /* Increment amount of free space managed */
    fspace->tot_space += sect->size;

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
H5FS_sect_link(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    H5FS_section_info_t *sect, unsigned flags)
{
    const H5FS_section_class_t *cls;    /* Class of section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_link)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(sect);

    /* Get section's class */
    cls = &fspace->sect_cls[sect->type];

    /* Add section to size tracked data structures */
#ifdef QAK
HDfprintf(stderr, "%s: Check 1.0 - fspace->tot_space = %Hu\n", FUNC, fspace->tot_space);
#endif /* QAK */
    if(H5FS_sect_link_size(fspace->sinfo, cls, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't add section to size tracking data structures")

#ifdef QAK
HDfprintf(stderr, "%s: Check 2.0 - fspace->tot_space = %Hu\n", FUNC, fspace->tot_space);
#endif /* QAK */
    /* Update rest of free space manager data structures for section addition */
    if(H5FS_sect_link_rest(f, dxpl_id, fspace, cls, sect, flags) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't add section to non-size tracking data structures")
#ifdef QAK
HDfprintf(stderr, "%s: Check 3.0 - fspace->tot_space = %Hu\n", FUNC, fspace->tot_space);
#endif /* QAK */

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
    H5FS_section_class_t *sect_cls;     /* Section's class */
    H5FS_section_info_t *tmp_sect_node; /* Temporary free space section */
    hbool_t modified;                   /* Flag to indicate merge or shrink occurred */
    htri_t status;                      /* Status value */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_merge)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(*sect);
    HDassert(H5F_addr_defined((*sect)->addr));
    HDassert((*sect)->size);

    /* Loop until no more merging */
    if(fspace->sinfo->merge_list) {
        do {
            H5FS_section_class_t *tmp_sect_cls;     /* Temporary section's class */

            /* Reset 'modification occurred' flag */
            modified = FALSE;

            /* Look for neighboring section before new section */
            tmp_sect_node = H5SL_less(fspace->sinfo->merge_list, &(*sect)->addr);

            /* Check for node before new node able to merge with new node */
            if(tmp_sect_node) {
                /* Get classes for right & left sections */
                tmp_sect_cls = &fspace->sect_cls[tmp_sect_node->type];
                sect_cls = &fspace->sect_cls[(*sect)->type];

                /* Check if sections of the left most class can merge with sections
                 *  of another class & whether the sections are the same type,
                 *  then check for 'can merge' callback
                 */
                if((!(tmp_sect_cls->flags & H5FS_CLS_MERGE_SYM) || (tmp_sect_node->type == (*sect)->type))
                        && tmp_sect_cls->can_merge) {
                    /* Determine if the sections can merge */
                    if((status = (*tmp_sect_cls->can_merge)(tmp_sect_node, *sect, op_data)) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't check for merging sections")
                    if(status > 0) {
                        /* Sanity check */
                        HDassert(tmp_sect_cls->merge);

                        /* Remove 'less than' node from data structures */
                        if(H5FS_sect_remove(f, dxpl_id, fspace, tmp_sect_node) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")

                        /* Merge the two sections together */
                        if((*tmp_sect_cls->merge)(tmp_sect_node, *sect, op_data) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't merge two sections")

                        /* Retarget section pointer to 'less than' node that was merged into */
                        *sect = tmp_sect_node;

                        /* Indicate successful merge occurred */
                        modified = TRUE;
                    } /* end if */
                } /* end if */
            } /* end if */

            /* Look for section after new (or merged) section */
            tmp_sect_node = H5SL_greater(fspace->sinfo->merge_list, &(*sect)->addr);

            /* Check for node after new node able to merge with new node */
            if(tmp_sect_node) {
                /* Get classes for right & left sections */
                sect_cls = &fspace->sect_cls[(*sect)->type];
                tmp_sect_cls = &fspace->sect_cls[tmp_sect_node->type];

                /* Check if sections of the left most class can merge with sections
                 *  of another class & whether the sections are the same type,
                 *  then check for 'can merge' callback
                 */
                if((!(sect_cls->flags & H5FS_CLS_MERGE_SYM) || ((*sect)->type == tmp_sect_node->type))
                        && sect_cls->can_merge) {

                    /* Determine if the sections can merge */
                    if((status = (*sect_cls->can_merge)(*sect, tmp_sect_node, op_data)) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't check for merging sections")
                    if(status > 0) {
                        /* Sanity check */
                        HDassert(sect_cls->merge);

                        /* Remove 'greater than' node from data structures */
                        if(H5FS_sect_remove(f, dxpl_id, fspace, tmp_sect_node) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")

                        /* Merge the two sections together */
                        if((*sect_cls->merge)(*sect, tmp_sect_node, op_data) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't merge two sections")

                        /* Indicate successful merge occurred */
                        modified = TRUE;
                    } /* end if */
                } /* end if */
            } /* end if */
        } while(modified);
    } /* end if */
    HDassert(*sect);
#ifdef QAK
HDfprintf(stderr, "%s: Done merging, (*sect) = {%a, %Hu, %u, %s}\n", FUNC, (*sect)->addr, (*sect)->size, (*sect)->type, ((*sect)->state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Loop until no more shrinking */
    do {
        /* Reset 'modification occurred' flag */
        modified = FALSE;

        /* Check for (possibly merged) section able to shrink the size of the container */
        sect_cls = &fspace->sect_cls[(*sect)->type];
        if(sect_cls->can_shrink) {
            if((status = (*sect_cls->can_shrink)(*sect, op_data)) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTSHRINK, FAIL, "can't check for shrinking container")
            if(status > 0) {
#ifdef QAK
HDfprintf(stderr, "%s: Can shrink!\n", FUNC);
#endif /* QAK */
                /* Look for neighboring section before new section */
                if(fspace->sinfo->merge_list) {
                    tmp_sect_node = H5SL_less(fspace->sinfo->merge_list, &(*sect)->addr);

                    /* Make certain there isn't a section after the new section */
                    HDassert(H5SL_greater(fspace->sinfo->merge_list, &(*sect)->addr) == NULL);
                } /* end if */
                else
                    tmp_sect_node = NULL;

                /* Shrink the container */
                /* (callback can indicate that it has discarded the section by setting *sect to NULL) */
                HDassert(sect_cls->shrink);
                if((*sect_cls->shrink)(sect, op_data) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't shrink free space container")

                /* Check if the new section was removed */
                if(*sect == NULL && tmp_sect_node) {
                    /* Remove 'less than' node from data structures */
                    if(H5FS_sect_remove(f, dxpl_id, fspace, tmp_sect_node) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")

                    *sect = tmp_sect_node;
                } /* end if */

                /* Indicate successful merge occurred */
                modified = TRUE;
            } /* end if */
        } /* end if */
    } while(modified && *sect);
#ifdef QAK
HDfprintf(stderr, "%s: Done shrinking\n", FUNC);
if(*sect)
    HDfprintf(stderr, "%s: (*sect) = {%a, %Hu, %u, %s}\n", FUNC, (*sect)->addr, (*sect)->size, (*sect)->type, ((*sect)->state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
else
    HDfprintf(stderr, "%s: *sect = %p\n", FUNC, *sect);
#endif /* QAK */

done:
#ifdef QAK
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif /* QAK */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_add
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
H5FS_sect_add(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, H5FS_section_info_t *sect,
    unsigned flags, void *op_data)
{
    H5FS_section_class_t *cls;          /* Section's class */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5FS_sect_add, FAIL)

#ifdef QAK
HDfprintf(stderr, "%s: *sect = {%a, %Hu, %u, %s}\n", FUNC, sect->addr, sect->size, sect->type, (sect->state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Check arguments. */
    HDassert(fspace);
    HDassert(sect);
    HDassert(H5F_addr_defined(sect->addr));
    HDassert(sect->size);

    /* Check if we need to go get the sections */
    if(fspace->sinfo == NULL) {
        if(NULL == (fspace->sinfo = H5FS_sinfo_pin(f, dxpl_id, fspace)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTDECODE, FAIL, "can't pin sections")
    } /* end if */

    /* Call "add" section class callback, if there is one */
    cls = &fspace->sect_cls[sect->type];
    if(cls->add) {
        if((*cls->add)(sect, &flags, op_data) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "'add' section class callback failed")
    } /* end if */

    /* Check for merging returned space with existing section node */
    if(flags & H5FS_ADD_RETURNED_SPACE) {
#ifdef QAK
HDfprintf(stderr, "%s: Returning space\n", FUNC);
#endif /* QAK */

        /* Attempt to merge returned section with existing sections */
        if(H5FS_sect_merge(f, dxpl_id, fspace, &sect, op_data) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't merge sections")
    } /* end if */

    /* Add new (possibly merged) node to free sections data structures */
    /* (If section has been completely merged or shrunk away, 'sect' will
     *  be NULL at this point - QAK)
     */
    if(sect)
        if(H5FS_sect_link(f, dxpl_id, fspace, sect, flags) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space section into skip list")

#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->tot_space = %Hu\n", FUNC, fspace->hdr->tot_space);
#endif /* QAK */
    /* Mark free space sections as changed */
    /* (if adding sections while deserializing sections, don't set the flag) */
    if(!(flags & H5FS_ADD_DESERIALIZING)) {
        if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace->sinfo) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space sections as dirty")
    } /* end if */

done:
#ifdef H5FS_DEBUG
if(!(flags & (H5FS_ADD_DESERIALIZING | H5FS_ADD_SKIP_VALID)))
    H5FS_assert(fspace);
#endif /* H5FS_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_add() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_find_node
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
H5FS_sect_find_node(H5FS_t *fspace, hsize_t request, H5FS_section_info_t **node)
{
    H5FS_node_t *fspace_node;        /* Free list size node */
    unsigned bin;                   /* Bin to put the free space section in */
    htri_t ret_value = FALSE;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_find_node)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(fspace->sinfo->bins);
    HDassert(request > 0);
    HDassert(node);

    /* Determine correct bin which holds items of at least the section's size */
    bin = H5V_log2_gen(request);
    HDassert(bin < fspace->sinfo->nbins);
    while(bin < fspace->sinfo->nbins && fspace->sinfo->bins[bin].bin_list == NULL)
        bin++;

    /* Find the first free space section that is large enough to fulfill request */
    /* (Since the bins use skip lists to track the sizes of the address-ordered
     *  lists, this is actually a "best fit" algorithm)
     */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->sinfo->nbins = %u\n", FUNC, fspace->sinfo->nbins);
HDfprintf(stderr, "%s: bin = %u\n", FUNC, bin);
#endif /* QAK */
    if(bin < fspace->sinfo->nbins)
        do {
            /* Look for large enough free space section in this bin */
            if(fspace->sinfo->bins[bin].bin_list)
                /* Check for large enough list of sections on list */
                if((fspace_node = H5SL_greater(fspace->sinfo->bins[bin].bin_list, &request))) {
                    const H5FS_section_class_t *cls;    /* Class of section */

                    /* Take first node off of the list (ie. node w/lowest address) */
                    if(NULL == (*node = H5SL_remove_first(fspace_node->sect_list)))
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space node from skip list")

                    /* Get section's class */
                    cls = &fspace->sect_cls[(*node)->type];

                    /* Decrement # of sections in section size node */
                    if(H5FS_size_node_decr(fspace->sinfo, bin, fspace_node, cls) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space size node from skip list")

                    /* Indicate that we found a node for the request */
                    HGOTO_DONE(TRUE)
                } /* end if */

            /* Advance to next larger bin */
            bin++;
        } while(bin < fspace->sinfo->nbins);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_find_node() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_find
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
H5FS_sect_find(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, hsize_t request,
    H5FS_section_info_t **node)
{
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI(H5FS_sect_find, FAIL)

#ifdef QAK
HDfprintf(stderr, "%s: request = %Hu\n", FUNC, request);
#endif /* QAK */

    /* Check arguments. */
    HDassert(fspace);
    HDassert(request);
    HDassert(node);

    /* Check if we need to go get the sections */
    if(fspace->sinfo == NULL) {
        if(NULL == (fspace->sinfo = H5FS_sinfo_pin(f, dxpl_id, fspace)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTDECODE, FAIL, "can't pin sections")
    } /* end if */

    /* Check for any sections on free space list */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->tot_sect_count = %Hu\n", FUNC, fspace->tot_sect_count);
HDfprintf(stderr, "%s: fspace->serial_sect_count = %Hu\n", FUNC, fspace->serial_sect_count);
HDfprintf(stderr, "%s: fspace->ghost_sect_count = %Hu\n", FUNC, fspace->ghost_sect_count);
#endif /* QAK */
    if(fspace->tot_sect_count > 0) {
        /* Look for node in bins */
        if((ret_value = H5FS_sect_find_node(fspace, request, node)) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from bins")

        /* Decrement # of sections on free list, if we found an object */
        if(ret_value > 0) {
            const H5FS_section_class_t *cls;    /* Class of section */

            /* Get section's class */
            cls = &fspace->sect_cls[(*node)->type];

            /* Update rest of free space manager data structures for node removal */
            if(H5FS_sect_unlink_rest(f, dxpl_id, fspace, cls, *node) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from non-size tracking data structures")
#ifdef QAK
HDfprintf(stderr, "%s: (*node)->size = %Hu, (*node)->addr = %a, (*node)->type = %u\n", FUNC, (*node)->size, (*node)->addr, (*node)->type);
#endif /* QAK */
        } /* end if */
    } /* end if */

done:
#ifdef H5FS_DEBUG
    H5FS_assert(fspace);
#endif /* H5FS_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_find() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_serialize_size
 *
 * Purpose:	Determine serialized size of all sections in free space manager
 *              And adjust space on disk for storing serialized sections
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
H5FS_sect_serialize_size(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_serialize_size)

    /* Check arguments. */
    HDassert(fspace);
#ifdef QAK
HDfprintf(stderr, "%s: Check 1.0 - fspace->sect_size = %Hu\n", FUNC, fspace->sect_size);
HDfprintf(stderr, "%s: fspace->alloc_sect_size = %Hu\n", FUNC, fspace->alloc_sect_size);
HDfprintf(stderr, "%s: fspace->sinfo->serial_size_count = %Zu\n", FUNC, fspace->sinfo->serial_size_count);
#endif /* QAK */

    /* Compute the size of the buffer required to serialize all the sections */
    if(fspace->serial_sect_count > 0) {
        size_t sect_buf_size;               /* Section buffer size */

        /* Serialized sections prefix */
        sect_buf_size = fspace->sinfo->sect_prefix_size;

        /* Count for each differently sized serializable section */
#ifdef QAK
HDfprintf(stderr, "%s: fspace->sinfo->serial_size_count = %Zu\n", FUNC, fspace->sinfo->serial_size_count);
HDfprintf(stderr, "%s: fspace->serial_sect_count = %Hu\n", FUNC, fspace->serial_sect_count);
#endif /* QAK */
        sect_buf_size += fspace->sinfo->serial_size_count * MAX(1, ((H5V_log2_gen(fspace->serial_sect_count) + 7) / 8));

        /* Size for each differently sized serializable section */
        sect_buf_size += fspace->sinfo->serial_size_count * fspace->sinfo->sect_len_size;

        /* Offsets of each section in address space */
        sect_buf_size += fspace->serial_sect_count * fspace->sinfo->sect_off_size;

        /* Class of each section */
        sect_buf_size += fspace->serial_sect_count * 1;

        /* Extra space required to serialize each section */
        sect_buf_size += fspace->sinfo->serial_size;

        /* Update section size in header */
        fspace->sect_size = sect_buf_size;
    } /* end if */
    else
        /* Reset section size in header */
        fspace->sect_size = H5FS_SINFO_SIZE_DEFAULT;

#ifdef QAK
HDfprintf(stderr, "%s: Check 2.0 - fspace->sect_size = %Hu\n", FUNC, fspace->sect_size);
HDfprintf(stderr, "%s: fspace->alloc_sect_size = %Hu\n", FUNC, fspace->alloc_sect_size);
#endif /* QAK */
    if(fspace->sect_size > fspace->alloc_sect_size) {
        size_t new_size;        /* New size of space for serialized sections */
        haddr_t old_addr;       /* Old address of serialized sections */

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
        old_addr = fspace->sect_addr;
        if(H5MF_xfree(f, H5FD_MEM_FSPACE_SINFO, dxpl_id, old_addr, fspace->alloc_sect_size) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to free free space sections")

        /* Compute new size */
        new_size = fspace->alloc_sect_size;
        while(new_size < fspace->sect_size)
            new_size *= (double)fspace->expand_percent / 100.0;
        fspace->alloc_sect_size = new_size;

        /* Allocate space for the new serialized sections on disk */
#ifdef QAK
HDfprintf(stderr, "%s: Allocating space for larger serialized sections, new_size = %Zu\n", FUNC, new_size);
HDfprintf(stderr, "%s: fspace->sect_size = %Hu\n", FUNC, fspace->sect_size);
#endif /* QAK */
        if(HADDR_UNDEF == (fspace->sect_addr = H5MF_alloc(f, H5FD_MEM_FSPACE_SINFO, dxpl_id, (hsize_t)fspace->alloc_sect_size)))
            HGOTO_ERROR(H5E_FSPACE, H5E_NOSPACE, FAIL, "file allocation failed for free space sections")
#ifdef QAK
HDfprintf(stderr, "%s: old_addr = %a, fspace->sect_addr = %a\n", FUNC, old_addr, fspace->sect_addr);
#endif /* QAK */

        /* Move object in cache, if it actually was relocated */
        if(H5F_addr_ne(fspace->sect_addr, old_addr)) {
            if(H5AC_rename(f, H5AC_FSPACE_SINFO, old_addr, fspace->sect_addr) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTRENAME, FAIL, "unable to move free space section info")
        } /* end if */
        else {
            /* Mark free space section as dirty */
            if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace->sinfo) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space section info as dirty")
        } /* end else */

        /* Mark free space header as dirty */
        if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space header as dirty")
    } /* end if */
    else {
        size_t decrease_threshold;      /* Size threshold for decreasing serialized section size */
        haddr_t old_addr;               /* Old address of serialized sections */

        /* Compute the threshold for decreasing the sections' serialized size */
        decrease_threshold = ((size_t)fspace->alloc_sect_size * (double)fspace->shrink_percent) / 100.0;

        if(fspace->alloc_sect_size > H5FS_SINFO_SIZE_DEFAULT &&
                fspace->sect_size < decrease_threshold) {
            size_t new_size = 0;        /* New size of space for serialized sections */

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
            old_addr = fspace->sect_addr;
            if(H5MF_xfree(f, H5FD_MEM_FSPACE_SINFO, dxpl_id, old_addr, fspace->alloc_sect_size) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to free free space sections")

            /* Compute new size */
            while(fspace->sect_size < decrease_threshold) {
                new_size = decrease_threshold;

                decrease_threshold *= (double)fspace->shrink_percent / 100.0;
            } /* end while */
            if(new_size < H5FS_SINFO_SIZE_DEFAULT)
                new_size = H5FS_SINFO_SIZE_DEFAULT;
            fspace->alloc_sect_size = new_size;

            /* Allocate space for the new serialized sections on disk */
#ifdef QAK
HDfprintf(stderr, "%s: Allocating space for smaller serialized sections, new_size = %Zu\n", FUNC, new_size);
#endif /* QAK */
            if(HADDR_UNDEF == (fspace->sect_addr = H5MF_alloc(f, H5FD_MEM_FSPACE_SINFO, dxpl_id, (hsize_t)fspace->alloc_sect_size)))
                HGOTO_ERROR(H5E_FSPACE, H5E_NOSPACE, FAIL, "file allocation failed for free space sections")

            /* Move object in cache, if it actually was relocated */
            if(H5F_addr_ne(fspace->sect_addr, old_addr)) {
                if(H5AC_rename(f, H5AC_FSPACE_SINFO, old_addr, fspace->sect_addr) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTRENAME, FAIL, "unable to move free space section info")
            } /* end if */
            else {
                /* Mark free space section as dirty */
                if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace->sinfo) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space section info as dirty")
            } /* end else */

            /* Mark free space header as dirty */
            if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space header as dirty")
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_serialize_size() */


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
    H5FS_iter_ud_t *udata = (H5FS_iter_ud_t *)_udata; /* Callback info */
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
    H5FS_iter_ud_t *udata = (H5FS_iter_ud_t *)_udata; /* Callback info */
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
 * Function:	H5FS_sect_iterate
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
H5FS_sect_iterate(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace, H5FS_operator_t op, void *op_data)
{
    H5FS_iter_ud_t udata;              /* User data for callbacks */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_iterate)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(op);

    /* Check if we need to go get the sections */
    if(fspace->sinfo == NULL) {
        if(NULL == (fspace->sinfo = H5FS_sinfo_pin(f, dxpl_id, fspace)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTDECODE, FAIL, "can't pin sections")
    } /* end if */

#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->sect_count = %Hu\n", FUNC, fspace->hdr->sect_count);
#endif /* QAK */

    /* Set up user data for iterator */
    udata.fspace = fspace;
    udata.op = op;
    udata.op_data = op_data;

    /* Iterate over sections, if there are any */
    if(fspace->tot_sect_count) {
        unsigned bin;           /* Current bin we are on */

        /* Iterate over all the bins */
#ifdef QAK
HDfprintf(stderr, "%s: Iterate over section bins\n", FUNC);
#endif /* QAK */
        for(bin = 0; bin < fspace->sinfo->nbins; bin++) {
            /* Check if there are any sections in this bin */
            if(fspace->sinfo->bins[bin].bin_list) {
                /* Iterate over list of section size nodes for bin */
                if(H5SL_iterate(fspace->sinfo->bins[bin].bin_list, H5FS_iterate_node_cb, &udata) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section size nodes")
            } /* end if */
        } /* end for */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_get_sect_count
 *
 * Purpose:	Retrieve the number of sections managed
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
H5FS_get_sect_count(const H5FS_t *fspace, hsize_t *nsects)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5FS_get_sect_count)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(nsects);

    /* Get the section count */
    *nsects = fspace->tot_sect_count;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_get_sect_count() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_change_class
 *
 * Purpose:	Make appropriate adjustments to internal data structures when
 *              a section changes class
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_sect_change_class(H5F_t *f, hid_t dxpl_id, H5FS_t *fspace,
    H5FS_section_info_t *sect, unsigned new_class)
{
    const H5FS_section_class_t *old_cls;        /* Old class of section */
    const H5FS_section_class_t *new_cls;        /* New class of section */
    unsigned old_class;                         /* Old class ID of section */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_sect_change_class)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(sect);
    HDassert(sect->type < fspace->nclasses);
    HDassert(new_class < fspace->nclasses);

    /* Get class info */
    old_class = sect->type;
    old_cls = &fspace->sect_cls[sect->type];
    new_cls = &fspace->sect_cls[new_class];
#ifdef QAK
HDfprintf(stderr, "%s: old_cls->flags = %x\n", FUNC, old_cls->flags);
HDfprintf(stderr, "%s: new_cls->flags = %x\n", FUNC, new_cls->flags);
#endif /* QAK */

    /* Check if the section's class change will affect the # of serializable or ghost sections */
    if((old_cls->flags & H5FS_CLS_GHOST_OBJ) != (new_cls->flags & H5FS_CLS_GHOST_OBJ)) {
        H5FS_node_t *fspace_node;       /* Free list size node */
        unsigned bin;                   /* Bin to put the free space section in */
        hbool_t to_ghost;       /* Flag if the section is changing to a ghost section */

        /* Determine if this section is becoming a ghost or is becoming serializable */
        if(old_cls->flags & H5FS_CLS_GHOST_OBJ)
            to_ghost = FALSE;
        else
            to_ghost = TRUE;
#ifdef QAK
HDfprintf(stderr, "%s: to_ghost = %u\n", FUNC, to_ghost);
#endif /* QAK */

        /* Sanity check */
        HDassert(fspace->sinfo->bins);

        /* Determine correct bin which holds items of at least the section's size */
        bin = H5V_log2_gen(sect->size);
        HDassert(bin < fspace->sinfo->nbins);
        HDassert(fspace->sinfo->bins[bin].bin_list);

        /* Get space node for section's size */
        fspace_node = H5SL_search(fspace->sinfo->bins[bin].bin_list, &sect->size);
        HDassert(fspace_node);

        /* Adjust serializable/ghost counts */
        if(to_ghost) {
            /* Adjust global section count totals */
            fspace->serial_sect_count--;
            fspace->ghost_sect_count++;

            /* Adjust bin's section count totals */
            fspace->sinfo->bins[bin].serial_sect_count--;
            fspace->sinfo->bins[bin].ghost_sect_count++;

            /* Adjust section size node's section count totals */
            fspace_node->serial_count--;
            fspace_node->ghost_count++;

            /* Check if we switched a section size node's status */
            if(fspace_node->serial_count == 0)
                fspace->sinfo->serial_size_count--;
            if(fspace_node->ghost_count == 1)
                fspace->sinfo->ghost_size_count++;
        } /* end if */
        else {
            /* Adjust global section count totals */
            fspace->serial_sect_count++;
            fspace->ghost_sect_count--;

            /* Adjust bin's section count totals */
            fspace->sinfo->bins[bin].serial_sect_count++;
            fspace->sinfo->bins[bin].ghost_sect_count--;

            /* Adjust section size node's section count totals */
            fspace_node->serial_count++;
            fspace_node->ghost_count--;

            /* Check if we switched a section size node's status */
            if(fspace_node->serial_count == 1)
                fspace->sinfo->serial_size_count++;
            if(fspace_node->ghost_count == 0)
                fspace->sinfo->ghost_size_count--;
        } /* end else */
    } /* end if */

    /* Check if the section's class change will affect the mergable list */
    if((old_cls->flags & H5FS_CLS_SEPAR_OBJ) != (new_cls->flags & H5FS_CLS_SEPAR_OBJ)) {
        hbool_t to_mergable;       /* Flag if the section is changing to a mergable section */

        /* Determine if this section is becoming mergable or is becoming separate */
        if(old_cls->flags & H5FS_CLS_SEPAR_OBJ)
            to_mergable = TRUE;
        else
            to_mergable = FALSE;
#ifdef QAK
HDfprintf(stderr, "%s: to_mergable = %u\n", FUNC, to_mergable);
#endif /* QAK */

        /* Add or remove section from merge list, as appropriate */
        if(to_mergable) {
#ifdef QAK
HDfprintf(stderr, "%s: inserting object into merge list, sect->type = %u\n", FUNC, (unsigned)sect->type);
#endif /* QAK */
            if(fspace->sinfo->merge_list == NULL)
                if(NULL == (fspace->sinfo->merge_list = H5SL_create(H5SL_TYPE_HADDR, 0.5, (size_t)H5FS_DEFAULT_SKIPLIST_HEIGHT)))
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for merging free space sections")
            if(H5SL_insert(fspace->sinfo->merge_list, sect, &sect->addr) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into merging skip list")
        } /* end if */
        else {
            H5FS_section_info_t *tmp_sect_node; /* Temporary section node */

#ifdef QAK
HDfprintf(stderr, "%s: removing object from merge list, sect->type = %u\n", FUNC, (unsigned)sect->type);
#endif /* QAK */
            tmp_sect_node = H5SL_remove(fspace->sinfo->merge_list, &sect->addr);
            if(tmp_sect_node == NULL || tmp_sect_node != sect)
                HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section node on size list")
        } /* end else */
    } /* end if */

    /* Change the section's class */
    sect->type = new_class;

    /* Change the serialized size of sections */
    fspace->sinfo->serial_size -= fspace->sect_cls[old_class].serial_size;
    fspace->sinfo->serial_size += fspace->sect_cls[new_class].serial_size;

    /* Update current space used for free space sections */
    if(H5FS_sect_serialize_size(f, dxpl_id, fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTCOMPUTE, FAIL, "can't adjust free space section size on disk")
    
    /* Mark free space sections as dirty */
    if(H5AC_mark_pinned_or_protected_entry_dirty(f, fspace->sinfo) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space sections as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_change_class() */

#ifdef H5FS_DEBUG

/*-------------------------------------------------------------------------
 * Function:	H5FS_sect_assert
 *
 * Purpose:	Verify that the sections managed are mostly sane
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
H5FS_sect_assert(const H5FS_t *fspace)
{
    hsize_t separate_obj;       /* The number of separate objects managed */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_sect_assert)
#ifdef QAK
HDfprintf(stderr, "%s: fspace->hdr->tot_sect_count = %Hu\n", "H5FS_sect_assert", fspace->hdr->tot_sect_count);
#endif /* QAK */

    /* Initialize state */
    separate_obj = 0;

    /* Check for bins to work on */
    if(fspace->sinfo->bins) {
        hsize_t acc_tot_sect_count;     /* Accumulated total section count from bins */
        hsize_t acc_serial_sect_count;  /* Accumulated serializable section count from bins */
        hsize_t acc_ghost_sect_count;   /* Accumulated ghost section count from bins */
        size_t acc_tot_size_count;      /* Accumulated total section size count from bins */
        size_t acc_serial_size_count;   /* Accumulated serializable section size count from bins */
        size_t acc_ghost_size_count;    /* Accumulated ghost section size count from bins */
        unsigned u;             /* Local index variable */

        /* Walk through all sections in bins */
        acc_tot_sect_count = 0;
        acc_serial_sect_count = 0;
        acc_ghost_sect_count = 0;
        acc_tot_size_count = 0;
        acc_serial_size_count = 0;
        acc_ghost_size_count = 0;
        for(u = 0; u < fspace->sinfo->nbins; u++) {
            acc_tot_sect_count += fspace->sinfo->bins[u].tot_sect_count;
            acc_serial_sect_count += fspace->sinfo->bins[u].serial_sect_count;
            acc_ghost_sect_count += fspace->sinfo->bins[u].ghost_sect_count;
            if(fspace->sinfo->bins[u].bin_list) {
                H5SL_node_t *curr_size_node;    /* Current section size node in skip list */
                size_t bin_serial_count;        /* # of serializable sections in this bin */
                size_t bin_ghost_count;         /* # of ghost sections in this bin */

                acc_tot_size_count += H5SL_count(fspace->sinfo->bins[u].bin_list);

                /* Walk through the sections in this bin */
                curr_size_node = H5SL_first(fspace->sinfo->bins[u].bin_list);
                bin_serial_count = 0;
                bin_ghost_count = 0;
                while(curr_size_node != NULL) {
                    H5FS_node_t *fspace_node;       /* Section size node */
                    H5SL_node_t *curr_sect_node;    /* Current section node in skip list */
                    size_t size_serial_count;       /* # of serializable sections of this size */
                    size_t size_ghost_count;        /* # of ghost sections of this size */

                    /* Get section size node */
                    fspace_node = H5SL_item(curr_size_node);

                    /* Check sections on list */
                    curr_sect_node = H5SL_first(fspace_node->sect_list);
                    size_serial_count = 0;
                    size_ghost_count = 0;
                    while(curr_sect_node != NULL) {
                        H5FS_section_class_t *cls;      /* Class of section */
                        H5FS_section_info_t *sect;      /* Section */

                        /* Get section node & it's class */
                        sect = H5SL_item(curr_sect_node);
                        cls = &fspace->sect_cls[sect->type];
#ifdef QAK
HDfprintf(stderr, "%s: sect->size = %Hu, sect->addr = %a, sect->type = %u\n", "H5FS_assert", sect->size, sect->addr, sect->type);
#endif /* QAK */

                        /* Sanity check section */
                        HDassert(H5F_addr_defined(sect->addr));
                        HDassert(fspace_node->sect_size == sect->size);
                        if(cls->valid)
                            (*cls->valid)(cls, sect);

                        /* Add to correct count */
                        if(cls->flags & H5FS_CLS_GHOST_OBJ)
                            size_ghost_count++;
                        else
                            size_serial_count++;

                        /* Count node, if separate */
                        if(cls->flags & H5FS_CLS_SEPAR_OBJ)
                            separate_obj++;

                        /* Get the next section node in the list */
                        curr_sect_node = H5SL_next(curr_sect_node);
                    } /* end while */

                    /* Check the number of serializable & ghost sections of this size */
                    HDassert(fspace_node->serial_count == size_serial_count);
                    HDassert(fspace_node->ghost_count == size_ghost_count);

                    /* Add to global count of serializable & ghost section sizes */
                    if(fspace_node->serial_count > 0)
                        acc_serial_size_count++;
                    if(fspace_node->ghost_count > 0)
                        acc_ghost_size_count++;

                    /* Add to bin's serializable & ghost counts */
                    bin_serial_count += size_serial_count;
                    bin_ghost_count += size_ghost_count;

                    /* Get the next section size node in the list */
                    curr_size_node = H5SL_next(curr_size_node);
                } /* end while */

                /* Check the number of serializable & ghost sections in this bin */
                HDassert(fspace->sinfo->bins[u].tot_sect_count == (bin_serial_count + bin_ghost_count));
                HDassert(fspace->sinfo->bins[u].serial_sect_count == bin_serial_count);
                HDassert(fspace->sinfo->bins[u].ghost_sect_count == bin_ghost_count);
            } /* end if */
        } /* end for */

        /* Check counts from bins vs. global counts */
        HDassert(fspace->sinfo->tot_size_count == acc_tot_size_count);
        HDassert(fspace->sinfo->serial_size_count == acc_serial_size_count);
        HDassert(fspace->sinfo->ghost_size_count == acc_ghost_size_count);
        HDassert(fspace->tot_sect_count == acc_tot_sect_count);
        HDassert(fspace->serial_sect_count == acc_serial_sect_count);
        HDassert(fspace->ghost_sect_count == acc_ghost_sect_count);
    } /* end if */
    else {
        /* Check counts are zero */
        HDassert(fspace->tot_sect_count == 0);
        HDassert(fspace->serial_sect_count == 0);
        HDassert(fspace->ghost_sect_count == 0);
    } /* end else */

    /* Make certain that the number of sections on the address list is correct */
    if(fspace->sinfo->merge_list)
        HDassert(fspace->tot_sect_count == (separate_obj + H5SL_count(fspace->sinfo->merge_list)));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FS_sect_assert() */
#endif /* H5FS_DEBUG */

