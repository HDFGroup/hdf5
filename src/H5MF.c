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
 * Created:             H5MF.c
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             File memory management functions.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5MF_PACKAGE		/*suppress error about including H5MFpkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5MFpkg.h"		/* File memory management		*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/


/****************/
/* Local Macros */
/****************/

#define H5MF_FSPACE_SHRINK      80              /* Percent of "normal" size to shrink serialized free space size */
#define H5MF_FSPACE_EXPAND      120             /* Percent of "normal" size to expand serialized free space size */

/* Map an allocation request type to a free list */
#define H5MF_ALLOC_TO_FS_TYPE(F, T)      ((H5FD_MEM_DEFAULT == (F)->shared->fs_type_map[T]) \
    ? (T) : (F)->shared->fs_type_map[T])


/******************/
/* Local Typedefs */
/******************/

/* Enum for kind of free space section+aggregator merging allowed for a file */
typedef enum {
    H5MF_AGGR_MERGE_SEPARATE,           /* Everything in separate free list */
    H5MF_AGGR_MERGE_DICHOTOMY,          /* Metadata in one free list and raw data in another */
    H5MF_AGGR_MERGE_TOGETHER            /* Metadata & raw data in one free list */
} H5MF_aggr_merge_t;


/********************/
/* Package Typedefs */
/********************/


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
 * Function:    H5MF_init_merge_flags
 *
 * Purpose:     Initialize the free space section+aggregator merge flags
 *              for the file.
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Friday, February  1, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_init_merge_flags(H5F_t *f)
{
    H5MF_aggr_merge_t mapping_type;     /* Type of free list mapping */
    H5FD_mem_t type;                    /* Memory type for iteration */
    hbool_t all_same;                   /* Whether all the types map to the same value */

    FUNC_ENTER_NOAPI_NOFUNC(H5MF_init_merge_flags)

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);

    /* Iterate over all the free space types to determine if sections of that type
     *  can merge with the metadata or small 'raw' data aggregator
     */
    all_same = TRUE;
    for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
        /* Check for any different type mappings */
        if(f->shared->fs_type_map[type] != f->shared->fs_type_map[H5FD_MEM_DEFAULT]) {
            all_same = FALSE;
            break;
        } /* end if */

    /* Check for all allocation types mapping to the same free list type */
    if(all_same) {
        if(f->shared->fs_type_map[H5FD_MEM_DEFAULT] == H5FD_MEM_DEFAULT)
            mapping_type = H5MF_AGGR_MERGE_SEPARATE;
        else
            mapping_type = H5MF_AGGR_MERGE_TOGETHER;
    } /* end if */
    else {
        /* Check for raw data mapping into same list as metadata */
        if(f->shared->fs_type_map[H5FD_MEM_DRAW] == f->shared->fs_type_map[H5FD_MEM_SUPER])
            mapping_type = H5MF_AGGR_MERGE_SEPARATE;
        else {
            hbool_t all_metadata_same;              /* Whether all metadata go in same free list */

            /* One or more allocation type don't map to the same free list type */
            /* Check if all the metadata allocation types map to the same type */
            all_metadata_same = TRUE;
            for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
                /* Skip checking raw data free list mapping */
                if(type != H5FD_MEM_DRAW) {
                    /* Check for any different type mappings */
                    if(f->shared->fs_type_map[type] != f->shared->fs_type_map[H5FD_MEM_SUPER]) {
                        all_metadata_same = FALSE;
                        break;
                    } /* end if */
                } /* end if */

            /* Check for all metadata on same free list */
            if(all_metadata_same)
                mapping_type = H5MF_AGGR_MERGE_DICHOTOMY;
            else
                mapping_type = H5MF_AGGR_MERGE_SEPARATE;
        } /* end else */
    } /* end else */

    /* Based on mapping type, initialize merging flags for each free list type */
    switch(mapping_type) {
        case H5MF_AGGR_MERGE_SEPARATE:
            /* Don't merge any metadata together */
            HDmemset(f->shared->fs_aggr_merge, 0, sizeof(f->shared->fs_aggr_merge));

            /* Check if merging raw data should be allowed */
            if(H5FD_MEM_DRAW == f->shared->fs_type_map[H5FD_MEM_DRAW] ||
                    H5FD_MEM_DEFAULT == f->shared->fs_type_map[H5FD_MEM_DRAW])
                f->shared->fs_aggr_merge[H5FD_MEM_DRAW] = H5F_FS_MERGE_RAWDATA;
            break;

        case H5MF_AGGR_MERGE_DICHOTOMY:
            /* Merge all metadata together (but not raw data) */
            HDmemset(f->shared->fs_aggr_merge, H5F_FS_MERGE_METADATA, sizeof(f->shared->fs_aggr_merge));

            /* Allow merging raw data allocations together */
            f->shared->fs_aggr_merge[H5FD_MEM_DRAW] = H5F_FS_MERGE_RAWDATA;
            break;

        case H5MF_AGGR_MERGE_TOGETHER:
            /* Merge all allocation types together */
            HDmemset(f->shared->fs_aggr_merge, (H5F_FS_MERGE_METADATA | H5F_FS_MERGE_RAWDATA), sizeof(f->shared->fs_aggr_merge));
            break;
    } /* end switch */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5MF_init_merge_flags() */


/*-------------------------------------------------------------------------
 * Function:	H5MF_alloc_start
 *
 * Purpose:	"Start up" free space for file - open existing free space
 *              structure if one exists, otherwise create a new free space
 *              structure
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jan  8 2008
 *
 * Modifications:
 *	Vailin Choi, July 29th, 2008
 *	  Pass values of alignment and threshold to FS_create() for handling alignment
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_alloc_start(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type, hbool_t may_create)
{
    const H5FS_section_class_t *classes[] = { /* Free space section classes implemented for file */
        H5MF_FSPACE_SECT_CLS_SIMPLE};
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5MF_alloc_start)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(f->shared);

    /* Check for creating free space info for the file */
    if(H5F_addr_defined(f->shared->fs_addr[type])) {
        /* Open an existing free space structure for the file */
        HDassert(f->shared->fs_state[type] == H5F_FS_STATE_CLOSED);
        if(NULL == (f->shared->fs_man[type] = H5FS_open(f, dxpl_id, f->shared->fs_addr[type],
                NELMTS(classes), classes, f, f->shared->alignment, f->shared->threshold)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space info")
    } /* end if */
    else {
        /* Check if we are allowed to create the free space manager */
        if(may_create) {
            H5FS_create_t fs_create;        /* Free space creation parameters */

            /* Set the free space creation parameters */
            fs_create.client = H5FS_CLIENT_FILE_ID;
            fs_create.shrink_percent = H5MF_FSPACE_SHRINK;
            fs_create.expand_percent = H5MF_FSPACE_EXPAND;
            fs_create.max_sect_addr = 1 + H5V_log2_gen((uint64_t)f->shared->maxaddr);
            fs_create.max_sect_size = f->shared->maxaddr;

            /* Create the free space structure for the heap */
            HDassert(f->shared->fs_state[type] == H5F_FS_STATE_CLOSED);
#ifdef LATER
            if(NULL == (f->shared->fs_man[type] = H5FS_create(f, dxpl_id, &f->shared->fs_addr[type],
                    &fs_create, NELMTS(classes), classes, f, f->shared->alignment, f->shared->threshold)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space info")
#else /* LATER */
            if(NULL == (f->shared->fs_man[type] = H5FS_create(f, dxpl_id, NULL,
                    &fs_create, NELMTS(classes), classes, f, f->shared->alignment, f->shared->threshold)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space info")
#endif /* LATER */
        } /* end if */
    } /* end else */

    /* Set the state for the free space manager to "open", if it is now */
    if(f->shared->fs_man[type])
        f->shared->fs_state[type] = H5F_FS_STATE_OPEN;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_alloc_start() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_alloc
 *
 * Purpose:     Allocate SIZE bytes of file memory and return the relative
 *		address where that contiguous chunk of file memory exists.
 *		The TYPE argument describes the purpose for which the storage
 *		is being requested.
 *
 * Return:      Success:        The file address of new chunk.
 *              Failure:        HADDR_UNDEF
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 11 1997
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5MF_alloc(H5F_t *f, H5FD_mem_t alloc_type, hid_t dxpl_id, hsize_t size)
{
    H5FD_mem_t  fs_type;                /* Free space type (mapped from allocation type) */
    haddr_t	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5MF_alloc, HADDR_UNDEF)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: alloc_type = %u, size = %Hu\n", FUNC, (unsigned)alloc_type, size);
#endif /* H5MF_ALLOC_DEBUG */

    /* check arguments */
    HDassert(f);
    HDassert(size > 0);

    /* Get free space type from allocation type */
    fs_type = H5MF_ALLOC_TO_FS_TYPE(f, alloc_type);

    /* Check if the free space manager for the file has been initialized */
    if(!f->shared->fs_man[fs_type])
        if(H5MF_alloc_start(f, dxpl_id, fs_type, FALSE) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, HADDR_UNDEF, "can't initialize file free space")

    /* Search for large enough space in the free space manager */
    if(f->shared->fs_man[fs_type]) {
        H5MF_free_section_t *node;      /* Free space section pointer */
        htri_t node_found = FALSE;      /* Whether an existing free list node was found */

        /* Try to get a section from the free space manager */
        if((node_found = H5FS_sect_find(f, dxpl_id, f->shared->fs_man[fs_type], size, (H5FS_section_info_t **)&node)) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, HADDR_UNDEF, "error locating free space in file")
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Check 1.5, node_found = %t\n", FUNC, node_found);
#endif /* H5MF_ALLOC_DEBUG_MORE */

        /* Check for actually finding section */
        if(node_found) {
            /* Sanity check */
            HDassert(node);

            /* Retrieve return value */
            ret_value = node->sect_info.addr;

            /* Check for eliminating the section */
            if(node->sect_info.size == size) {
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Check 1.6, freeing node\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
                /* Free section node */
                if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, HADDR_UNDEF, "can't free simple section node")
            } /* end if */
            else {
                H5MF_sect_ud_t udata;               /* User data for callback */

                /* Adjust information for section */
                node->sect_info.addr += size;
                node->sect_info.size -= size;

                /* Construct user data for callbacks */
                udata.f = f;
                udata.dxpl_id = dxpl_id;
                udata.alloc_type = alloc_type;
                udata.allow_sect_absorb = TRUE;

#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Check 1.7, re-adding node, node->sect_info.size = %Hu\n", FUNC, node->sect_info.size);
#endif /* H5MF_ALLOC_DEBUG_MORE */
                /* Re-insert section node into file's free space */
                if(H5FS_sect_add(f, dxpl_id, f->shared->fs_man[fs_type], (H5FS_section_info_t *)node, H5FS_ADD_RETURNED_SPACE, &udata) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINSERT, HADDR_UNDEF, "can't re-add section to file free space")
            } /* end else */

            /* Leave now */
            HGOTO_DONE(ret_value)
        } /* end if */
    } /* end if */
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Check 2.0\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */

    /* Couldn't find anything from the free space manager, go allocate some */
    if(alloc_type != H5FD_MEM_DRAW) {
        /* Handle metadata differently from "raw" data */
        if(HADDR_UNDEF == (ret_value = H5MF_aggr_alloc(f, dxpl_id, &(f->shared->meta_aggr), &(f->shared->sdata_aggr), alloc_type, size)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, HADDR_UNDEF, "can't allocate metadata")
    } /* end if */
    else {
        /* Allocate "raw" data */
        if(HADDR_UNDEF == (ret_value = H5MF_aggr_alloc(f, dxpl_id, &(f->shared->sdata_aggr), &(f->shared->meta_aggr), alloc_type, size)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, HADDR_UNDEF, "can't allocate raw data")
    } /* end else */

done:
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving: ret_value = %a, size = %Hu\n", FUNC, ret_value, size);
#endif /* H5MF_ALLOC_DEBUG */
#ifdef H5MF_ALLOC_DEBUG_DUMP
H5MF_sects_dump(f, dxpl_id, stderr);
#endif /* H5MF_ALLOC_DEBUG_DUMP */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_xfree
 *
 * Purpose:     Frees part of a file, making that part of the file
 *              available for reuse.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 17 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_xfree(H5F_t *f, H5FD_mem_t alloc_type, hid_t dxpl_id, haddr_t addr,
    hsize_t size)
{
    H5MF_free_section_t *node = NULL;   /* Free space section pointer */
    H5MF_sect_ud_t udata;               /* User data for callback */
    H5FD_mem_t fs_type;                 /* Free space type (mapped from allocation type) */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5MF_xfree, FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering - alloc_type = %u, addr = %a, size = %Hu\n", FUNC, (unsigned)alloc_type, addr, size);
#endif /* H5MF_ALLOC_DEBUG */

    /* check arguments */
    HDassert(f);
    if(!H5F_addr_defined(addr) || 0 == size)
        HGOTO_DONE(SUCCEED);
    HDassert(addr != 0);        /* Can't deallocate the superblock :-) */

    /* Check if the space to free intersects with the file's metadata accumulator */
    if(H5F_accum_free(f, dxpl_id, alloc_type, addr, size) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "can't check free space intersection w/metadata accumulator")

    /* Get free space type from allocation type */
    fs_type = H5MF_ALLOC_TO_FS_TYPE(f, alloc_type);
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: fs_type = %u\n", FUNC, (unsigned)fs_type);
#endif /* H5MF_ALLOC_DEBUG_MORE */

    /* Check if the free space manager for the file has been initialized */
    if(!f->shared->fs_man[fs_type]) {
        /* If there's no free space manager for objects of this type,
         *  see if we can avoid creating one by checking if the freed
         *  space is at the end of the file
         */
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: f->shared->fs_addr[%u] = %a\n", FUNC, (unsigned)fs_type, f->shared->fs_addr[fs_type]);
#endif /* H5MF_ALLOC_DEBUG_MORE */
        if(!H5F_addr_defined(f->shared->fs_addr[fs_type])) {
            htri_t status;          /* "can absorb" status for section into */

#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Trying to avoid starting up free space manager\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
            /* Try to shrink the file or absorb the block into a block aggregator */
            if((status = H5MF_try_shrink(f, alloc_type, dxpl_id, addr, size)) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't check for absorbing block")
            else if(status > 0)
                /* Indicate success */
                HGOTO_DONE(SUCCEED)
        } /* end if */

        /* If we are deleting the free space manager, leave now, to avoid
         *  [re-]starting it.
         *
         * Note: this drops the space to free on the floor...
         *
         */
        if(f->shared->fs_state[fs_type] == H5F_FS_STATE_DELETING)
{
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: dropping addr = %a, size = %Hu, on the floor!\n", FUNC, addr, size);
#endif /* H5MF_ALLOC_DEBUG_MORE */
            HGOTO_DONE(SUCCEED)
}

        /* There's either already a free space manager, or the freed
         *  space isn't at the end of the file, so start up (or create)
         *  the file space manager
         */
        if(H5MF_alloc_start(f, dxpl_id, fs_type, TRUE) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize file free space")
    } /* end if */
    HDassert(f->shared->fs_man[fs_type]);

    /* Create free space section for block */
    if(NULL == (node = H5MF_sect_simple_new(addr, size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space section")

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.alloc_type = alloc_type;
    udata.allow_sect_absorb = TRUE;

    /* Add to the free space for the file */
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Before H5FS_sect_add()\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
    if(H5FS_sect_add(f, dxpl_id, f->shared->fs_man[fs_type], (H5FS_section_info_t *)node, H5FS_ADD_RETURNED_SPACE, &udata) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINSERT, FAIL, "can't add section to file free space")
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: After H5FS_sect_add()\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
    node = NULL;

done:
    if(ret_value < 0 && node)
        /* On error, free section node allocated */
        if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't free simple section node")

#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif /* H5MF_ALLOC_DEBUG */
#ifdef H5MF_ALLOC_DEBUG_DUMP
H5MF_sects_dump(f, dxpl_id, stderr);
#endif /* H5MF_ALLOC_DEBUG_DUMP */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_xfree() */


/*-------------------------------------------------------------------------
 * Function:	H5MF_try_extend
 *
 * Purpose:	Extend a block in the file if possible.
 *
 * Return:	Success:	TRUE(1)  - Block was extended
 *                              FALSE(0) - Block could not be extended
 * 		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *              Friday, June 11, 2004
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5MF_try_extend(H5F_t *f, hid_t dxpl_id, H5FD_mem_t alloc_type, haddr_t addr,
    hsize_t size, hsize_t extra_requested)
{
    haddr_t     end;            /* End of block to extend */
    htri_t	ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5MF_try_extend, FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering: alloc_type = %u, addr = %a, size = %Hu, extra_requested = %Hu\n", FUNC, (unsigned)alloc_type, addr, size, extra_requested);
#endif /* H5MF_ALLOC_DEBUG */

    /* Compute end of block to extend */
    end = addr + size;

    /* Check if the block is exactly at the end of the file */
    if((ret_value = H5FD_try_extend(f->shared->lf, alloc_type, end, extra_requested)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "error extending file")
    else if(ret_value == FALSE) {
        H5F_blk_aggr_t *aggr;   /* Aggregator to use */

        /* Check for test block able to extend aggregation block */
        aggr = (alloc_type == H5FD_MEM_DRAW) ?  &(f->shared->sdata_aggr) : &(f->shared->meta_aggr);
        if((ret_value = H5MF_aggr_try_extend(f, aggr, alloc_type, end, extra_requested)) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "error extending aggregation block")
        else if(ret_value == FALSE) {
            H5FD_mem_t  fs_type;                /* Free space type (mapped from allocation type) */

            /* Get free space type from allocation type */
            fs_type = H5MF_ALLOC_TO_FS_TYPE(f, alloc_type);

            /* Check if the free space for the file has been initialized */
            if(!f->shared->fs_man[fs_type])
                if(H5MF_alloc_start(f, dxpl_id, fs_type, FALSE) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize file free space")

            /* Check for test block able to block in free space manager */
            if(f->shared->fs_man[fs_type])
                if((ret_value = H5FS_sect_try_extend(f, dxpl_id, f->shared->fs_man[fs_type], addr, size, extra_requested)) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "error extending block in free space manager")
        } /* end if */
    } /* end if */

done:
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving: ret_value = %t\n", FUNC, ret_value);
#endif /* H5MF_ALLOC_DEBUG */
#ifdef H5MF_ALLOC_DEBUG_DUMP
H5MF_sects_dump(f, dxpl_id, stderr);
#endif /* H5MF_ALLOC_DEBUG_DUMP */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_try_extend() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_get_freespace
 *
 * Purpose:     Retrieve the amount of free space in a file.
 *
 * Return:      Success:        Amount of free space in file
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Monday, October  6, 2003
 *
 *-------------------------------------------------------------------------
 */
hssize_t
H5MF_get_freespace(H5F_t *f, hid_t dxpl_id)
{
    haddr_t eoa;                /* End of allocated space in the file */
    haddr_t ma_addr = HADDR_UNDEF;    /* Base "metadata aggregator" address */
    hsize_t ma_size = 0;        /* Size of "metadata aggregator" */
    haddr_t sda_addr = HADDR_UNDEF;    /* Base "small data aggregator" address */
    hsize_t sda_size = 0;       /* Size of "small data aggregator" */
    hsize_t tot_fs_size = 0;    /* Amount of all free space managed */
    H5FD_mem_t type;            /* Memory type for iteration */
    hssize_t ret_value;         /* Return value */

    FUNC_ENTER_NOAPI(H5MF_get_freespace, FAIL)

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);

    /* Retrieve the 'eoa' for the file */
    if(HADDR_UNDEF == (eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_DEFAULT)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "driver get_eoa request failed")

    /* Retrieve metadata aggregator info, if available */
    H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

    /* Retrieve 'small data' aggregator info, if available */
    H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sda_addr, &sda_size);

    /* Iterate over all the free space types that have managers and get each free list's space */
    for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
        hsize_t type_fs_size = 0;    /* Amount of free space managed for each type */

        /* Check if the free space for the file has been initialized */
        if(!f->shared->fs_man[type])
            if(H5MF_alloc_start(f, dxpl_id, type, FALSE) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize file free space")

        /* Retrieve free space size from free space manager */
        if(f->shared->fs_man[type])
            if((ret_value = H5FS_sect_stats(f->shared->fs_man[type], &type_fs_size, NULL)) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't query free space stats")

        /* Increment total free space for types */
        tot_fs_size += type_fs_size;
    } /* end for */

    /* Start computing value to return */
    ret_value = tot_fs_size;

    /* Check for aggregating metadata allocations */
    if(ma_size > 0) {
        /* Add in the reserved space for metadata to the available free space */
        /* (if it's not at the tail of the file) */
        if(H5F_addr_ne(ma_addr + ma_size, eoa))
            ret_value += ma_size;
    } /* end if */

    /* Check for aggregating small data allocations */
    if(sda_size > 0) {
        /* Add in the reserved space for metadata to the available free space */
        /* (if it's not at the tail of the file) */
        if(H5F_addr_ne(sda_addr + sda_size, eoa))
            ret_value += sda_size;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_get_freespace() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_can_shrink
 *
 * Purpose:     Try to shrink the size of a file with a block or absorb it
 *              into a block aggregator.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Feb 14 2008
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5MF_try_shrink(H5F_t *f, H5FD_mem_t alloc_type, hid_t dxpl_id, haddr_t addr,
    hsize_t size)
{
    H5MF_free_section_t *node = NULL;   /* Free space section pointer */
    H5MF_sect_ud_t udata;               /* User data for callback */
    htri_t ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI(H5MF_try_shrink, FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering - alloc_type = %u, addr = %a, size = %Hu\n", FUNC, (unsigned)alloc_type, addr, size);
#endif /* H5MF_ALLOC_DEBUG */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(size > 0);

    /* Create free space section for block */
    if(NULL == (node = H5MF_sect_simple_new(addr, size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space section")

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.alloc_type = alloc_type;
    udata.allow_sect_absorb = FALSE;    /* Force section to be absorbed into aggregator */

    /* Call the "can shrink" callback for the section */
    if((ret_value = H5MF_sect_simple_can_shrink((const H5FS_section_info_t *)node, &udata)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTMERGE, FAIL, "can't check if section can shrink container")
    else if(ret_value > 0) {
        /* Shrink or absorb the section */
        if(H5MF_sect_simple_shrink((H5FS_section_info_t **)&node, &udata) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSHRINK, FAIL, "can't shrink container")
    } /* end if */

done:
    /* Free section node allocated */
    if(node && H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
        HDONE_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't free simple section node")

#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif /* H5MF_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_try_shrink() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_close
 *
 * Purpose:     Close the free space tracker(s) for a file
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, January 22, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_close(H5F_t *f, hid_t dxpl_id)
{
    H5FD_mem_t type;                    /* Memory type for iteration */
    H5F_blk_aggr_t *first_aggr;         /* First aggregator to reset */
    H5F_blk_aggr_t *second_aggr;        /* Second aggregator to reset */
    haddr_t ma_addr = HADDR_UNDEF;      /* Base "metadata aggregator" address */
    hsize_t ma_size = 0;                /* Size of "metadata aggregator" */
    haddr_t sda_addr = HADDR_UNDEF;     /* Base "small data aggregator" address */
    hsize_t sda_size = 0;               /* Size of "small data aggregator" */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5MF_close, FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG */

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);

    /* Iterate over all the free space types that have managers and get each free list's space */
    for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Check 1.0 - f->shared->fs_man[%u] = %p, f->shared->fs_addr[%u] = %a\n", FUNC, (unsigned)type, f->shared->fs_man[type], (unsigned)type, f->shared->fs_addr[type]);
#endif /* H5MF_ALLOC_DEBUG_MORE */
        /* If the free space manager for this type is open, close it */
        if(f->shared->fs_man[type]) {
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Before closing free space manager\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
            if(H5FS_close(f, dxpl_id, f->shared->fs_man[type]) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release free space info")
            f->shared->fs_man[type] = NULL;
            f->shared->fs_state[type] = H5F_FS_STATE_CLOSED;
        } /* end if */
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Check 2.0 - f->shared->fs_man[%u] = %p, f->shared->fs_addr[%u] = %a\n", FUNC, (unsigned)type, f->shared->fs_man[type], (unsigned)type, f->shared->fs_addr[type]);
#endif /* H5MF_ALLOC_DEBUG_MORE */

        /* If there is free space manager info for this type, delete it */
        /* (XXX: Make this optional when free space for a file can be persistant) */
        if(H5F_addr_defined(f->shared->fs_addr[type])) {
            haddr_t tmp_fs_addr;            /* Temporary holder for free space manager address */

            /* Put address into temporary variable and reset it */
            /* (Avoids loopback in file space freeing routine) */
            tmp_fs_addr = f->shared->fs_addr[type];
            f->shared->fs_addr[type] = HADDR_UNDEF;

            /* Shift to "deleting" state, to make certain we don't track any
             *  file space freed as a result of deleting the free space manager.
             */
            f->shared->fs_state[type] = H5F_FS_STATE_DELETING;

#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Before deleting free space manager\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
            /* Delete free space manager for this type */
            if(H5FS_delete(f, dxpl_id, tmp_fs_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "can't delete free space manager")

            /* Shift [back] to closed state */
            HDassert(f->shared->fs_state[type] == H5F_FS_STATE_DELETING);
            f->shared->fs_state[type] = H5F_FS_STATE_CLOSED;

            /* Sanity check that the free space manager for this type wasn't started up again */
            HDassert(!H5F_addr_defined(f->shared->fs_addr[type]));
        } /* end if */
    } /* end for */

    /* Retrieve metadata aggregator info, if available */
    H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: ma_addr = %a, ma_size = %Hu, end of ma = %a\n", FUNC, ma_addr, ma_size, (haddr_t)((ma_addr + ma_size) - 1));
#endif /* H5MF_ALLOC_DEBUG_MORE */

    /* Retrieve 'small data' aggregator info, if available */
    H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sda_addr, &sda_size);
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: sda_addr = %a, sda_size = %Hu, end of sda = %a\n", FUNC, sda_addr, sda_size, (haddr_t)((sda_addr + sda_size) - 1));
#endif /* H5MF_ALLOC_DEBUG_MORE */

    /* Make certain we release the aggregator that's later in the file first */
    /* (so the file shrinks properly) */
    if(H5F_addr_defined(ma_addr) && H5F_addr_defined(sda_addr)) {
        if(H5F_addr_lt(ma_addr, sda_addr)) {
            first_aggr = &(f->shared->sdata_aggr);
            second_aggr = &(f->shared->meta_aggr);
        } /* end if */
        else {
            first_aggr = &(f->shared->meta_aggr);
            second_aggr = &(f->shared->sdata_aggr);
        } /* end else */
    } /* end if */
    else {
        first_aggr = &(f->shared->meta_aggr);
        second_aggr = &(f->shared->sdata_aggr);
    } /* end else */

     /* Release the unused portion of the metadata and "small data" blocks back
      * to the free lists in the file.
      */
    if(H5MF_aggr_reset(f, dxpl_id, first_aggr) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFREE, FAIL, "can't reset metadata block")
    if(H5MF_aggr_reset(f, dxpl_id, second_aggr) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFREE, FAIL, "can't reset 'small data' block")

done:
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_close() */

