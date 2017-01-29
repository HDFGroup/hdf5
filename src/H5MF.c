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

#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */
#include "H5MFmodule.h"         /* This source code file is part of the H5MF module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MFpkg.h"		/* File memory management		*/
#include "H5VMprivate.h"	/* Vectors and arrays 			*/


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

/* User data for section info iterator callback for iterating over free space sections */
typedef struct {
    H5F_sect_info_t *sects;     /* section info to be retrieved */
    size_t sect_count;          /* # of sections requested */
    size_t sect_idx;            /* the current count of sections */
} H5MF_sect_iter_ud_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Allocator routines */
static herr_t H5MF_alloc_create(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type);
static herr_t H5MF__alloc_close(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type);
static herr_t H5MF__close_delete(H5F_t *f, hid_t dxpl_id, H5P_genplist_t **dxpl);
static herr_t H5MF__close_shrink_eoa(H5F_t *f, hid_t dxpl_id);


/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;


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
    herr_t ret_value = SUCCEED;        	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

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
                /* (global heap is treated as raw data) */
                if(type != H5FD_MEM_DRAW && type != H5FD_MEM_GHEAP) {
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
            /* (treat global heaps as raw data) */
            if(H5FD_MEM_DRAW == f->shared->fs_type_map[H5FD_MEM_DRAW] ||
                    H5FD_MEM_DEFAULT == f->shared->fs_type_map[H5FD_MEM_DRAW]) {
                f->shared->fs_aggr_merge[H5FD_MEM_DRAW] = H5F_FS_MERGE_RAWDATA;
                f->shared->fs_aggr_merge[H5FD_MEM_GHEAP] = H5F_FS_MERGE_RAWDATA;
	    } /* end if */
            break;

        case H5MF_AGGR_MERGE_DICHOTOMY:
            /* Merge all metadata together (but not raw data) */
            HDmemset(f->shared->fs_aggr_merge, H5F_FS_MERGE_METADATA, sizeof(f->shared->fs_aggr_merge));

            /* Allow merging raw data allocations together */
            /* (treat global heaps as raw data) */
            f->shared->fs_aggr_merge[H5FD_MEM_DRAW] = H5F_FS_MERGE_RAWDATA;
            f->shared->fs_aggr_merge[H5FD_MEM_GHEAP] = H5F_FS_MERGE_RAWDATA;
            break;

        case H5MF_AGGR_MERGE_TOGETHER:
            /* Merge all allocation types together */
            HDmemset(f->shared->fs_aggr_merge, (H5F_FS_MERGE_METADATA | H5F_FS_MERGE_RAWDATA), sizeof(f->shared->fs_aggr_merge));
            break;

        default:
            HGOTO_ERROR(H5E_RESOURCE, H5E_BADVALUE, FAIL, "invalid mapping type")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_init_merge_flags() */


/*-------------------------------------------------------------------------
 * Function:	H5MF__alloc_open
 *
 * Purpose:	Open an existing free space manager of TYPE for file by
 *		creating a free-space structure
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jan  8 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF__alloc_open(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type)
{
    const H5FS_section_class_t *classes[] = { /* Free space section classes implemented for file */
        H5MF_FSPACE_SECT_CLS_SIMPLE};
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t fsm_ring;               /* Free space manager ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    hbool_t reset_ring = FALSE;         /* Whether the ring was set */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(f->shared);
    HDassert(type != H5FD_MEM_NOLIST);
    HDassert(H5F_addr_defined(f->shared->fs_addr[type]));
    HDassert(f->shared->fs_state[type] == H5F_FS_STATE_CLOSED);
    HDassert(type == H5MF_ALLOC_TO_FS_TYPE(f, type));

    /* Set the ring type in the DXPL */
    if((type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
            || (type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
        fsm_ring = H5AC_RING_MDFSM;
    else
        fsm_ring = H5AC_RING_RDFSM;
    if(H5AC_set_ring(dxpl_id, fsm_ring, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value")
    reset_ring = TRUE;

    /* Open an existing free space structure for the file */
    if(NULL == (f->shared->fs_man[type] = H5FS_open(f, dxpl_id, f->shared->fs_addr[type],
	    NELMTS(classes), classes, f, f->shared->alignment, f->shared->threshold)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space info")

    /* Set the state for the free space manager to "open", if it is now */
    if(f->shared->fs_man[type])
        f->shared->fs_state[type] = H5F_FS_STATE_OPEN;

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set property value")

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5MF__alloc_open() */


/*-------------------------------------------------------------------------
 * Function:	H5MF_alloc_create
 *
 * Purpose:	Create free space manager of TYPE for the file by creating
 *		a free-space structure
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jan  8 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5MF_alloc_create(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type)
{
    const H5FS_section_class_t *classes[] = { /* Free space section classes implemented for file */
        H5MF_FSPACE_SECT_CLS_SIMPLE};
    herr_t ret_value = SUCCEED;         /* Return value */
    H5FS_create_t fs_create; 		/* Free space creation parameters */

    FUNC_ENTER_NOAPI_NOINIT_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(f->shared);
    HDassert(type != H5FD_MEM_NOLIST);
    HDassert(!H5F_addr_defined(f->shared->fs_addr[type]));
    HDassert(f->shared->fs_state[type] == H5F_FS_STATE_CLOSED);

    /* Set the free space creation parameters */
    fs_create.client = H5FS_CLIENT_FILE_ID;
    fs_create.shrink_percent = H5MF_FSPACE_SHRINK;
    fs_create.expand_percent = H5MF_FSPACE_EXPAND;
    fs_create.max_sect_addr = 1 + H5VM_log2_gen((uint64_t)f->shared->maxaddr);
    fs_create.max_sect_size = f->shared->maxaddr;

    if(NULL == (f->shared->fs_man[type] = H5FS_create(f, dxpl_id, NULL,
	    &fs_create, NELMTS(classes), classes, f, f->shared->alignment, f->shared->threshold)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space info")


    /* Set the state for the free space manager to "open", if it is now */
    if(f->shared->fs_man[type])
        f->shared->fs_state[type] = H5F_FS_STATE_OPEN;

done:
    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5MF_alloc_create() */


/*-------------------------------------------------------------------------
 * Function:	H5MF__alloc_start
 *
 * Purpose:	Open or create a free space manager of a given type
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jan  8 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF__alloc_start(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(f->shared);
    HDassert(type != H5FD_MEM_NOLIST);

    /* Check if the free space manager exists already */
    if(H5F_addr_defined(f->shared->fs_addr[type])) {
        /* Open existing free space manager */
        if(H5MF__alloc_open(f, dxpl_id, type) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTOPENOBJ, FAIL, "can't initialize file free space")
    } /* end if */
    else {
        /* Create new free space manager */
        if(H5MF_alloc_create(f, dxpl_id, type) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCREATE, FAIL, "can't initialize file free space")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF__alloc_start() */


/*-------------------------------------------------------------------------
 * Function:    H5MF__alloc_close
 *
 * Purpose:     Close an existing free space manager of TYPE for file
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer: Vailin Choi; July 1st, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5MF__alloc_close(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(f->shared);
    HDassert(type != H5FD_MEM_NOLIST);
    HDassert(f->shared->fs_man[type]);
    HDassert(f->shared->fs_state[type] != H5F_FS_STATE_CLOSED);

    /* Close an existing free space structure for the file */
    if(H5FS_close(f, dxpl_id, f->shared->fs_man[type]) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release free space info")
    f->shared->fs_man[type] = NULL;
    f->shared->fs_state[type] = H5F_FS_STATE_CLOSED;

done:
    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5MF__alloc_close() */


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
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t fsm_ring = H5AC_RING_INV;       /* free space manager ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    H5FD_mem_t  fs_type;                /* Free space type (mapped from allocation type) */
    hbool_t reset_ring = FALSE;         /* Whether the ring was set */
    haddr_t ret_value = HADDR_UNDEF;    /* Return value */

    FUNC_ENTER_NOAPI_TAG(dxpl_id, H5AC__FREESPACE_TAG, HADDR_UNDEF)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: alloc_type = %u, size = %Hu\n", FUNC, (unsigned)alloc_type, size);
#endif /* H5MF_ALLOC_DEBUG */

    /* check arguments */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(size > 0);

    /* Get free space type from allocation type */
    fs_type = H5MF_ALLOC_TO_FS_TYPE(f, alloc_type);

    /* Set the ring type in the DXPL */
    if((fs_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
            || (fs_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
        fsm_ring = H5AC_RING_MDFSM;
    else
        fsm_ring = H5AC_RING_RDFSM;
    if(H5AC_set_ring(dxpl_id, fsm_ring, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, HADDR_UNDEF, "unable to set ring value")
    reset_ring = TRUE;

    /* Check if we are using the free space manager for this file */
    if(H5F_HAVE_FREE_SPACE_MANAGER(f)) {
        /* Check if the free space manager for the file has been initialized */
        if(!f->shared->fs_man[fs_type] && H5F_addr_defined(f->shared->fs_addr[fs_type]))
            if(H5MF__alloc_open(f, dxpl_id, fs_type) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTOPENOBJ, HADDR_UNDEF, "can't initialize file free space")

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
		    udata.allow_eoa_shrink_only = FALSE; 

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
    } /* end if */

    /* Allocate from the metadata aggregator (or the VFD) */
    if(HADDR_UNDEF == (ret_value = H5MF_aggr_vfd_alloc(f, alloc_type, dxpl_id, size)))
	HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, HADDR_UNDEF, "allocation failed from aggr/vfd")

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, HADDR_UNDEF, "unable to set property value")

#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving: ret_value = %a, size = %Hu\n", FUNC, ret_value, size);
#endif /* H5MF_ALLOC_DEBUG */
#ifdef H5MF_ALLOC_DEBUG_DUMP
H5MF_sects_dump(f, dxpl_id, stderr);
#endif /* H5MF_ALLOC_DEBUG_DUMP */

    FUNC_LEAVE_NOAPI_TAG(ret_value, HADDR_UNDEF)
} /* end H5MF_alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_alloc_tmp
 *
 * Purpose:     Allocate temporary space in the file
 *
 * Note:	The address returned is non-overlapping with any other address
 *		in the file and suitable for insertion into the metadata
 *		cache.
 *
 *		The address is _not_ suitable for actual file I/O and will
 *		cause an error if it is so used.
 *
 *		The space allocated with this routine should _not_ be freed,
 *		it should just be abandoned.  Calling H5MF_xfree() with space
 *              from this routine will cause an error.
 *
 * Return:      Success:        Temporary file address
 *              Failure:        HADDR_UNDEF
 *
 * Programmer:  Quincey Koziol
 *              Thursday, June  4, 2009
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5MF_alloc_tmp(H5F_t *f, hsize_t size)
{
    haddr_t eoa;                /* End of allocated space in the file */
    haddr_t ret_value = HADDR_UNDEF;    /* Return value */

    FUNC_ENTER_NOAPI(HADDR_UNDEF)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: size = %Hu\n", FUNC, size);
#endif /* H5MF_ALLOC_DEBUG */

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(size > 0);

    /* Retrieve the 'eoa' for the file */
    if(HADDR_UNDEF == (eoa = H5F_get_eoa(f, H5FD_MEM_DEFAULT)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, HADDR_UNDEF, "driver get_eoa request failed")

    /* Compute value to return */
    ret_value = f->shared->tmp_addr - size;

    /* Check for overlap into the actual allocated space in the file */
    if(H5F_addr_le(ret_value, eoa))
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, HADDR_UNDEF, "driver get_eoa request failed")

    /* Adjust temporary address allocator in the file */
    f->shared->tmp_addr = ret_value;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_alloc_tmp() */


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
    H5F_io_info_t fio_info;             /* I/O info for operation */
    H5MF_free_section_t *node = NULL;   /* Free space section pointer */
    H5MF_sect_ud_t udata;               /* User data for callback */
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t fsm_ring;               /* Free space manager ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    hbool_t reset_ring = FALSE;         /* Whether the ring was set */
    H5FD_mem_t fs_type;                 /* Free space type (mapped from allocation type) */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering - alloc_type = %u, addr = %a, size = %Hu\n", FUNC, (unsigned)alloc_type, addr, size);
#endif /* H5MF_ALLOC_DEBUG */

    /* check arguments */
    HDassert(f);
    if(!H5F_addr_defined(addr) || 0 == size)
        HGOTO_DONE(SUCCEED);
    HDassert(addr != 0);        /* Can't deallocate the superblock :-) */

    /* Check for attempting to free space that's a 'temporary' file address */
    if(H5F_addr_le(f->shared->tmp_addr, addr))
        HGOTO_ERROR(H5E_RESOURCE, H5E_BADRANGE, FAIL, "attempting to free temporary file space")

    /* Get free space type from allocation type */
    fs_type = H5MF_ALLOC_TO_FS_TYPE(f, alloc_type);
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: fs_type = %u\n", FUNC, (unsigned)fs_type);
#endif /* H5MF_ALLOC_DEBUG_MORE */

    /* Set the ring type in the DXPL */
    if((fs_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
            || (fs_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
        fsm_ring = H5AC_RING_MDFSM;
    else
        fsm_ring = H5AC_RING_RDFSM;
    if(H5AC_set_ring(dxpl_id, fsm_ring, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value")
    reset_ring = TRUE;

    /* Set up I/O info for operation */
    fio_info.f = f;
    if(NULL == (fio_info.dxpl = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get property list")

    /* Check if the space to free intersects with the file's metadata accumulator */
    if(H5F__accum_free(&fio_info, alloc_type, addr, size) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "can't check free space intersection w/metadata accumulator")

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
	    else if(size < f->shared->fs_threshold) {
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: dropping addr = %a, size = %Hu, on the floor!\n", FUNC, addr, size);
#endif /* H5MF_ALLOC_DEBUG_MORE */
		HGOTO_DONE(SUCCEED)
	    } /* end else-if */
        } /* end if */

        /* If we are deleting the free space manager, leave now, to avoid
         *  [re-]starting it.
	 * or if file space strategy type is not using a free space manager
	 *   (H5F_FILE_SPACE_AGGR_VFD or H5F_FILE_SPACE_VFD), drop free space
         *   section on the floor.
         *
         * Note: this drops the space to free on the floor...
         *
         */
        if(f->shared->fs_state[fs_type] == H5F_FS_STATE_DELETING ||
	        !H5F_HAVE_FREE_SPACE_MANAGER(f)) {
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: dropping addr = %a, size = %Hu, on the floor!\n", FUNC, addr, size);
#endif /* H5MF_ALLOC_DEBUG_MORE */
            HGOTO_DONE(SUCCEED)
        } /* end if */

        /* There's either already a free space manager, or the freed
         *  space isn't at the end of the file, so start up (or create)
         *  the file space manager
         */
        if(H5MF__alloc_start(f, dxpl_id, fs_type) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize file free space")
    } /* end if */

    /* Create free space section for block */
    if(NULL == (node = H5MF_sect_simple_new(addr, size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space section")

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.alloc_type = alloc_type;
    udata.allow_sect_absorb = TRUE;
    udata.allow_eoa_shrink_only = FALSE; 

    /* If size of section freed is larger than threshold, add it to the free space manager */
    if(size >= f->shared->fs_threshold) {
	HDassert(f->shared->fs_man[fs_type]);

#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Before H5FS_sect_add()\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
        /* Add to the free space for the file */
	if(H5FS_sect_add(f, dxpl_id, f->shared->fs_man[fs_type], (H5FS_section_info_t *)node, H5FS_ADD_RETURNED_SPACE, &udata) < 0)
	    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINSERT, FAIL, "can't add section to file free space")
	node = NULL;
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: After H5FS_sect_add()\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
    } /* end if */
    else {
        htri_t merged;          /* Whether node was merged */

        /* Try to merge the section that is smaller than threshold */
	if((merged = H5FS_sect_try_merge(f, dxpl_id, f->shared->fs_man[fs_type], (H5FS_section_info_t *)node, H5FS_ADD_RETURNED_SPACE, &udata)) < 0)
	    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINSERT, FAIL, "can't merge section to file free space")
	else if(merged == TRUE) /* successfully merged */
	    /* Indicate that the node was used */
            node = NULL;
    } /* end else */

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set property value")

    /* Release section node, if allocated and not added to section list or merged */
    if(node)
        if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't free simple section node")

#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif /* H5MF_ALLOC_DEBUG */
#ifdef H5MF_ALLOC_DEBUG_DUMP
H5MF_sects_dump(f, dxpl_id, stderr);
#endif /* H5MF_ALLOC_DEBUG_DUMP */
    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
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
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t fsm_ring;               /* Free space manager ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    haddr_t     end;                    /* End of block to extend */
    H5FD_mem_t fs_type;			/* Memory type of the free space manager */
    H5FD_mem_t  map_type;               /* Mapped type */
    hbool_t reset_ring = FALSE;         /* Whether the ring was set */
    htri_t	ret_value = FAIL;       /* Return value */

    FUNC_ENTER_NOAPI_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering: alloc_type = %u, addr = %a, size = %Hu, extra_requested = %Hu\n", FUNC, (unsigned)alloc_type, addr, size, extra_requested);
#endif /* H5MF_ALLOC_DEBUG */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_INTENT(f) & H5F_ACC_RDWR);

    /* Set mapped type, treating global heap as raw data */
    map_type = (alloc_type == H5FD_MEM_GHEAP) ? H5FD_MEM_DRAW : alloc_type;

    /* Compute end of block to extend */
    end = addr + size;

    /* Get free space type from allocation type */
    fs_type = H5MF_ALLOC_TO_FS_TYPE(f, alloc_type);

    /* Set the ring type in the DXPL */
    if((fs_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
            || (fs_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
        fsm_ring = H5AC_RING_MDFSM;
    else
        fsm_ring = H5AC_RING_RDFSM;
    if(H5AC_set_ring(dxpl_id, fsm_ring, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value")
    reset_ring = TRUE;

    /* Check if the block is exactly at the end of the file */
    if((ret_value = H5FD_try_extend(f->shared->lf, map_type, f, dxpl_id, end, extra_requested)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "error extending file")
    else if(ret_value == FALSE) {
        H5F_blk_aggr_t *aggr;   /* Aggregator to use */

        /* Check for test block able to extend aggregation block */
        aggr = (map_type == H5FD_MEM_DRAW) ?  &(f->shared->sdata_aggr) : &(f->shared->meta_aggr);
        if((ret_value = H5MF_aggr_try_extend(f, dxpl_id, aggr, map_type, end, extra_requested)) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "error extending aggregation block")
        else if(ret_value == FALSE) {

            /* Check if the free space for the file has been initialized */
            if(!f->shared->fs_man[fs_type] && H5F_addr_defined(f->shared->fs_addr[fs_type]))
                if(H5MF__alloc_open(f, dxpl_id, fs_type) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize file free space")

            /* Check for test block able to block in free space manager */
            if(f->shared->fs_man[fs_type])
                if((ret_value = H5FS_sect_try_extend(f, dxpl_id, f->shared->fs_man[fs_type], addr, size, extra_requested)) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "error extending block in free space manager")
        } /* end if */
    } /* end else-if */

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set property value")

#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving: ret_value = %t\n", FUNC, ret_value);
#endif /* H5MF_ALLOC_DEBUG */
#ifdef H5MF_ALLOC_DEBUG_DUMP
H5MF_sects_dump(f, dxpl_id, stderr);
#endif /* H5MF_ALLOC_DEBUG_DUMP */

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
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
 * Modifications:
 *      Vailin Choi; July 2012
 *      As the default free-list mapping is changed to H5FD_FLMAP_DICHOTOMY,
 *      checks are added to account for the last section of each free-space manager
 *      and the remaining space in the two aggregators are at EOF.
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_get_freespace(H5F_t *f, hid_t dxpl_id, hsize_t *tot_space, hsize_t *meta_size)
{
    H5P_genplist_t *dxpl = NULL; /* DXPL for setting ring */
    H5AC_ring_t curr_ring;      /* Current ring value */
    H5AC_ring_t needed_ring;    /* Ring value needed for loop iteration  */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    haddr_t eoa;                /* End of allocated space in the file */
    haddr_t ma_addr = HADDR_UNDEF;    /* Base "metadata aggregator" address */
    hsize_t ma_size = 0;        /* Size of "metadata aggregator" */
    haddr_t sda_addr = HADDR_UNDEF;    /* Base "small data aggregator" address */
    hsize_t sda_size = 0;       /* Size of "small data aggregator" */
    hsize_t tot_fs_size = 0;    /* Amount of all free space managed */
    hsize_t tot_meta_size = 0;  /* Amount of metadata for free space managers */
    H5FD_mem_t type;            /* Memory type for iteration */
    hbool_t fs_started[H5FD_MEM_NTYPES]; /* Indicate whether the free-space manager has been started */
    hbool_t eoa_shrank;		/* Whether an EOA shrink occurs */
    hbool_t reset_ring = FALSE; /* Whether the ring was set */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);

    /* Retrieve the 'eoa' for the file */
    if(HADDR_UNDEF == (eoa = H5F_get_eoa(f, H5FD_MEM_DEFAULT)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "driver get_eoa request failed")

    /* Set the ring type in the DXPL */
    if(H5AC_set_ring(dxpl_id, H5AC_RING_RDFSM, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value")
    reset_ring = TRUE;
    curr_ring = H5AC_RING_RDFSM;

    /* Retrieve metadata aggregator info, if available */
    if(H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't query metadata aggregator stats")

    /* Retrieve 'small data' aggregator info, if available */
    if(H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sda_addr, &sda_size) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't query small data aggregator stats")

    /* Iterate over all the free space types that have managers and get each free list's space */
    for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {

	fs_started[type] = FALSE;

        /* test to see if we need to switch rings -- do so if required */
        if((type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
                || (type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
            needed_ring = H5AC_RING_MDFSM;
        else
            needed_ring = H5AC_RING_RDFSM;

        if(needed_ring != curr_ring) {
            if(H5AC_set_ring(dxpl_id, needed_ring, &dxpl, &curr_ring) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value (2)")
            curr_ring = needed_ring;
        } /* end if */

	/* Check if the free space for the file has been initialized */
        if(!f->shared->fs_man[type] && H5F_addr_defined(f->shared->fs_addr[type])) {
            if(H5MF__alloc_open(f, dxpl_id, type) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize file free space")
            HDassert(f->shared->fs_man[type]);
            fs_started[type] = TRUE;
        } /* end if */

	/* Check if there's free space of this type */
        if(f->shared->fs_man[type]) {
            hsize_t type_fs_size = 0;    /* Amount of free space managed for each type */
            hsize_t type_meta_size = 0;  /* Amount of free space metadata for each type */

            /* Retrieve free space size from free space manager */
            if(H5FS_sect_stats(f->shared->fs_man[type], &type_fs_size, NULL) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't query free space stats")
            if(H5FS_size(f, f->shared->fs_man[type], &type_meta_size) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't query free space metadata stats")

            /* Increment total free space for types */
            tot_fs_size += type_fs_size;
            tot_meta_size += type_meta_size;
	} /* end if */
    } /* end for */

    /* Iterate until no more EOA shrink occurs */
    do {
	eoa_shrank = FALSE;

	/* Check the last section of each free-space manager */
	for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
	    haddr_t sect_addr = HADDR_UNDEF;
	    hsize_t sect_size = 0;

	    if(f->shared->fs_man[type]) {
		if(H5FS_sect_query_last_sect(f->shared->fs_man[type], &sect_addr, &sect_size) < 0)
		    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't query last section on merge list")

		/* Deduct space from previous accumulation if the section is at EOA */
		if(H5F_addr_eq(sect_addr + sect_size, eoa)) {
		    eoa = sect_addr;
		    eoa_shrank = TRUE;
		    tot_fs_size -= sect_size;
		} /* end if */
	    } /* end if */
	} /* end for */

	/* Check the metadata and raw data aggregators */
	if(ma_size > 0 && H5F_addr_eq(ma_addr + ma_size, eoa)) {
	    eoa = ma_addr;
	    eoa_shrank = TRUE;
	    ma_size = 0;
	} /* end if */
	if(sda_size > 0 && H5F_addr_eq(sda_addr + sda_size, eoa)) {
	    eoa = sda_addr;
	    eoa_shrank = TRUE;
	    sda_size = 0;
	} /* end if */
    } while(eoa_shrank);

    /* Close the free-space managers if they were opened earlier in this routine */
    for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
        if(fs_started[type]) {

            /* test to see if we need to switch rings -- do so if required */
            if((type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
                    || (type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
                needed_ring = H5AC_RING_MDFSM;
            else
                needed_ring = H5AC_RING_RDFSM;

            if(needed_ring != curr_ring) {
                if(H5AC_set_ring(dxpl_id, needed_ring, &dxpl, &curr_ring) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value (3)")
                curr_ring = needed_ring;
            } /* end if */

            if(H5MF__alloc_close(f, dxpl_id, type) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't close file free space")
        } /* end if */
    } /* end for */

    /* Set the value(s) to return */
    /* (The metadata & small data aggregators count as free space now, since they aren't at EOA) */
    if(tot_space)
	*tot_space = tot_fs_size + ma_size + sda_size;
    if(meta_size)
	*meta_size = tot_meta_size;

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set property value")

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5MF_get_freespace() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_try_shrink
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
    htri_t ret_value = FAIL;                   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering - alloc_type = %u, addr = %a, size = %Hu\n", FUNC, (unsigned)alloc_type, addr, size);
#endif /* H5MF_ALLOC_DEBUG */

    /* check arguments */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
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
    udata.allow_eoa_shrink_only = FALSE; 

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
 * Function:    H5MF__close_shrink_eoa
 *
 * Purpose:     Shrink the EOA while closing
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Saturday, July 7, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5MF__close_shrink_eoa(H5F_t *f, hid_t dxpl_id)
{
    H5FD_mem_t type;            /* Memory type for iteration */
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    H5AC_ring_t curr_ring = H5AC_RING_INV;      /* current ring value */
    H5AC_ring_t needed_ring = H5AC_RING_INV;    /* ring value needed for this
                                                 * iteration.
                                                 */
    hbool_t eoa_shrank;		/* Whether an EOA shrink occurs */
    hbool_t reset_ring = FALSE; /* Whether the ring was set */
    htri_t status;		/* Status value */
    H5MF_sect_ud_t udata;	/* User data for callback */
    herr_t ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_STATIC_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)

    /* check args */
    HDassert(f);
    HDassert(f->shared);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.allow_sect_absorb = FALSE;    
    udata.allow_eoa_shrink_only = TRUE; 

    /* Set the ring type in the DXPL.  In most cases, we will 
     * need H5AC_RING_RDFSM, so initialy set the ring type in 
     * the DXPL to that value.  We will alter this later if needed.
     */
    if(H5AC_set_ring(dxpl_id, H5AC_RING_RDFSM, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value(1)")
    reset_ring = TRUE;
    curr_ring = H5AC_RING_RDFSM;

    /* Iterate until no more EOA shrinking occurs */
    do {
	eoa_shrank = FALSE;

	/* Check the last section of each free-space manager */
	for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {

            /* test to see if we need to switch rings -- do so if required */
            if((H5MF_ALLOC_TO_FS_TYPE(f, type) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
                    || (H5MF_ALLOC_TO_FS_TYPE(f, type) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
                needed_ring = H5AC_RING_MDFSM;
            else
                needed_ring = H5AC_RING_RDFSM;

            if(needed_ring != curr_ring) {
                if(H5AC_set_ring(dxpl_id, needed_ring, &dxpl, &curr_ring) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value (2)")
                curr_ring = needed_ring;
            } /* end if */

	    if(f->shared->fs_man[type]) {
		udata.alloc_type = type;
		if((status = H5FS_sect_try_shrink_eoa(f, dxpl_id, f->shared->fs_man[type], &udata)) < 0)
		    HGOTO_ERROR(H5E_FSPACE, H5E_CANTSHRINK, FAIL, "can't check for shrinking eoa")
		else if(status > 0)
		    eoa_shrank = TRUE;
	    } /* end if */
	} /* end for */

	/* check the two aggregators */
	if((status = H5MF_aggrs_try_shrink_eoa(f, dxpl_id)) < 0)
	    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSHRINK, FAIL, "can't check for shrinking eoa")
	else if(status > 0)
	    eoa_shrank = TRUE;
    } while(eoa_shrank);

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set property value")

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5MF__close_shrink_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_settle_raw_data_fsm()
 *
 * Purpose: 	Handle any tasks required before the metadata cache 
 *		can serialize or flush the raw data free space manager
 *		and any metadata free space managers that reside in the 
 *		raw data free space manager ring. 
 *
 *		Specifically, any metadata managers that DON'T handle 
 *		space allocation for free space manager header or section 
 *		info will reside in the raw data free space manager ring.  
 *		As of this writing, the plan is to move to only two free space 
 *		managers, one for raw data and one for metadata -- which
 *		means that only the raw data free space manager will reside
 *		in the free space manager ring.  However, this has not been
 *		fully implemented yet, so this code must support the 
 *		possibilty of multiple metadata free space managers, at most
 *		two of which handle free space manager header or section info, 
 *		and thus reside in the metadata free space manager ring.
 *
 *		At present, the task list is:
 *
 *		1) Reduce the EOA to the extent possible.  To do this:
 *
 *		    a) Free both aggregators.  Space not at EOA will be
 *		       added to the appropriate free space manager.
 *		    
 *		       The raw data aggregator should not be restarted 
 *		       after this point.  It is possible that the metadata
 *		       aggregator will be.
 *		    
 *		    b) Free all file space currently allocated to free
 *		       space managers.  
 *
 *		    c) Delete the free space manager superblock 
 *		       extension message if allocated.
 *
 *		   This done, reduce the EOA by moving it to just before
 *		   the last piece of free memory in the file.
 *		   
 *		2) Ensure that space is allocated for the free space
 *                 manager superblock extension message.  Must do this
 *                 now, before reallocating file space for free space 
 *		   managers, as it is possible that this allocation may
 *		   grab the last section in a FSM -- making it unnecessary
 *		   to re-allocate file space for it.
 *
 *		3) Scan all free space managers not involved in allocating
 *		   space for free space managers.  For each such free space
 *		   manager, test to see if it contains free space.  If 
 *		   it does, allocate file space for its header and section
 *		   data.  If it contains no free space, leave it without 
 *		   allocated file space as there is no need to save it to 
 *		   file.
 *
 *		   Note that all free space managers in this class should
 *		   see no further space allocations / deallocations as 
 *		   at this point, all raw data allocations should be 
 *		   finalized, as should all metadata allocations not 
 *		   involving free space managers.
 *
 *		   We will allocate space for free space managers involved
 *		   in the allocation of file space for free space managers
 *		   in H5MF_settle_meta_data_fsm()
 * Return:	SUCCEED/FAIL
 *
 * Programmer:  John Mainzer
 *	        5/25/16
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_settle_raw_data_fsm(H5F_t *f, hid_t dxpl_id, hbool_t *fsm_settled)
{
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    hbool_t reset_ring = FALSE;         /* Whether the ring was set */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(fsm_settled);

    /* Only need to settle things if we are persisting the free space info */
    if(f->shared->fs_strategy == H5F_FILE_SPACE_ALL_PERSIST) {
        H5O_fsinfo_t fsinfo;                    /* Free space manager info message */
        H5FD_mem_t	type;			/* Memory type for iteration */
        H5AC_ring_t curr_ring = H5AC_RING_INV;  /* Current ring value */
        H5AC_ring_t needed_ring = H5AC_RING_INV; /* Ring value needed for this iteration  */
        hbool_t fsm_opened[H5FD_MEM_NTYPES];    /* State of FSM */
        hbool_t fsm_visited[H5FD_MEM_NTYPES];   /* State of FSM */

        /* Initialize fsm_opened and fsm_visited */
        HDmemset(fsm_opened, 0, sizeof(fsm_opened));
        HDmemset(fsm_visited, 0, sizeof(fsm_visited));

        /* 1) Reduce the EOA to the extent possible. */

        /* a) Free the space in aggregators:
         *
         * (for space not at EOF, it may be put into free space managers) 
         *
         * Do this now so that the raw data FSM (and any other FSM that isn't 
         * involved in space allocation for FSMs) will have no further activity.
         *
         * Note that while the raw data aggregator should not be restarted during
         * the close process, this need not be the case for the metadata aggregator.
         */
        if(H5MF_free_aggrs(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't free aggregators")

        /* Set the ring type in the DXPL.  In most cases, we will
         * need H5AC_RING_MDFSM first, so initialy set the ring in
         * the DXPL to that value.  We will alter this later if
         * needed.
         */
        if(H5AC_set_ring(dxpl_id, H5AC_RING_MDFSM, &dxpl, &orig_ring) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSET, FAIL, "unable to set ring value(0)")
        reset_ring = TRUE;
        curr_ring = H5AC_RING_MDFSM;

        /* b) Free the file space (if any) allocated to each free space manager.
         *
         * Do this to facilitate reduction of the size of the file to the 
         * extent possible.  We will re-allocate space to free space managers 
         * that have free space to save after this reduction.
         *
         * In the case of the raw data free space manager, and any other free
         * space manager that does not allocate space for free space managers,
         * allocations should be complete at this point, as all raw data should
         * have space allocated and be flushed to file at this point.  Thus we
         * can examine such free space managers and only re-allocate space for 
         * them if they contain free space.  Do this later in this function after
         * the EOA has been reduced to the extent possible.
         *
         * For free space managers that allocate file space for free space 
         * managers (usually just a single metadata free space manager, but for 
         * now at least, free space managers for different types of metadata 
         * are possible), the matter is more ticklish due to the self-
         * referential nature of the problem.  These FSMs are dealt with in 
         * H5MF_settle_meta_data_fsm().
         */
        for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
            H5FS_stat_t fs_stat;        /* Information for free-space manager */
            H5FD_mem_t  fsm_type;       /* File memory type for FSM */

            /* There is potentially a many-to-one mapping from memory types to 
             * free space managers.  Use the fsm_visited[] array to avoid visiting 
             * a given FSM more than once.  Use fsm_opened[] to track which FSMs
             * must be closed at the end of this function.
             */
            fsm_type = H5MF_ALLOC_TO_FS_TYPE(f, type);
            if(!fsm_visited[fsm_type]) {
                fsm_visited[fsm_type] = TRUE;

                /* If there is no active FSM for this type, but such a FSM has 
                 * space allocated in file, open it so that we can free its file 
                 * space.
                 */
                if(NULL == f->shared->fs_man[fsm_type]) {
                    if(H5F_addr_defined(f->shared->fs_addr[fsm_type])) {
                        /* Sanity check */
                        HDassert(fsm_opened[fsm_type] == FALSE);

                        /* Start up FSM for the file memory type */
                        if(H5MF__alloc_open(f, dxpl_id, fsm_type) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, FAIL, "can't initialize file free space manager")
                        fsm_opened[fsm_type] = TRUE;
                    } /* end if */
                } /* end if */

                /* Check for an actual FSM for this type now */
                /* (Possibly opened in previous step) */
                if(f->shared->fs_man[fsm_type]) {
                    /* Test to see if we need to switch rings -- do so if required */
                    if((fsm_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
                           || (fsm_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
                        needed_ring = H5AC_RING_MDFSM;
                    else
                        needed_ring = H5AC_RING_RDFSM;
                    if(needed_ring != curr_ring) {
                        if(H5AC_set_ring(dxpl_id, needed_ring, &dxpl, &curr_ring)< 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSET, FAIL, "unable to set ring value.")
                        curr_ring = needed_ring;
                    } /* end if */

                    /* Query free space manager info for this type */
                    if(H5FS_stat_info(f, f->shared->fs_man[fsm_type], &fs_stat) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't get free-space info")

                    /* Check if the free space manager has space in the file */
                    if(H5F_addr_defined(fs_stat.addr) || H5F_addr_defined(fs_stat.sect_addr)) {
                        /* Delete the free space manager in the file.  Will 
                         * reallocate later if the free space manager contains
                         * any free space.
                         */
                        if(H5FS_free(f, f->shared->fs_man[fsm_type], dxpl_id) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release free-space headers")
                        f->shared->fs_addr[fsm_type] = HADDR_UNDEF;
                    } /* end if */
                } /* end if */

                /* note that we are tracking opened FSM -- we will close them
                 * at the end of the function.
                 */
            } /* end if */
        } /* end for */

        /* c) Delete the free space manager superblock extension message 
         *    if allocated.
         *
         *    Must do this since the routine that writes / creates superblock
         *    extension messages will choke if the target message is 
         *    unexpectedly either absent or present.
         */
        if(H5F_addr_defined(f->shared->sblock->ext_addr))
            if(H5F_super_ext_remove_msg(f, dxpl_id, H5O_FSINFO_ID) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "error in removing message from superblock extension")

        /* As the final element in 1), shrink the EOA for the file */
        if(H5MF__close_shrink_eoa(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSHRINK, FAIL, "can't shrink eoa")

     
        /* 2) Ensure that space is allocated for the free space manager superblock 
         *    extension message.  Must do this now, before reallocating file space 
         *    for free space managers, as it is possible that this allocation may
         *    grab the last section in a FSM -- making it unnecessary to 
         *    re-allocate file space for it.
         *
         * Do this by writing a free space manager superblock extension message.
         * 
         * Since no free space manager has file space allocated for it, this 
         * message must be invalid since we can't save addresses of FSMs when
         * those addresses are unknown.  This is OK -- we will write the correct
         * values to the message at free space manager shutdown.
         */
        for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) 
            fsinfo.fs_addr[type-1] = HADDR_UNDEF;
        fsinfo.strategy = f->shared->fs_strategy;
        fsinfo.threshold = f->shared->fs_threshold;
        if(H5F_super_ext_write_msg(f, dxpl_id, H5O_FSINFO_ID, &fsinfo, TRUE, H5O_MSG_NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_WRITEERROR, FAIL, "error in writing message to superblock extension")


        /* 3) Scan all free space managers not involved in allocating
         *    space for free space managers.  For each such free space
         *    manager, test to see if it contains free space.  If 
         *    it does, allocate file space for its header and section
         *    data.  If it contains no free space, leave it without 
         *    allocated file space as there is no need to save it to 
         *    file.
         *
         *    Note that all free space managers in this class should
         *    see no further space allocations / deallocations as 
         *    at this point, all raw data allocations should be 
         *    finalized, as should all metadata allocations not involving
         *    free space managers.
         *
         *    We will allocate space for free space managers involved
         *    in the allocation of file space for free space managers
         *    in H5MF_settle_meta_data_fsm()
         */

        /* Reinitialize fsm_visited */
        HDmemset(fsm_visited, 0, sizeof(fsm_visited));

        for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
            H5FD_mem_t  fsm_type;       /* File memory type for FSM */

            fsm_type = H5MF_ALLOC_TO_FS_TYPE(f, type);

            /* test to see if we need to switch rings -- do so if required */
            if((fsm_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
                    || (fsm_type == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
                needed_ring = H5AC_RING_MDFSM;
            else
                needed_ring = H5AC_RING_RDFSM;

            if(needed_ring != curr_ring) {
                if(H5AC_set_ring(dxpl_id, needed_ring, &dxpl, &curr_ring)< 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTSET, FAIL, "unable to set ring value.")
                curr_ring = needed_ring;
            } /* end if */

            /* Since there can be a many-to-one mapping from memory types 
             * to free space managers, ensure that we don't visit any FSM
             * more than once.
             */
            if(!fsm_visited[fsm_type]) {
                fsm_visited[fsm_type] = TRUE;

                if(f->shared->fs_man[fsm_type]) {
                    /* Only allocate file space if the target free space manager 
                     * doesn't allocate file space for free space managers.  Note 
                     * that this is also the deciding factor as to whether a FSM 
                     * in in the raw data FSM ring.
                     */
                    if((fsm_type != H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR)) 
                            && (fsm_type != H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO))) {
                        H5FS_stat_t fs_stat;        /* Information for free-space manager */

                        /* The current ring should be H5AC_RING_RDFSM */
                        HDassert(curr_ring == H5AC_RING_RDFSM);

                        /* Query free space manager info for this type */
                        if(H5FS_stat_info(f, f->shared->fs_man[fsm_type], &fs_stat) < 0 )
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't get free-space info")

                        /* If the free space manager contains section info, 
                         * allocate space for the header and sinfo (note that 
                         * space must not be allocated at present -- verify
                         * verify this with assertions).
                         */
                        if(fs_stat.serial_sect_count > 0) {
                            /* Sanity check */
                            HDassert(!H5F_addr_defined(fs_stat.addr));

                            /* Allocate FSM header */
                            if(H5FS_alloc_hdr(f, f->shared->fs_man[fsm_type], &f->shared->fs_addr[fsm_type], dxpl_id) < 0)
                                HGOTO_ERROR(H5E_FSPACE, H5E_CANTALLOC, FAIL, "can't allocated free-space header")

                            /* Allocate FSM section info */
                            HDassert(!H5F_addr_defined(fs_stat.sect_addr));
                            HDassert(fs_stat.alloc_sect_size == 0);
                            if(H5FS_alloc_sect(f, f->shared->fs_man[fsm_type], dxpl_id) < 0 )
                                HGOTO_ERROR(H5E_FSPACE, H5E_CANTALLOC, FAIL, "can't allocate free-space section info")

#ifndef NDEBUG
                            /* Re-Query free space manager info for this type */
                            if(H5FS_stat_info(f, f->shared->fs_man[fsm_type], &fs_stat) < 0)
                                HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't get free-space info")

                            HDassert(H5F_addr_defined(fs_stat.addr));
                            HDassert(H5F_addr_defined(fs_stat.sect_addr));
                            HDassert(fs_stat.serial_sect_count > 0);
                            HDassert(fs_stat.alloc_sect_size > 0);
                            HDassert(fs_stat.alloc_sect_size == fs_stat.sect_size);
#endif /* NDEBUG */
                        } /* end if */
                        else {
                            HDassert(!H5F_addr_defined(fs_stat.addr));
                            HDassert(!H5F_addr_defined(fs_stat.sect_addr));
                            HDassert(fs_stat.serial_sect_count == 0);
                            HDassert(fs_stat.alloc_sect_size == 0);
                        } /* end else */
                    } /* end if */
                } /* end if */

                /* Close any opened FSMs */
                if(fsm_opened[fsm_type]) {
                    if(H5MF__alloc_close(f, dxpl_id, fsm_type) < 0)
                       HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, FAIL, "can't close file free space manager")
                    fsm_opened[fsm_type] = FALSE;
                } /* end if */
            } /* end if */
        } /* end for */

        /* verify that all opened FSMs were closed */
        for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
            HDassert(!fsm_opened[type]);

        /* Indicate that the FSM was settled successfully */
        *fsm_settled = TRUE;
    } /* end if */

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_FSPACE, H5E_CANTSET, FAIL, "unable to set property value")

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* H5MF_settle_raw_data_fsm() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_settle_meta_data_fsm()
 *
 * Purpose: 	If the free space manager is persistent, handle any tasks 
 *		required before the metadata cache can serialize or flush 
 *		the metadata free space manager(sI) that handle file space 
 *		allocation for free space managers.
 *
 *		In most cases, there will be only one manager assigned 
 *		to this role.  However, since for reason or reason unknown,
 *		free space manager headers and section info blocks are 
 *		different classes of memory, it is possible that two free 
 *		space managers will be involved.
 *
 *		On entry to this function, the raw data settle routine
 *		(H5MF_settle_raw_data_fsm()) should have:
 *
 *		1) Freed the accumulators.
 *
 *		2) Freed all file space allocated to the free space managers.
 *
 *		3) Deleted the free space manager superblock extension message
 *
 *		4) Reduced the EOA to the extent possible.
 *
 *		5) Re-created the free space manager superblock extension
 *		   message.
 *
 *		6) Reallocated file space for all non-empty free space 
 *		   managers NOT involved in allocation of space for free 
 *		   space managers.
 *
 *		   Note that these free space managers (if not empty) should
 *		   have been written to file by this point, and that no
 *		   further space allocations involving them should take 
 *		   place during file close.
 *
 *		On entry to this routine. the free space manager(s) involved
 *		in allocation of file space for free space managers should
 *		still be floating. (i.e. should not have any file space 
 *		allocated to them.)
 *
 *		Similarly, the raw data aggregator should not have been 
 *		restarted.  Note that it is probable that reallocation of 
 *		space in 5) and 6) above will have re-started the metadata 
 *		aggregator.
 *
 *
 *		In this routine, we proceed as follows:
 *
 *		1) Verify that the free space manager(s) involved in file
 *		   space allocation for free space managers are still floating.
 *
 *		2) Free the accumulators.
 *
 *		3) Reduce the EOA to the extent possible.
 *
 *		4) Re-allocate space for any free space manager(s) that:
 *
 *		   a) are involved in allocation of space for free space 
 *		      managers, and 
 *
 *		   b) contain free space.
 *
 *		   It is possible that we could allocate space for one 
 *		   of these free space manager(s) only to have the allocation 
 *		   result in the free space manager being empty and thus 
 *		   obliging us to free the space again.  Thus there is the
 *		   potential for an infinte loop if we want to avoid saving
 *		   empty free space managers.
 *
 *		   Similarly, it is possible that we could allocate space 
 *		   for a section info block, only to discover that this 
 *		   allocation has changed the size of the section info -- 
 *		   forcing us to deallocate and start the loop over again.
 *
 *		   To avoid this, simply allocate file space for these
 *		   FSM(s) directly from the VFD layer if allocation is 
 *		   indicated.  This avoids the issue by bypassing the FSMs
 *		   in this case.  
 *
 *		   Note that this may increase the size of the file needlessly. 
 *		   A better solution would be to modify the FSM code to 
 *		   save empty FSMs to file, and to allow section info blocks
 *		   to be oversized.  However, given that the FSM code is 
 *		   also used by the fractal heaps, and that we are under 
 *		   severe time pressure at the moment, the above brute 
 *		   force solution is attractive. 
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:  John Mainzer
 *	        5/25/16
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_settle_meta_data_fsm(H5F_t *f, hid_t dxpl_id, hbool_t *fsm_settled)
{
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV; /* Original ring value */
    hbool_t	reset_ring = FALSE;     /* Whether we set the ring */
    herr_t 	ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)

    /* Check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(fsm_settled);

    /* Only need to settle things if we are persisting the free space info */
    if(f->shared->fs_strategy == H5F_FILE_SPACE_ALL_PERSIST) {
        H5FS_t     *hdr_fspace;             /* Ptr to FSM hdr alloc FSM */
        H5FS_t     *sinfo_fspace;           /* Ptr to FSM sinfo alloc FSM */
        H5FS_stat_t fs_stat;		    /* Information for FSM */
        H5FD_mem_t  hdr_fsm_alloc_type;	    /* FSM hdr alloc type */
        H5FD_mem_t  sinfo_fsm_alloc_type;   /* FSM info alloc type */

        /* Get the file memory types for the FSM header & section info */
        hdr_fsm_alloc_type = H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR);
        sinfo_fsm_alloc_type = H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO);

        /* Sanity checks */
        HDassert(hdr_fsm_alloc_type > H5FD_MEM_NOLIST);
        HDassert(hdr_fsm_alloc_type < H5FD_MEM_NTYPES);
        HDassert(sinfo_fsm_alloc_type > H5FD_MEM_NOLIST);
        HDassert(sinfo_fsm_alloc_type < H5FD_MEM_NTYPES);
        HDassert(!H5F_addr_defined(f->shared->fs_addr[hdr_fsm_alloc_type]));
        HDassert(!H5F_addr_defined(f->shared->fs_addr[sinfo_fsm_alloc_type]));

        /* Note that in most cases, hdr_fspace will equal sinfo_fspace. */
        hdr_fspace = f->shared->fs_man[hdr_fsm_alloc_type];
        sinfo_fspace = f->shared->fs_man[sinfo_fsm_alloc_type];

        /* Set the ring in the dxpl appropriately for subsequent calls */
        if(H5AC_set_ring(dxpl_id, H5AC_RING_MDFSM, &dxpl, &orig_ring) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSET, FAIL, "unable to set ring value")
        reset_ring = TRUE;

#ifndef NDEBUG
        /* Verify that hdr_fspace is floating if it exists */
        if(hdr_fspace) {
            /* Query free space manager info for this type */
            if(H5FS_stat_info(f, hdr_fspace, &fs_stat) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get free-space info")

            HDassert(!H5F_addr_defined(fs_stat.addr));
            HDassert(!H5F_addr_defined(fs_stat.sect_addr));
            HDassert(fs_stat.alloc_sect_size == 0);
        } /* end if */

        /* Verify that sinfo_fspace is floating if it exists */
        if((sinfo_fspace) && (hdr_fspace != sinfo_fspace)) {
            /* Query free space manager info for this type */
            if(H5FS_stat_info(f, sinfo_fspace, &fs_stat) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get free-space info")

            HDassert(!H5F_addr_defined(fs_stat.addr));
            HDassert(!H5F_addr_defined(fs_stat.sect_addr));
            HDassert(fs_stat.alloc_sect_size == 0);
        } /* end if */
#endif /* NDEBUG */

        /* Free the space in the metadata aggregator.  Do this via the 
         * H5MF_free_aggrs() call.  Note that the raw data aggregator must
         * have already been freed.  Sanity checks for this?
         */
        /* (for space not at EOF, it may be put into free space managers) */
        if(H5MF_free_aggrs(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't free aggregators")

        /* Trying shrinking the EOA for the file */
        if(H5MF__close_shrink_eoa(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTSHRINK, FAIL, "can't shrink eoa")

        /* ******************* PROBLEM: ********************
         *
         * If the file has an alignement other than 1, and if 
         * the EOA is not a multiple of this alignment, allocating sapce 
         * for the section via the VFD info has the potential of generating 
         * a fragment that will be added to the free space manager.  This 
         * of course undoes everything we have been doing here.
         *
         * Need a way around this.  Obvious solution is to force the EOA to 
         * be a multiple of the alignment.  
         *
         * Fortunately, alignment is typically 1, so this is a non-issue in
         * most cases.  In cases where the alignment is not 1, for now we 
         * have decided to drop the fragment on the floor.  
         *
         * Eventually, we should fix this by modifying the on disk representations
         * of free space managers to allow for empty space, so as to bypass the
         * issues created by self-referential free space managers, and make
         * this issue moot.
         */
        /* HDassert(f->shared->alignment == 1); */


        /* The free space manager(s) that handle space allocations for free 
         * space managers should be settled now, albeit without file space 
         * allocated to them.  To avoid the possibility of changing the sizes
         * of their section info blocks, allocate space for them now at the 
         * end of file via H5FD_alloc().
         *
         * In the past, this issue of allocating space without touching the 
         * free space managers has been deal with by calling 
         * H5MF_aggr_vfd_alloc(), which in turn calls H5MF_aggr_alloc().  
         * This is problematic since (if I read the code correctly) it will 
         * re-constitute the metadata aggregator, which will add any left 
         * over space to one of the free space managers when freed.
         *
         * This is a non-starter, since the entire objective is to settle the
         * free space managers.
         *
         * Hence the decision to call H5FD_alloc() directly.  
         * 
         * As discussed in PROBLEM above, if f->shared->alignment is not 1, 
         * this has the possibility of generating a fragment of file space
         * that would typically be inserted into one of the free space managers.
         *
         * This is isn't good, but due to schedule pressure, we will just drop
         * the fragement on the floor for now.
         */
        if(hdr_fspace)
            if(H5FS_alloc_vfd_alloc_hdr_and_section_info(f, dxpl_id, hdr_fspace,
                    &(f->shared->fs_addr[hdr_fsm_alloc_type])) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTALLOC, FAIL, "can't vfd allocate hdr FSM file space")

        if(sinfo_fspace && (sinfo_fspace != hdr_fspace))
            if(H5FS_alloc_vfd_alloc_hdr_and_section_info(f, dxpl_id, sinfo_fspace,
                    &(f->shared->fs_addr[sinfo_fsm_alloc_type])) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTALLOC, FAIL, "can't vfd allocate sinfo FSM file space")

        /* Indicate that the FSM was settled successfully */
        *fsm_settled = TRUE;
    } /* end if */

done:
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_FSPACE, H5E_CANTSET, FAIL, "unable to set property value")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MF_settle_meta_data_fsm() */


/*-------------------------------------------------------------------------
 * Function:    H5MF__close_delete
 *
 * Purpose:     Common code for closing and deleting freespace managers from
 *              the file.
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Vailin Choi
 *              Jan 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5MF__close_delete(H5F_t *f, hid_t dxpl_id, H5P_genplist_t **dxpl)
{
    H5FD_mem_t type;                    /* Memory type for iteration */
    H5AC_ring_t curr_ring = H5AC_RING_INV;      /* current ring value */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG */

    /* check args */
    HDassert(f);
    HDassert(f->shared);

    /* Iterate over all the free space types that have managers and get each free list's space */
    for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
        H5AC_ring_t needed_ring;        /* Ring value needed for this iteration */

#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Check 1.0 - f->shared->fs_man[%u] = %p, f->shared->fs_addr[%u] = %a\n", FUNC, (unsigned)type, f->shared->fs_man[type], (unsigned)type, f->shared->fs_addr[type]);
#endif /* H5MF_ALLOC_DEBUG_MORE */

        /* test to see if we need to switch rings -- do so if required */
        if((H5MF_ALLOC_TO_FS_TYPE(f, type) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
                || (H5MF_ALLOC_TO_FS_TYPE(f, type) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
            needed_ring = H5AC_RING_MDFSM;
        else
            needed_ring = H5AC_RING_RDFSM;

        if(needed_ring != curr_ring) {
            if(H5AC_set_ring(dxpl_id, needed_ring, dxpl, &curr_ring) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value (4)")
            curr_ring = needed_ring;
        } /* end if */

        /* If the free space manager for this type is open, close it */
        if(f->shared->fs_man[type]) {
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Before closing free space manager\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG_MORE */
            if(H5FS_close(f, dxpl_id, f->shared->fs_man[type]) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release free space info")
            f->shared->fs_man[type] = NULL;
            f->shared->fs_state[type] = H5F_FS_STATE_CLOSED;
        } /* end if */
#ifdef H5MF_ALLOC_DEBUG_MORE
HDfprintf(stderr, "%s: Check 2.0 - f->shared->fs_man[%u] = %p, f->shared->fs_addr[%u] = %a\n", FUNC, (unsigned)type, f->shared->fs_man[type], (unsigned)type, f->shared->fs_addr[type]);
#endif /* H5MF_ALLOC_DEBUG_MORE */

        /* If there is free space manager info for this type, delete it */
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
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't delete free space manager")

            /* Shift [back] to closed state */
            HDassert(f->shared->fs_state[type] == H5F_FS_STATE_DELETING);
            f->shared->fs_state[type] = H5F_FS_STATE_CLOSED;

            /* Sanity check that the free space manager for this type wasn't started up again */
            HDassert(!H5F_addr_defined(f->shared->fs_addr[type]));
        } /* end if */
    } /* end for */

done:
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* H5MF__close_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_try_close
 *
 * Purpose:     This is called by H5Fformat_convert() to close and delete
 *		free-space managers when downgrading persistent free-space
 *		to non-persistent.
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Vailin Choi
 *              Jan 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_try_close(H5F_t *f, hid_t dxpl_id)
{
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    hbool_t reset_ring = FALSE;         /* Whether the ring was set */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG */

    /* check args */
    HDassert(f);

    /* Set the ring type in the DXPL */
    if(H5AC_set_ring(dxpl_id, H5AC_RING_RDFSM, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value")
    reset_ring = TRUE;

    /* Close and delete freespace managers from the file */
    if(H5MF__close_delete(f, dxpl_id, &dxpl) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to close delete free-space managers")

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set property value")

#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MF_try_close() */


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
 * Modifications:
 *      Vailin Choi; July 2012
 *      As the default free-list mapping is changed to H5FD_FLMAP_DICHOTOMY,
 *      modifications are needed to shrink EOA if the last section of each free-space manager
 *      and the remaining space in the two aggregators are at EOA.

 *-------------------------------------------------------------------------
 */
herr_t
H5MF_close(H5F_t *f, hid_t dxpl_id)
{
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t curr_ring;              /* Current ring value */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    H5FD_mem_t type;                    /* Memory type for iteration */
    hbool_t reset_ring = FALSE;         /* Whether the ring was set */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)
#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Entering\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG */

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(f->shared->sblock);

    /* Set the ring type in the DXPL.  In most cases, we will
     * need H5AC_RING_RDFSM, so initialy set the ring in
     * the DXPL to that value.  We will alter this later if 
     * needed.
     */
    if(H5AC_set_ring(dxpl_id, H5AC_RING_RDFSM, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value")
    reset_ring = TRUE;
    curr_ring = H5AC_RING_RDFSM;

    /* Free the space in aggregators */
    /* (for space not at EOF, it may be put into free space managers) */
    if(H5MF_free_aggrs(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFREE, FAIL, "can't free aggregators")

    /* Trying shrinking the EOA for the file */
    if(H5MF__close_shrink_eoa(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSHRINK, FAIL, "can't shrink eoa")

    /* Making free-space managers persistent for superblock version >= 2 */
    if(f->shared->sblock->super_vers >= HDF5_SUPERBLOCK_VERSION_2
            && f->shared->fs_strategy == H5F_FILE_SPACE_ALL_PERSIST) {
        H5O_fsinfo_t fsinfo;		/* Free space manager info message */

        /* Superblock extension and free space manager message should 
         * exist at this point -- verify at least the former.
         */
        HDassert(H5F_addr_defined(f->shared->sblock->ext_addr));

        /* Note that unlike the previous version of this code, we do not 
         * delete free space managers that have no section to store.
         *
         * Can't do this, as that would involve freeing file space, which would 
         * dirty the free space manager in question. 
         * 
         * Fortunately, the code doesn't seem to care about this.
         */

        /* Gather data for the free space manager superblock extension message.
         * 
         * In passing, verify that all the free space managers are closed.
         */
        for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
            fsinfo.fs_addr[type - 1] = f->shared->fs_addr[type];
	fsinfo.strategy = f->shared->fs_strategy;
	fsinfo.threshold = f->shared->fs_threshold;

        /* Write the free space manager message -- message must already exist */
        if(H5F_super_ext_write_msg(f, dxpl_id, H5O_FSINFO_ID, &fsinfo, FALSE, H5O_MSG_NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_WRITEERROR, FAIL, "error in writing message to superblock extension")

	/* Final close of free-space managers */
	for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
	    if(f->shared->fs_man[type]) {
                H5AC_ring_t needed_ring;    /* Ring value needed for this iteration */

                /* test to see if we need to switch rings -- do so if required */
                if((H5MF_ALLOC_TO_FS_TYPE(f, type) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
                        || (H5MF_ALLOC_TO_FS_TYPE(f, type) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
                    needed_ring = H5AC_RING_MDFSM;
                else
                    needed_ring = H5AC_RING_RDFSM;

                if(needed_ring != curr_ring) {
                    if(H5AC_set_ring(dxpl_id, needed_ring, &dxpl, &curr_ring) < 0)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value (3)")
                    curr_ring = needed_ring;
                } /* end if */

                HDassert(f->shared->fs_state[type] == H5F_FS_STATE_OPEN);
		if(H5FS_close(f, dxpl_id, f->shared->fs_man[type]) < 0)
		    HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't close free space manager")
		f->shared->fs_man[type] = NULL;
		f->shared->fs_state[type] = H5F_FS_STATE_CLOSED;
	    } /* end if */
	    f->shared->fs_addr[type] = HADDR_UNDEF;
	} /* end for */

        /* Verify that we haven't dirtied any metadata cache entries 
         * from the metadata free space manager ring out.
         */
        HDassert(H5AC_cache_is_clean(f, H5AC_RING_MDFSM));

        /* Verify that the aggregators are still shutdown. */
        HDassert(f->shared->sdata_aggr.tot_size == 0);
        HDassert(f->shared->sdata_aggr.addr == 0);
        HDassert(f->shared->sdata_aggr.size == 0);
        HDassert(f->shared->meta_aggr.tot_size == 0);
        HDassert(f->shared->meta_aggr.addr == 0);
        HDassert(f->shared->meta_aggr.size == 0);
    } /* end if */
    else {  /* super_vers can be 0, 1, 2 */
        /* Close and delete freespace managers from the file */
        if(H5MF__close_delete(f, dxpl_id, &dxpl) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize file free space")

          /* moved code that was for both the persistant and non persistant free
           * space managers to the non-persistant case.  In the persistant
           * case, the EOA should already be as shrunked as it can be, and the 
           * aggregators should alread be shut down.
           */

        /* Free the space in aggregators (again) */
        /* (in case any free space information re-started them) */
        if(H5MF_free_aggrs(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTFREE, FAIL, "can't free aggregators")

        /* Trying shrinking the EOA for the file */
        /* (in case any free space is now at the EOA) */
        if(H5MF__close_shrink_eoa(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSHRINK, FAIL, "can't shrink eoa")
    } /* end else */

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set property value")

#ifdef H5MF_ALLOC_DEBUG
HDfprintf(stderr, "%s: Leaving\n", FUNC);
#endif /* H5MF_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5MF_close() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_sects_cb()
 *
 * Purpose:	Iterator callback for each free-space section
 *		Retrieve address and size into user data
 *
 * Return:	Always succeed
 *
 * Programmer:  Vailin Choi
 *	        July 1st, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5MF_sects_cb(H5FS_section_info_t *_sect, void *_udata)
{
    H5MF_free_section_t *sect = (H5MF_free_section_t *)_sect;
    H5MF_sect_iter_ud_t *udata = (H5MF_sect_iter_ud_t *)_udata;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(udata->sect_idx < udata->sect_count) {
        udata->sects[udata->sect_idx].addr = sect->sect_info.addr;
        udata->sects[udata->sect_idx].size  = sect->sect_info.size;
        udata->sect_idx++;
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5MF_sects_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_get_free_sections()
 *
 * Purpose: 	To iterate over one or all free-space managers for:
 *			# of sections
 *			section info as defined in H5F_sect_info_t
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *	        July 1st, 2009
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5MF_get_free_sections(H5F_t *f, hid_t dxpl_id, H5FD_mem_t type, size_t nsects, H5F_sect_info_t *sect_info)
{
    size_t 	total_sects = 0;	/* total number of sections */
    H5MF_sect_iter_ud_t sect_udata;     /* User data for callback */
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t curr_ring;              /* Current ring value */
    H5AC_ring_t needed_ring;            /* Ring value needed for this iteration */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    H5FD_mem_t	start_type, end_type;   /* Memory types to iterate over */
    H5FD_mem_t 	ty;     		/* Memory type for iteration */
    hbool_t reset_ring = FALSE;         /* Whether the ring was set */
    ssize_t 	ret_value = -1;         /* Return value */

    FUNC_ENTER_NOAPI_TAG(dxpl_id, H5AC__FREESPACE_TAG, FAIL)

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);

    /* Determine start/end points for loop */
    if(type == H5FD_MEM_DEFAULT) {
	start_type = H5FD_MEM_SUPER;
	end_type = H5FD_MEM_NTYPES;
    } /* end if */
    else {
	start_type = end_type = type;
	H5_INC_ENUM(H5FD_mem_t, end_type);
    } /* end else */

    /* Set up user data for section iteration */
    sect_udata.sects = sect_info;
    sect_udata.sect_count = nsects;
    sect_udata.sect_idx = 0;

    /* Set the ring type in the DXPL.  Note that if we are 
     * scanning a number of different free space managers, 
     * we may have to change the ring
     */
    if((H5MF_ALLOC_TO_FS_TYPE(f, start_type) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
            || (H5MF_ALLOC_TO_FS_TYPE(f, start_type) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
        needed_ring = H5AC_RING_MDFSM;
    else
        needed_ring = H5AC_RING_RDFSM;
    curr_ring = needed_ring;
    if(H5AC_set_ring(dxpl_id, needed_ring, &dxpl, &orig_ring) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value")
    reset_ring = TRUE;

    /* Iterate over memory types, retrieving the number of sections of each type */
    for(ty = start_type; ty < end_type; H5_INC_ENUM(H5FD_mem_t, ty)) {
	hbool_t fs_started = FALSE;

        /* test to see if we need to switch rings -- do so if required */
        if((H5MF_ALLOC_TO_FS_TYPE(f, ty) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_HDR))
                || (H5MF_ALLOC_TO_FS_TYPE(f, ty) == H5MF_ALLOC_TO_FS_TYPE(f, H5FD_MEM_FSPACE_SINFO)))
            needed_ring = H5AC_RING_MDFSM;
        else
            needed_ring = H5AC_RING_RDFSM;

        if(needed_ring != curr_ring) {
            if(H5AC_set_ring(dxpl_id, needed_ring, &dxpl, &curr_ring) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set ring value (2)")
            curr_ring = needed_ring;
        } /* end if */

	/* Open free space manager of this type, if it isn't already */
        if(!f->shared->fs_man[ty] && H5F_addr_defined(f->shared->fs_addr[ty])) {
	    if(H5MF__alloc_open(f, dxpl_id, ty) < 0)
		HGOTO_ERROR(H5E_RESOURCE, H5E_CANTOPENOBJ, FAIL, "can't initialize file free space")
            HDassert(f->shared->fs_man[ty]);
	    fs_started = TRUE;
	} /* end if */

	/* Check if f there's free space sections of this type */
        if(f->shared->fs_man[ty]) {
            hsize_t hnums = 0;          /* Total # of sections */
            size_t nums;                /* Total # of sections, cast to a size_t */

            /* Query how many sections of this type */
	    if(H5FS_sect_stats(f->shared->fs_man[ty], NULL, &hnums) < 0)
		HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't query free space stats")
            H5_CHECKED_ASSIGN(nums, size_t, hnums, hsize_t);

            /* Increment total # of sections */
            total_sects += nums;

            /* Check if we should retrieve the section info */
            if(sect_info && nums > 0) {
                /* Iterate over all the free space sections of this type, adding them to the user's section info */
                if(H5FS_sect_iterate(f, dxpl_id, f->shared->fs_man[ty], H5MF_sects_cb, &sect_udata) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_BADITER, FAIL, "can't iterate over sections")
            } /* end if */
        } /* end if */

	/* Close the free space manager of this type, if we started it here */
        if(fs_started)
            if(H5MF__alloc_close(f, dxpl_id, ty) < 0)
	        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCLOSEOBJ, FAIL, "can't close file free space")
    } /* end for */

    /* Set return value */
    ret_value = (ssize_t)total_sects;

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "unable to set property value")

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* H5MF_get_free_sections() */

