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
 * Created:		H5FDspace.c
 *			Jan  3 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Space allocation routines for the file.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5FD_PACKAGE		/*suppress error about including H5FDpkg  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5FD_space_init_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"         /* File access				*/
#include "H5FDpkg.h"		/* File Drivers				*/
#include "H5FDmulti.h"		/* Usage-partitioned file family	*/
#include "H5MMprivate.h"	/* Memory management			*/


/****************/
/* Local Macros */
/****************/

/* Define this to display information about file allocations */
/* #define H5FD_ALLOC_DEBUG */


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static haddr_t H5FD_alloc_from_free_list(H5FD_t *file, H5FD_mem_t type,
    hsize_t size);
static haddr_t H5FD_aggr_alloc(H5FD_t *file, H5FD_blk_aggr_t *aggr,
    H5FD_blk_aggr_t *other_aggr, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
static herr_t H5FD_aggr_adjoin(const H5FD_t *file, H5FD_blk_aggr_t *aggr,
    H5FD_free_t *last);
static htri_t H5FD_aggr_can_extend(const H5FD_t *file, const H5FD_blk_aggr_t *aggr,
    haddr_t eoa, haddr_t end);
static herr_t H5FD_aggr_shift(H5FD_blk_aggr_t *aggr, hsize_t extra);
static herr_t H5FD_aggr_query(const H5FD_t *file, const H5FD_blk_aggr_t *aggr,
    haddr_t *addr, hsize_t *size);
static haddr_t H5FD_real_alloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
static haddr_t H5FD_update_eoa(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5FD_free_t struct */
H5FL_DEFINE(H5FD_free_t);

/* Declare a PQ free list to manage the metadata accumulator buffer */
H5FL_BLK_DEFINE(meta_accum);



/*--------------------------------------------------------------------------
NAME
   H5FD_space_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5FD_space_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5FD_init_iterface currently).

--------------------------------------------------------------------------*/
static herr_t
H5FD_space_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_space_init_interface)

    FUNC_LEAVE_NOAPI(H5FD_init())
} /* H5FD_space_init_interface() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_free_freelist
 *
 * Purpose:     Split off from H5FD_close(). Free the elements in the
 *              free list for this file driver.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    Never fails
 *
 * Programmer:  Bill Wendling
 *              17. February 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_free_freelist(H5FD_t *file)
{
    H5FD_mem_t  i;
#ifdef H5FD_ALLOC_DEBUG
    unsigned    nblocks = 0;
    hsize_t     nbytes = 0;
#endif  /* H5FD_ALLOC_DEBUG */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_free_freelist)

    /* check args */
    HDassert(file && file->cls);

    /*
     * Free all free-lists, leaking any memory thus described. Also leaks
     * file space allocated but not used when metadata aggregation is
     * turned on.
     */
    for(i = H5FD_MEM_DEFAULT; i < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, i)) {
        H5FD_free_t *cur, *next;

        for( cur = file->fl[i]; cur; cur = next) {
#ifdef H5FD_ALLOC_DEBUG
            ++nblocks;
            nbytes += cur->size;
#endif  /* H5FD_ALLOC_DEBUG */
            next = cur->next;
            H5FL_FREE(H5FD_free_t, cur);
        } /* end for */

        file->fl[i] = NULL;
    } /* end for */

#ifdef H5FD_ALLOC_DEBUG
    if(nblocks)
        HDfprintf(stderr, "%s: leaked %Hu bytes of file memory in %u blocks\n",
                  "H5FD_free_freelist", nbytes, nblocks);
#endif  /* H5FD_ALLOC_DEBUG */

    /* Check if we need to reset the metadata accumulator information */
    if(file->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) {
        /* Free the buffer */
        if(file->meta_accum)
            file->meta_accum = H5FL_BLK_FREE(meta_accum, file->meta_accum);

        /* Reset the buffer sizes & location */
        file->accum_buf_size = file->accum_size = 0;
        file->accum_loc = HADDR_UNDEF;
        file->accum_dirty = 0;
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_free_freelist() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_alloc
 *
 * Purpose:     Private version of H5FDalloc().
 *
 * Return:      Success:    The format address of the new file memory.
 *              Failure:    The undefined address HADDR_UNDEF
 *
 * Programmer:  Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FD_alloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    haddr_t     ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI(H5FD_alloc, HADDR_UNDEF)
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: type = %u, size = %Hu\n", FUNC, (unsigned)type, size);
#endif /* H5FD_ALLOC_DEBUG */

    /* check args */
    assert(file);
    assert(file->cls);
    assert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    assert(size > 0);

#ifdef H5F_DEBUG
    if(H5DEBUG(F))
	HDfprintf(H5DEBUG(F), "%s: alignment=%Hd, threshold=%Hd, size=%Hd\n",
                  FUNC, file->alignment, file->threshold, size);
#endif  /* H5F_DEBUG */

    /* Try to allocate from the free list first */
    if((ret_value = H5FD_alloc_from_free_list(file, type, size)) != HADDR_UNDEF)
        HGOTO_DONE(ret_value)

#ifdef H5F_DEBUG
    if(H5DEBUG(F))
	HDfprintf(H5DEBUG(F), "%s: Could not allocate from freelists\n", FUNC);
#endif  /* H5F_DEBUG */

    if(type != H5FD_MEM_DRAW) {
        /* Handle metadata differently from "raw" data */
        if(HADDR_UNDEF == (ret_value = H5FD_aggr_alloc(file, &(file->meta_aggr), &(file->sdata_aggr), type, dxpl_id, size)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, HADDR_UNDEF, "can't allocate metadata")
    } /* end if */
    else {
        /* Allocate "raw" data */
        if(HADDR_UNDEF == (ret_value = H5FD_aggr_alloc(file, &(file->sdata_aggr), &(file->meta_aggr), type, dxpl_id, size)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, HADDR_UNDEF, "can't allocate raw data")
    } /* end else */

done:
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: ret_value = %a\n", FUNC, ret_value);
#endif /* H5FD_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_alloc_from_free_list
 *
 * Purpose:     Try to allocate SIZE bytes of memory from the free list
 *              if possible.
 *
 *              This is split from H5FD_alloc().
 *
 * Return:      Success:    The format address of the new file memory.
 *              Failure:    The undefined address HADDR_UNDEF
 *
 * Programmer:  Bill Wendling
 *              02. December, 2002
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_alloc_from_free_list(H5FD_t *file, H5FD_mem_t type, hsize_t size)
{
    H5FD_mem_t mapped_type;
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI(H5FD_alloc_from_free_list, HADDR_UNDEF)
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: type = %u, size = %Hu\n", FUNC, (unsigned)type, size);
#endif /* H5FD_ALLOC_DEBUG */

    assert(file);
    assert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    assert(size > 0);

    /* Map the allocation request to a free list */
    if(H5FD_MEM_DEFAULT == file->cls->fl_map[type])
        mapped_type = type;
    else
        mapped_type = file->cls->fl_map[type];

    /*
     * Try to satisfy the request from the free list. Only perform the
     * search if the free list has the potential of satisfying the
     * request.
     *
     * Here, aligned requests are requests that are >= threshold and
     * alignment > 1.
     *
     * For non-aligned request, first try to find an exact match,
     * otherwise use the best match which is the smallest size that meets
     * the requested size.
     *
     * For aligned address request, find a block in the following order
     * of preferences:
     *
     *   1. block address is aligned and exact match in size;
     *   2. block address is aligned with smallest size > requested size;
     *   3. block address is not aligned with smallest size >= requested size.
     */
    if(mapped_type >= H5FD_MEM_DEFAULT && (file->maxsize == 0 || size <= file->maxsize)) {
        H5FD_free_t    *prev = NULL, *best = NULL;
        H5FD_free_t    *cur = file->fl[mapped_type];
        hbool_t         found_aligned = FALSE;
        hbool_t         need_aligned;
        hsize_t         head;

        need_aligned = file->alignment > 1 && size >= file->threshold;

        while(cur) {
            if(cur->size > file->maxsize)
                file->maxsize = cur->size;

            if(need_aligned) {
                if((head = cur->addr % file->alignment) == 0) {
                    /*
                     * Aligned address
                     */
                    if(cur->size >= size) {
                        if(cur->size == size) {
                            /* exact match */
                            ret_value = cur->addr;

                            /*
                             * Make certain we don't hand out a block of raw data
                             * from the free list which overlaps with the metadata
                             * aggregation buffer (if it's turned on)
                             */
                            if(type == H5FD_MEM_DRAW &&
                                    (file->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) &&
                                    H5F_addr_overlap(ret_value, size,
                                                     file->accum_loc, file->accum_size)) {
                                ret_value = HADDR_UNDEF;
                            } else {
                                if(prev)
                                    prev->next = cur->next;
                                else
                                    file->fl[mapped_type] = cur->next;

                                H5FL_FREE(H5FD_free_t, cur);

                                if(size == file->maxsize)
                                    file->maxsize = 0;  /*unknown*/

#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Exact size match (aligned)\n", FUNC);
#endif /* H5FD_ALLOC_DEBUG */
                                HGOTO_DONE(ret_value)
                            }
                        }
                        else
                            /* Favor smallest block, that's closest to the beginning of the file */
                            if(!best || !found_aligned || cur->size < best->size ||
                                    (cur->size == best->size && H5F_addr_lt(cur->addr, best->addr))) {
                                best = cur;
                                found_aligned = TRUE;
                            }
                    } /* end if */
                } else {
                    /*
                     * Non-aligned address
                     *
                     * Check to see if this block is big enough to skip
                     * to the next aligned address and is still big
                     * enough for the requested size.  The extra
                     * (cur->size > head) is for preventing unsigned
                     * underflow.  (This could be improved by checking for
                     * an exact match after excluding the head.  Such
                     * match is as good as the found_aligned case above.)
                     */
                    head = file->alignment - head;	/* actual head size */

                    if(!found_aligned && cur->size > head && cur->size-head >= size) {
                        /* Favor smallest block, that's closest to the beginning of the file */
                        if(!best || cur->size < best->size ||
                                (cur->size == best->size && H5F_addr_lt(cur->addr, best->addr)))
                            best = cur;
                    } /* end if */
                } /* end else */
            } else {
                /* !need_aligned */
                if(cur->size >= size) {
                    if(cur->size == size) {
                        /* exact match */
                        ret_value = cur->addr;

                        /*
                         * Make certain we don't hand out a block of raw data
                         * from the free list which overlaps with the metadata
                         * aggregation buffer (if it's turned on)
                         */
                        if(type == H5FD_MEM_DRAW &&
                                (file->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) &&
                                H5F_addr_overlap(ret_value, size, file->accum_loc,
                                                 file->accum_size)) {
                            ret_value = HADDR_UNDEF;
                        } else {
                            if(prev)
                                prev->next = cur->next;
                            else
                                file->fl[mapped_type] = cur->next;

                            H5FL_FREE(H5FD_free_t, cur);

                            if(size == file->maxsize)
                                file->maxsize = 0;  /*unknown*/

#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Exact size match (unaligned)\n", FUNC);
#endif /* H5FD_ALLOC_DEBUG */
                            HGOTO_DONE(ret_value)
                        }
                    } /* end if */
                    else {
                        /* Favor smallest block, that's closest to the beginning of the file */
                        if(!best || cur->size < best->size ||
                                (cur->size == best->size && H5F_addr_lt(cur->addr, best->addr)))
                            best = cur;
                    } /* end else */
                } /* end if */
            } /* end else */

            prev = cur;
            cur = cur->next;
        } /* end while */

        /* Couldn't find exact match, use best fitting piece found */
        if(best) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Splitting %Hu byte sized block\n", FUNC, best->size);
#endif /* H5FD_ALLOC_DEBUG */
            if(best->size == file->maxsize)
                file->maxsize = 0;  /*unknown*/

            if(!need_aligned || found_aligned) {
                /* free only tail */
                ret_value = best->addr;

                /*
                 * Make certain we don't hand out a block of raw data
                 * from the free list which overlaps with the metadata
                 * aggregation buffer (if it's turned on)
                 */
                if(type == H5FD_MEM_DRAW &&
                        (file->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) &&
                        H5F_addr_overlap(ret_value, size, file->accum_loc,
                                         file->accum_size)) {
                    ret_value = HADDR_UNDEF;
                } else {
                    best->addr += size;     /* Reduce size of block on free list */
                    best->size -= size;
                    HGOTO_DONE(ret_value)
                }
            } else {
                /*
                 * Split into 3 pieces. Keep the the head and tail in the
                 * freelist.
                 */
                H5FD_free_t *tmp = NULL;

                head = file->alignment - (best->addr % file->alignment);
                ret_value = best->addr + head;

                /*
                 * Make certain we don't hand out a block of raw data
                 * from the free list which overlaps with the metadata
                 * aggregation buffer (if it's turned on)
                 */
                if(type == H5FD_MEM_DRAW &&
                        (file->feature_flags & H5FD_FEAT_ACCUMULATE_METADATA) &&
                        H5F_addr_overlap(ret_value, size, file->accum_loc, file->accum_size)) {
                    ret_value = HADDR_UNDEF;
                } else {
                    /* Attempt to allocate memory for temporary node */
                    if((tmp = H5FL_MALLOC(H5FD_free_t))==NULL)
                        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "free block allocation failed")

                    if((tmp->size = (best->size - (head + size)))!=0) {
                        tmp->addr = best->addr + (head + size);
                        tmp->next = best->next;
                        best->next = tmp;
                    } else {
                        /* no tail piece */
                        H5FL_FREE(H5FD_free_t,tmp);
                    }

                    best->size = head;
                    HGOTO_DONE(ret_value)
                } /* end else */
            } /* end else */
        } /* end if */
    } /* end if */

done:
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: ret_value = %a\n", FUNC, ret_value);
#endif /* H5FD_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_alloc_from_free_list() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_aggr_alloc
 *
 * Purpose:     Try to allocate SIZE bytes of memory from an aggregator
 *              block if possible.
 *
 *              This is split from H5FD_alloc().
 *
 * Return:      Success:    The format address of the new file memory.
 *              Failure:    The undefined address HADDR_UNDEF
 *
 * Programmer:  Bill Wendling
 *              2. December, 2002
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_aggr_alloc(H5FD_t *file, H5FD_blk_aggr_t *aggr, H5FD_blk_aggr_t *other_aggr,
    H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    haddr_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_aggr_alloc)
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: type = %u, size = %Hu\n", FUNC, (unsigned)type, size);
#endif /* H5FD_ALLOC_DEBUG */

    /* check args */
    HDassert(file);
    HDassert(aggr);
    HDassert(aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA || aggr->feature_flag == H5FD_FEAT_AGGREGATE_SMALLDATA);
    HDassert(other_aggr);
    HDassert(other_aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA || other_aggr->feature_flag == H5FD_FEAT_AGGREGATE_SMALLDATA);
    HDassert(other_aggr->feature_flag != aggr->feature_flag);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    HDassert(size > 0);

    /*
     * If the aggregation feature is enabled for this VFL
     * driver, allocate "generic" space and sub-allocate out of
     * that, if possible. Otherwise just allocate through
     * H5FD_real_alloc()
     */
    if(file->feature_flags & aggr->feature_flag) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: aggr = {%a, %Hu, %Hu}\n", FUNC, aggr->addr, aggr->tot_size, aggr->size);
#endif /* H5FD_ALLOC_DEBUG */
        /* Check if the space requested is larger than the space left in the block */
        if(size > aggr->size) {
            haddr_t new_space;   /* Address for newly allocated space */

            /* Check if the block asked for is too large for 'normal' aggregator block */
            if(size >= aggr->alloc_size) {
                /* Allocate more room for this new block the regular way */
                if(HADDR_UNDEF == (new_space = H5FD_real_alloc(file, type, dxpl_id, size)))
                    HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, HADDR_UNDEF, "can't allocate aggregation block")

                /* Check if the new space is at the end of the current block */
                if((aggr->addr + aggr->size) == new_space) {
                    /*
                     * Treat the allocation request as if the current block
                     * grew by the amount allocated and just update the address.
                     *
                     * Don't bother updating the block's size since it will
                     * just grow and shrink by the same amount.
                     *
                     * _Do_ add to the total size aggregated.
                     *
                     */
                    ret_value = aggr->addr;
                    aggr->addr += size;
                    aggr->tot_size += size;
                } /* end if */
                else {
                    /* Check if the new space is at the end of the _other_ block */
                    if(other_aggr->size > 0 && (other_aggr->addr + other_aggr->size) == new_space) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: New block is at end of 'other' block: other_aggr = {%a, %Hu, %Hu}\n", FUNC, other_aggr->addr, other_aggr->tot_size, other_aggr->size);
#endif /* H5FD_ALLOC_DEBUG */
                        /* If the other block has used at least the
                         *      'allocation' amount for that block, shift the
                         *      newly allocated space down over the remainder
                         *      in the 'other block', shift the 'other block'
                         *      up by the same amount and free it.  (Which
                         *      should amount to "bubbling" the remainder in
                         *      the 'other block' to the end of the file and
                         *      then "popping" the bubble by shrinking the
                         *      file)
                         */
                        if((other_aggr->tot_size - other_aggr->size) >= other_aggr->alloc_size) {
                            H5FD_mem_t alloc_type = (other_aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA ? H5FD_MEM_DEFAULT : H5FD_MEM_DRAW);      /* Type of file memory to work with */
                            haddr_t free_addr = (new_space + size) - other_aggr->size;   /* Address of free space in 'other block' shifted toward end of the file */
                            hsize_t free_size = other_aggr->size;               /* Size of the free space in 'other block' */

#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Freeing 'other' block\n", FUNC);
#endif /* H5FD_ALLOC_DEBUG */
                            /* Reset 'other' block's info */
                            other_aggr->addr = 0;
                            other_aggr->tot_size = 0;
                            other_aggr->size = 0;

                            /* Shift newly allocated space down */
                            new_space -= free_size;

                            /* Return the unused portion of the 'other' block to a free list */
                            if(H5FD_free(file, alloc_type, dxpl_id, free_addr, free_size) < 0)
                                HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, HADDR_UNDEF, "can't free aggregation block")
                        } /* end if */
                    } /* end if */

                    /* Use the new space allocated, leaving the old block */
                    ret_value = new_space;
                } /* end else */
            } /* end if */
            else {
                H5FD_mem_t alloc_type = (aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA ? H5FD_MEM_DEFAULT : H5FD_MEM_DRAW);      /* Type of file memory to work with */

                /* Allocate another block */
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Allocating block\n", FUNC);
#endif /* H5FD_ALLOC_DEBUG */
                if(HADDR_UNDEF == (new_space = H5FD_real_alloc(file, alloc_type, dxpl_id, aggr->alloc_size)))
                    HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, HADDR_UNDEF, "can't allocate aggregation block")

                /* Check if the new space is at the end of the current block */
                if(aggr->addr + aggr->size == new_space) {
                    aggr->size += aggr->alloc_size;
                    aggr->tot_size += aggr->alloc_size;
                } /* end if */
                else {
                    hsize_t new_size;   /* Size of new aggregator block */

                    /* Return the unused portion of the block to a free list */
                    if(aggr->size > 0)
                        if(H5FD_free(file, alloc_type, dxpl_id, aggr->addr, aggr->size) < 0)
                            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, HADDR_UNDEF, "can't free aggregation block")

                    /* Check if the new space is at the end of the _other_ block */
                    if(other_aggr->size > 0 && (other_aggr->addr + other_aggr->size) == new_space) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: New block is at end of 'other' block: other_aggr = {%a, %Hu, %Hu}\n", FUNC, other_aggr->addr, other_aggr->tot_size, other_aggr->size);
#endif /* H5FD_ALLOC_DEBUG */
#ifdef QAK
                        /* If the other block has used at least the
                         *      'allocation' amount for that block, give the
                         *      remaining free space in the 'other' block to
                         *      the new space allocated for 'this' block.
                         */
                        if((other_aggr->tot_size - other_aggr->size) >= other_aggr->alloc_size) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Absorbing 'other' block\n", FUNC);
#endif /* H5FD_ALLOC_DEBUG */
                            /* Absorb the remaining free space into newly allocated block */
                            new_space -= other_aggr->size;
                            new_size = aggr->alloc_size + other_aggr->size;

                            /* Reset the info for the 'other' block */
                            other_aggr->addr = 0;
                            other_aggr->tot_size = 0;
                            other_aggr->size = 0;
                        } /* end if */
                        else
                            new_size = aggr->alloc_size;
#else /* QAK */
                        /* If the other block has used at least the
                         *      'allocation' amount for that block, shift the
                         *      newly allocated space down over the remainder
                         *      in the 'other block', shift the 'other block'
                         *      up by the same amount and free it.  (Which
                         *      should amount to "bubbling" the remainder in
                         *      the 'other block' to the end of the file and
                         *      then "popping" the bubble by shrinking the
                         *      file)
                         */
                        if((other_aggr->tot_size - other_aggr->size) >= other_aggr->alloc_size) {
                            H5FD_mem_t other_type = (other_aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA ? H5FD_MEM_DEFAULT : H5FD_MEM_DRAW);      /* Type of file memory to work with */
                            haddr_t free_addr = (new_space + aggr->alloc_size) - other_aggr->size;   /* Address of free space in 'other block' shifted toward end of the file */
                            hsize_t free_size = other_aggr->size;               /* Size of the free space in 'other block' */

#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Freeing 'other' block\n", FUNC);
#endif /* H5FD_ALLOC_DEBUG */
                            /* Reset 'other' block's info */
                            other_aggr->addr = 0;
                            other_aggr->tot_size = 0;
                            other_aggr->size = 0;

                            /* Shift newly allocated space down */
                            new_space -= free_size;

                            /* Return the unused portion of the 'other' block to a free list */
                            if(H5FD_free(file, other_type, dxpl_id, free_addr, free_size) < 0)
                                HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, HADDR_UNDEF, "can't free aggregation block")
                        } /* end if */
                        new_size = aggr->alloc_size;
#endif /* QAK */
                    } /* end if */
                    else
                        new_size = aggr->alloc_size;

                    /* Point the aggregator at the newly allocated block */
                    aggr->addr = new_space;
                    aggr->size = new_size;
                    aggr->tot_size = new_size;
                } /* end else */

                /* Allocate space out of the metadata block */
                ret_value = aggr->addr;
                aggr->size -= size;
                aggr->addr += size;
            } /* end else */
        } /* end if */
        else {
            /* Allocate space out of the block */
            ret_value = aggr->addr;
            aggr->size -= size;
            aggr->addr += size;
        }
    } /* end if */
    else {
        /* Allocate data the regular way */
        if(HADDR_UNDEF == (ret_value = H5FD_real_alloc(file, type, dxpl_id, size)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, HADDR_UNDEF, "can't allocate file space")
    } /* end else */

done:
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: ret_value = %a\n", FUNC, ret_value);
#endif /* H5FD_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_aggr_alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_aggr_adjoin
 *
 * Purpose:     Check if a newly freed block of space in the file adjoins an
 *              aggregator block
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Thursday, December 13, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_aggr_adjoin(const H5FD_t *file, H5FD_blk_aggr_t *aggr, H5FD_free_t *last)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_aggr_adjoin)

    /* Check args */
    HDassert(file);
    HDassert(file->cls);
    HDassert(aggr);
    HDassert(aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA || aggr->feature_flag == H5FD_FEAT_AGGREGATE_SMALLDATA);
    HDassert(last);

    /* Check if this free block adjoins the aggregator */
    if((file->feature_flags & aggr->feature_flag) && aggr->size > 0) {
        hbool_t adjoins = FALSE;    /* Whether the block adjoined the aggregator */

        /* Does the newly freed space adjoin the end of the aggregator */
        if((aggr->addr + aggr->size) == last->addr) {
            last->addr = aggr->addr;
            adjoins = TRUE;
        } /* end if */
        /* Does the newly freed space adjoin the beginning of the aggregator */
        else if((last->addr + last->size) == aggr->addr)
            adjoins = TRUE;

        /* Reset aggregator information, if adjoined */
        if(adjoins) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Adjoined flag = %lx aggregator\n", "H5FD_aggr_adjoin", aggr->feature_flag);
#endif /* H5FD_ALLOC_DEBUG */
            last->size += aggr->size;
            aggr->addr = 0;
            aggr->size = 0;
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_aggr_adjoin() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_aggr_can_extend
 *
 * Purpose:     Check is an aggregator block can be extended
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Thursday, December 13, 2007
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5FD_aggr_can_extend(const H5FD_t *file, const H5FD_blk_aggr_t *aggr, haddr_t eoa,
    haddr_t end)
{
    htri_t ret_value = FALSE;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_aggr_can_extend)

    /* Check args */
    HDassert(file);
    HDassert(aggr);
    HDassert(aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA || aggr->feature_flag == H5FD_FEAT_AGGREGATE_SMALLDATA);

    /* Check if this aggregator is active */
    if(file->feature_flags & aggr->feature_flag) {
        /* If the aggregator block is at the end of the file, and the block to
         * test adjoins the beginning of the aggregator block, then it's
         * extendable
         */
        if((aggr->addr + aggr->size) == eoa && end == aggr->addr)
            HGOTO_DONE(TRUE)
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_aggr_can_extend() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_aggr_extend
 *
 * Purpose:     Shift an aggregator block in the file
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Thursday, December 13, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_aggr_shift(H5FD_blk_aggr_t *aggr, hsize_t extra)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_aggr_shift)

    /* Check args */
    HDassert(aggr);
    HDassert(aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA || aggr->feature_flag == H5FD_FEAT_AGGREGATE_SMALLDATA);

    /* Shift the aggregator block by the extra amount */
    aggr->addr += extra;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_aggr_shift() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_aggr_query
 *
 * Purpose:     Query a block aggregator's current address & size info
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Thursday, December 13, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_aggr_query(const H5FD_t *file, const H5FD_blk_aggr_t *aggr, haddr_t *addr,
    hsize_t *size)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_aggr_query)

    /* Check args */
    HDassert(file);
    HDassert(aggr);
    HDassert(aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA || aggr->feature_flag == H5FD_FEAT_AGGREGATE_SMALLDATA);

    /* Check if this aggregator is active */
    if(file->feature_flags & aggr->feature_flag) {
        *addr = aggr->addr;
        *size = aggr->size;
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_aggr_query() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_aggr_reset
 *
 * Purpose:     Reset a block aggregator, returning any space back to file
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Thursday, December 13, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_aggr_reset(H5FD_t *file, H5FD_blk_aggr_t *aggr, hid_t dxpl_id)
{
    H5FD_mem_t alloc_type;      /* Type of file memory to work with */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5FD_aggr_reset, FAIL)

    /* Check args */
    HDassert(file);
    HDassert(aggr);
    HDassert(aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA || aggr->feature_flag == H5FD_FEAT_AGGREGATE_SMALLDATA);

    /* Set the type of memory in the file */
    alloc_type = (aggr->feature_flag == H5FD_FEAT_AGGREGATE_METADATA ? H5FD_MEM_DEFAULT : H5FD_MEM_DRAW);      /* Type of file memory to work with */

    /* Check if this aggregator is active */
    if(file->feature_flags & aggr->feature_flag) {
        /* Return the unused portion of the metadata block to a free list */
        if(aggr->size > 0)
            if(H5FD_free(file, alloc_type, dxpl_id, aggr->addr, aggr->size) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free aggregator block")

        /* Reset aggregator block information */
        aggr->tot_size = 0;
        aggr->addr = 0;
        aggr->size = 0;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_aggr_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_real_alloc
 *
 * Purpose:	Double private version of H5FDalloc() :-)
 *
 * Return:	Success:	The format address of the new file memory.
 *		Failure:	The undefined address HADDR_UNDEF
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 25, 2000
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_real_alloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_real_alloc)
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: type = %u, size = %Hu\n", FUNC, (unsigned)type, size);
#endif /* H5FD_ALLOC_DEBUG */

    /* check args */
    assert(file);
    assert(file->cls);
    assert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    assert(size > 0);

    /*
     * Dispatch to driver `alloc' callback or extend the end-of-address
     * marker
     */
    if(file->cls->alloc) {
        if((ret_value = (file->cls->alloc)(file, type, dxpl_id, size)) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "driver allocation request failed")
    } else {
        if((ret_value = H5FD_update_eoa(file, type, dxpl_id, size)) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "driver eoa update request failed")
    }

done:
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: ret_value = %a\n", FUNC, ret_value);
#endif /* H5FD_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_real_alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_update_eoa
 *
 * Purpose:     Update the EOA field of the file's memory.
 *
 *              This was split off from the H5FD_real_alloc function to
 *              make life easier for all.
 *
 * Return:      Success:    The format address of the new file memory.
 *              Failure:    The undefined address HADDR_UNDEF
 *
 * Programmer:  Bill Wendling
 *              Wednesday, 04. December, 2002
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_update_eoa(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    haddr_t eoa, oldeoa = 0;
    hsize_t wasted;
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_update_eoa)

    /* check args */
    assert(file);
    assert(file->cls);
    assert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    assert(size > 0);

    eoa = file->cls->get_eoa(file, type);

#ifdef H5F_DEBUG
    if(file->alignment * file->threshold != 1 && H5DEBUG(F))
        HDfprintf(H5DEBUG(F),
                  "%s: alignment=%Hd, threshold=%Hd, size=%Hd, Begin eoa=%a\n",
                  FUNC, file->alignment, file->threshold, size, eoa);
#endif  /* H5F_DEBUG */

    /* Wasted is 0 if not exceeding threshold or eoa happens to be aligned */
    wasted = (size >= file->threshold) ? (eoa % file->alignment) : 0;
    if(wasted) {
        wasted = file->alignment - wasted;      /* actual waste                 */
        oldeoa = eoa;                           /* save it for later freeing    */

        /* Advance eoa to the next alignment by allocating the wasted */
        if(H5F_addr_overflow(eoa, size) || (eoa + wasted) > file->maxaddr)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")

        eoa += wasted;

        if(file->cls->set_eoa(file, type, eoa) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")
    } /* end if */

    /* allocate the aligned memory */
    if(H5F_addr_overflow(eoa, size) || eoa + size > file->maxaddr)
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")

    ret_value = eoa;
    eoa += size;

    if(file->cls->set_eoa(file, type, eoa) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")

    /* Free the wasted memory */
    if(wasted) {
        if(H5FD_free(file, type, dxpl_id, oldeoa, wasted) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, HADDR_UNDEF, "file deallocation request failed")
    } /* end if */

#ifdef H5F_DEBUG
    if(file->alignment * file->threshold != 1 && H5DEBUG(F))
        HDfprintf(H5DEBUG(F),
                  "%s: ret_value=%a, wasted=%Hd, Ended eoa=%a\n",
                  FUNC, ret_value, wasted, eoa);
#endif  /* H5F_DEBUG */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_update_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_free
 *
 * Purpose:     Private version of H5FDfree()
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_free(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, hsize_t size)
{
    H5FD_mem_t		mapped_type;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_free, FAIL)

    /* Check args */
    HDassert(file);
    HDassert(file->cls);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);

#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: type = %u, addr = %a, size = %Hu\n", FUNC, (unsigned)type, addr, size);
#endif /* H5FD_ALLOC_DEBUG */

    if(!H5F_addr_defined(addr) || addr > file->maxaddr ||
            H5F_addr_overflow(addr, size) || (addr + size) > file->maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid region")

    /* Allow 0-sized free's to occur without penalty */
    if(0 == size)
        HGOTO_DONE(SUCCEED)

    /* Map request type to free list */
    if(H5FD_MEM_DEFAULT==file->cls->fl_map[type])
        mapped_type = type;
    else
        mapped_type = file->cls->fl_map[type];
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: mapped_type = %u\n", FUNC, (unsigned)mapped_type);
#endif /* H5FD_ALLOC_DEBUG */

    /*
     * If the request maps to a free list then add memory to the free list
     * without ever telling the driver that it was freed.  Otherwise let the
     * driver deallocate the memory.
     */
    if(mapped_type >= H5FD_MEM_DEFAULT) {
        H5FD_free_t *last;          /* Last merged node */
        H5FD_free_t *last_prev = NULL;/* Pointer to node before merged node */
        H5FD_free_t *curr;          /* Current free block being inspected */
        H5FD_free_t *prev;          /* Previous free block being inspected */

        /* Adjust the metadata accumulator to remove the freed block, if it overlaps */
        if((file->feature_flags&H5FD_FEAT_ACCUMULATE_METADATA)
                && H5F_addr_overlap(addr, size, file->accum_loc, file->accum_size)) {
            size_t overlap_size;        /* Size of overlap with accumulator */

            /* Check for overlapping the beginning of the accumulator */
            if(H5F_addr_le(addr, file->accum_loc)) {
                /* Check for completely overlapping the accumulator */
                if(H5F_addr_ge(addr + size, file->accum_loc + file->accum_size)) {
                    /* Reset the entire accumulator */
                    file->accum_loc=HADDR_UNDEF;
                    file->accum_size=FALSE;
                    file->accum_dirty=FALSE;
                } /* end if */
                /* Block to free must end within the accumulator */
                else {
                    size_t new_accum_size;      /* Size of new accumulator buffer */

                    /* Calculate the size of the overlap with the accumulator, etc. */
                    H5_ASSIGN_OVERFLOW(overlap_size,(addr+size)-file->accum_loc,haddr_t,size_t);
                    new_accum_size=file->accum_size-overlap_size;

                    /* Move the accumulator buffer information to eliminate the freed block */
                    HDmemmove(file->meta_accum,file->meta_accum+overlap_size,new_accum_size);

                    /* Adjust the accumulator information */
                    file->accum_loc+=overlap_size;
                    file->accum_size=new_accum_size;
                } /* end else */
            } /* end if */
            /* Block to free must start within the accumulator */
            else {
                /* Calculate the size of the overlap with the accumulator */
                H5_ASSIGN_OVERFLOW(overlap_size,(file->accum_loc+file->accum_size)-addr,haddr_t,size_t);

                /* Block to free is in the middle of the accumulator */
                if(H5F_addr_lt((addr + size), file->accum_loc + file->accum_size)) {
                    haddr_t tail_addr;
                    size_t tail_size;

                    /* Calculate the address & size of the tail to write */
                    tail_addr=addr+size;
                    H5_ASSIGN_OVERFLOW(tail_size,(file->accum_loc+file->accum_size)-tail_addr,haddr_t,size_t);

                    /* Write out the part of the accumulator after the block to free */
                    /* (Use the driver's write call directly - to avoid looping back and writing to metadata accumulator) */
                    if((file->cls->write)(file, H5FD_MEM_DEFAULT, dxpl_id, tail_addr, tail_size, file->meta_accum+(tail_addr-file->accum_loc)) < 0)
                        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "file write request failed")
                } /* end if */

                /* Adjust the accumulator information */
                file->accum_size=file->accum_size-overlap_size;
            } /* end else */
        } /* end if */

        /* Scan through the existing blocks for the mapped type to see if we can extend one */
        curr = file->fl[mapped_type];
        last = prev = NULL;
        while(curr != NULL) {
            /* Check if the block to free adjoins the start of the current block */
            if((addr + size) == curr->addr) {
                /* If we previously found & merged a node, eliminate it from the list & free it */
                if(last != NULL) {
                    /* Check if there was a previous block in the list */
                    if(last_prev != NULL)
                        /* Eliminate the merged block from the list */
                        last_prev->next = last->next;
                    /* No previous block, this must be the head of the list */
                    else
                        /* Eliminate the merged block from the list */
                        file->fl[mapped_type] = last->next;

                    /* Check for eliminating the block before the 'current' one */
                    if(last == prev)
                        prev = last_prev;

                    /* Free the memory for the merged block */
                    H5FL_FREE(H5FD_free_t, last);
                } /* end if */

                /* Adjust the address and size of the block found */
                curr->addr = addr;
                curr->size += size;

                /* Adjust the information about to memory block to include the merged block */
                addr = curr->addr;
                size = curr->size;

                /* Update the information about the merged node */
                last = curr;
                last_prev = prev;
            } /* end if */
            else {
                /* Check if the block to free adjoins the end of the current block */
                if((curr->addr + curr->size) == addr) {
                    /* If we previously found & merged a node, eliminate it from the list & free it */
                    if(last != NULL) {
                        /* Check if there was a previous block in the list */
                        if(last_prev != NULL)
                            /* Eliminate the merged block from the list */
                            last_prev->next = last->next;
                        /* No previous block, this must be the head of the list */
                        else
                            /* Eliminate the merged block from the list */
                            file->fl[mapped_type] = last->next;

                        /* Check for eliminating the block before the 'current' one */
                        if(last == prev)
                            prev = last_prev;

                        /* Free the memory for the merged block */
                        H5FL_FREE(H5FD_free_t, last);
                    } /* end if */

                    /* Adjust the size of the block found */
                    curr->size += size;

                    /* Adjust the information about to memory block to include the merged block */
                    addr = curr->addr;
                    size = curr->size;

                    /* Update the information about the merged node */
                    last = curr;
                    last_prev = prev;
                } /* end if */
            } /* end else */

            /* Advance to next node in list */
            prev = curr;
            curr = curr->next;
        } /* end while */

        /* Check if we adjusted an existing block */
        if(last != NULL) {
            /* Move the node found to the front, if it wasn't already there */
            if(last_prev != NULL) {
                last_prev->next = last->next;
                last->next = file->fl[mapped_type];
                file->fl[mapped_type] = last;
            } /* end if */
        } /* end if */
        else {
            /* Allocate a new node to hold the free block's information */
            if(NULL == (last = H5FL_MALLOC(H5FD_free_t)))
                HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate node for free space info")

            last->addr = addr;
            last->size = size;
            last->next = file->fl[mapped_type];
            file->fl[mapped_type] = last;
        } /* end else */
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: mapped_type = %u, last = {%a, %Hu}\n", FUNC, (unsigned)mapped_type, last->addr, last->size);
#endif /* H5FD_ALLOC_DEBUG */

        /* Check if we increased the size of the largest block on the list */
        file->maxsize = MAX(file->maxsize, last->size);

        /* Check if this free block adjoins the "metadata aggregator" */
        if(H5FD_aggr_adjoin(file, &(file->meta_aggr), last) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "aggregator deallocation request failed")

        /* Check if this free block adjoins the "small data aggregator" */
        if(H5FD_aggr_adjoin(file, &(file->sdata_aggr), last) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "aggregator deallocation request failed")

        /* Check if this free block is at the end of file allocated space.
         * Truncate it if this is true. */
        if(file->cls->get_eoa) {
            haddr_t     eoa;

            eoa = file->cls->get_eoa(file, type);
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: eoa = %a\n", FUNC, eoa);
#endif /* H5FD_ALLOC_DEBUG */
            if(eoa == (last->addr + last->size)) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Reducing file size to = %a\n", FUNC, last->addr);
#endif /* H5FD_ALLOC_DEBUG */
                if(file->cls->set_eoa(file, type, last->addr) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "set end of space allocation request failed")

                /* Remove this free block from the list */
                file->fl[mapped_type] = last->next;
                if(file->maxsize == last->size)
                    file->maxsize = 0; /*unknown*/
                H5FL_FREE(H5FD_free_t, last);
            } /* end if */
        } /* end if */
    } else if(file->cls->free) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Letting VFD free space\n", FUNC);
#endif /* H5FD_ALLOC_DEBUG */
        if((file->cls->free)(file, type, dxpl_id, addr, size) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver free request failed")
    } else {
        /* leak memory */
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: LEAKED MEMORY!!! type = %u, addr = %a, size = %Hu\n", FUNC, (unsigned)type, addr, size);
#endif /* H5FD_ALLOC_DEBUG */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_free() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_realloc
 *
 * Purpose:	Private version of H5FDrealloc()
 *
 * Return:	Success:	New address of the block of memory, not
 *				necessarily the same as the original address.
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FD_realloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t old_addr, hsize_t old_size,
	     hsize_t new_size)
{
    haddr_t	new_addr=old_addr;
    uint8_t	_buf[8192];
    uint8_t	*buf=_buf;
    haddr_t      ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_realloc, HADDR_UNDEF)

    if(new_size == old_size) {
        /*nothing to do*/
    } else if(0 == old_size) {
        /* allocate memory */
        HDassert(!H5F_addr_defined(old_addr));
        if(HADDR_UNDEF == (new_addr = H5FD_alloc(file, type, dxpl_id, new_size)))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HADDR_UNDEF, "file allocation failed")
    } else if(0==new_size) {
        /* free memory */
        HDassert(H5F_addr_defined(old_addr));
        if(H5FD_free(file, type, dxpl_id, old_addr, old_size) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, HADDR_UNDEF, "file deallocation request failed")
        new_addr = HADDR_UNDEF;
    } else if(new_size<old_size) {
        /* free the end of the block */
        if(H5FD_free(file, type, dxpl_id, old_addr+old_size, old_size-new_size) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, HADDR_UNDEF, "file deallocation request failed")
    } else {
        /* move memory to new location */
        /* Note!  This may fail if sizeof(hsize_t)>sizeof(size_t) and the
         * object on disk is too large to read into a memory buffer all at one
         * time.  This chunk of code would have to be re-written using a loop
         * to move pieces of the realloced data through a fixed size buffer, etc.
         * -QAK, 6/20/01
         */
        if(HADDR_UNDEF == (new_addr = H5FD_alloc(file, type, dxpl_id, new_size)))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HADDR_UNDEF, "file allocation failed")
        H5_CHECK_OVERFLOW(old_size,hsize_t,size_t);
        if(old_size > sizeof(_buf) && NULL == (buf = (uint8_t *)H5MM_malloc((size_t)old_size))) {
            (void)H5FD_free(file, type, dxpl_id, new_addr, new_size);
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, HADDR_UNDEF, "memory allocation failed")
        } /* end if */
        if(H5FD_read(file, type, dxpl_id, old_addr, (size_t)old_size, buf) < 0 ||
                H5FD_write(file, type, dxpl_id, new_addr, (size_t)old_size, buf) < 0) {
            (void)H5FD_free(file, type, dxpl_id, new_addr, new_size);
            if(buf != _buf)
                H5MM_xfree(buf);
            HGOTO_ERROR(H5E_FILE, H5E_READERROR, HADDR_UNDEF, "unable to move file block")
        } /* end if */

        if(buf != _buf)
            H5MM_xfree(buf);
        if(H5FD_free(file, type, dxpl_id, old_addr, old_size) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, HADDR_UNDEF, "file deallocation request failed")
    } /* end else */

    /* Set return value */
    ret_value = new_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_realloc() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_can_extend
 *
 * Purpose:	Check if a block in the file can be extended.
 *
 * Return:	Success:	TRUE(1)/FALSE(0)
 * 		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *              Friday, June 11, 2004
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FD_can_extend(const H5FD_t *file, H5FD_mem_t type, haddr_t addr, hsize_t size, hsize_t extra_requested)
{
    haddr_t end;                        /* End of block in file */
    haddr_t eoa;                        /* End of address space in the file */
    htri_t  ret_value = FALSE;          /* Return value */

    FUNC_ENTER_NOAPI(H5FD_can_extend, FAIL)

    /* Retrieve the end of the address space */
    if(HADDR_UNDEF == (eoa = H5FD_get_eoa(file, type)))
	HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

    /* Compute end of block */
    end = addr + size;

    /* Check if the block is exactly at the end of the file */
    if(end == eoa)
        HGOTO_DONE(TRUE)
    else {
        H5FD_free_t *curr;          /* Current free block being inspected */
        H5FD_mem_t mapped_type;     /* Memory type, after mapping */

        /* Map request type to free list */
        if(H5FD_MEM_DEFAULT==file->cls->fl_map[type])
            mapped_type = type;
        else
            mapped_type = file->cls->fl_map[type];

        /* Check if block is inside the metadata or small data aggregator */
        if(mapped_type!=H5FD_MEM_DRAW) {
            /* Check for test block able to extend metadata aggregation block */
            if((ret_value = H5FD_aggr_can_extend(file, &(file->meta_aggr), eoa, end)) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't determine if metadata aggregation block can be extended")
            else if(ret_value > 0)
                HGOTO_DONE(TRUE)
        } /* end if */
        else {
            /* Check for test block able to extend metadata aggregation block */
            if((ret_value = H5FD_aggr_can_extend(file, &(file->sdata_aggr), eoa, end)) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't determine if 'small data' aggregation block can be extended")
            else if(ret_value > 0)
                HGOTO_DONE(TRUE)
        } /* end else */

        /* Scan through the existing blocks for the mapped type to see if we can extend one */
        if(mapped_type >= H5FD_MEM_DEFAULT) {
            curr = file->fl[mapped_type];
            while(curr != NULL) {
                if(end == curr->addr) {
                    if(extra_requested <= curr->size)
                        HGOTO_DONE(TRUE)
                    else
                        HGOTO_DONE(FALSE)
                } /* end if */

                /* Advance to next node in list */
                curr=curr->next;
            } /* end while */
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_can_extend() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_extend
 *
 * Purpose:	Extend a block in the file.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, June 12, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_extend(H5FD_t *file, H5FD_mem_t type, haddr_t addr, hsize_t size, hsize_t extra_requested)
{
    haddr_t     eoa;                    /* End of address space in the file */
    haddr_t     end;                    /* End of block in file */
    hbool_t     update_eoma=FALSE;      /* Whether we need to update the eoma */
    hbool_t     update_eosda=FALSE;     /* Whether we need to update the eosda */
    hbool_t     at_end=FALSE;           /* Block is at end of file */
    H5FD_mem_t  mapped_type;            /* Memory type, after mapping */
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5FD_extend, FAIL)
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: type = %u, addr = %a, size = %Hu, extra_requested = %Hu\n", FUNC, (unsigned)type, addr, size, extra_requested);
#endif /* H5FD_ALLOC_DEBUG */

    /* Retrieve the end of the address space */
    if(HADDR_UNDEF==(eoa=H5FD_get_eoa(file, type)))
	HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

    /* Map request type to free list */
    if(H5FD_MEM_DEFAULT==file->cls->fl_map[type])
        mapped_type = type;
    else
        mapped_type = file->cls->fl_map[type];

    /* Compute end of block */
    end = addr + size;

    /* Check if the block is exactly at the end of the file */
    if(end == eoa)
        at_end = TRUE;
    else {
        /* (Check if block is inside the metadata or small data accumulator) */
        if(mapped_type!=H5FD_MEM_DRAW) {
            /* Check for test block able to extend metadata aggregation block */
            if((ret_value = H5FD_aggr_can_extend(file, &(file->meta_aggr), eoa, end)) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't determine if metadata aggregation block can be extended")
            else if(ret_value > 0)
                update_eoma = TRUE;
        } /* end if */
        else {
            /* Check for test block able to extend metadata aggregation block */
            if((ret_value = H5FD_aggr_can_extend(file, &(file->sdata_aggr), eoa, end)) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't determine if metadata aggregation block can be extended")
            else if(ret_value > 0)
                update_eosda = TRUE;
        } /* end else */
    } /* end else */

    /* Block is at end of file, we are extending the eoma or eosda */
    if(update_eoma || update_eosda || at_end) {
        /* Check for overflowing the file */
        if(H5F_addr_overflow(eoa, extra_requested) || eoa + extra_requested > file->maxaddr)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "file allocation request failed")

        /* Extend the file */
        eoa += extra_requested;
        if(file->cls->set_eoa(file, type, eoa) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "file allocation request failed")

        /* Update the metadata and/or small data block */
        HDassert(!(update_eoma && update_eosda));
        if(update_eoma)
            H5FD_aggr_shift(&(file->meta_aggr), extra_requested);
        if(update_eosda)
            H5FD_aggr_shift(&(file->sdata_aggr), extra_requested);
    } /* end if */
    /* If the block we are extending isn't at the end of the file, find a free block to extend into */
    else {
        H5FD_free_t *curr;          /* Current free block being inspected */
        H5FD_free_t *prev;          /* Current free block being inspected */

        /* Walk through free list, looking for block to merge with */
        curr = file->fl[mapped_type];
        prev = NULL;
        while(curr!=NULL) {
            /* Found block that ajoins end of block to extend */
            if(end == curr->addr) {
                /* Check if free space is large enough */
                if(extra_requested <= curr->size) {
                    /* Check for exact match */
                    if(extra_requested == curr->size) {
                        /* Unlink node from free list */
                        if(prev == NULL)
                            file->fl[mapped_type] = curr->next;
                        else
                            prev->next = curr->next;

                        /* Free the memory for the used block */
                        H5FL_FREE(H5FD_free_t, curr);
                    } /* end if */
                    else {
                        curr->addr += extra_requested;
                        curr->size -= extra_requested;
                    } /* end else */

                    /* Leave now */
                    break;
                } /* end if */
                else
                    HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "can't extend block")
            } /* end if */

            /* Advance to next node in list */
            prev = curr;
            curr = curr->next;
        } /* end while */

        /* Couldn't find block to extend */
        if(curr == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "can't extend block")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_extend() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_get_freespace
 *
 * Purpose:     Retrieve the amount of free space in a file.
 *
 * Return:      Success:        Amount of free space in file
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Monday, October  6, 2003
 *
 * Note:
 *              Raymond Lu
 *              5 January 2007
 *              Due to the complexity EOA for Multi driver, this function
 *              is made failed for now.
 * 
 *-------------------------------------------------------------------------
 */
hssize_t
H5FD_get_freespace(const H5FD_t *file)
{
    H5FD_free_t *free_node;     /* Pointer to node on free list */
    H5FD_mem_t type;            /* Type of memory */
    haddr_t ma_addr = HADDR_UNDEF;    /* Base "metadata aggregator" address */
    hsize_t ma_size = 0;        /* Size of "metadata aggregator" */
    haddr_t sda_addr = HADDR_UNDEF;    /* Base "small data aggregator" address */
    hsize_t sda_size = 0;       /* Size of "small data aggregator" */
    haddr_t eoa = 0;            /* End of allocated space in the file */
    hssize_t ret_value = 0;     /* Return value */

    FUNC_ENTER_NOAPI(H5FD_get_freespace, FAIL)

    /* check args */
    HDassert(file);
    HDassert(file->cls);

    /* Multi driver doesn't support this function because of the complexity.
     * It doesn't have eoa for the whole file. */
    if(file->driver_id == H5FD_MULTI)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "Multi driver doesn't support this function")
    
    /* Retrieve the 'eoa' for the file */
    eoa = file->cls->get_eoa(file, H5FD_MEM_DEFAULT);

    /* Retrieve metadata aggregator info, if available */
    H5FD_aggr_query(file, &(file->meta_aggr), &ma_addr, &ma_size);

    /* Retrieve 'small data' aggregator info, if available */
    H5FD_aggr_query(file, &(file->sdata_aggr), &sda_addr, &sda_size);

    /* Iterate over all the types of memory, to retrieve amount of free space for each */
    for(type = H5FD_MEM_DEFAULT; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,type)) {
        /* Iterate through the free list, accumulating the amount of free space for this type */
        free_node = file->fl[type];
        while(free_node) {
            /* Check for current node adjoining the metadata & small data aggregators */
            if(H5F_addr_eq(free_node->addr + free_node->size, ma_addr)) {
                ma_addr -= free_node->size;
                ma_size += free_node->size;
            } else if(H5F_addr_eq(free_node->addr + free_node->size, sda_addr)) {
                sda_addr -= free_node->size;
                sda_size += free_node->size;
            } else if(H5F_addr_eq(ma_addr + ma_size, free_node->addr))
                ma_size += free_node->size;
            else if(H5F_addr_eq(sda_addr + sda_size, free_node->addr))
                sda_size += free_node->size;
            else
                ret_value += (hssize_t)free_node->size;
            free_node = free_node->next;
        } /* end while */
    } /* end for */

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
} /* end H5FD_get_freespace() */

