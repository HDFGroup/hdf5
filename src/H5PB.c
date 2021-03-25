/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:  H5PB2.c
 *
 * Purpose:  Re-implementation of the page buffer with added features to
 *           support VFD SWMR.
 *                                              JRM -- 10/11/18
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_FRIEND      /* suppress error about including H5Fpkg */
#include "H5PBmodule.h" /* This source code file is part of the 
                                 * H5PB module 
                                 */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions			*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5Fpkg.h"      /* Files				*/
#include "H5FDprivate.h" /* File drivers				*/
#include "H5Iprivate.h"  /* IDs			  		*/
#include "H5FLprivate.h" /* Free lists                           */
#include "H5MMprivate.h" /* Memory management                    */
#include "H5PBpkg.h"     /* File access				*/

/****************/
/* Local Macros */
/****************/

/* Round _x down to nearest _size. */
/* not used at present */
/*
#ifndef rounddown
#define rounddown(_x, _size) (((_x) / (_size)) * (_size))
#endif
*/

/* Round _x up to nearest _size. */
#ifndef roundup
#define roundup(_x, _size) ((((_x) + (_size)-1) / (_size)) * (_size))
#endif

/******************/
/* Local Typedefs */
/******************/

typedef struct _metadata_section {
    haddr_t     addr;
    size_t      len;
    const char *buf;
} metadata_section_t;

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/

static H5PB_entry_t *H5PB__allocate_page(H5PB_t *pb_ptr, size_t buf_size, hbool_t clean_image);

static herr_t H5PB__create_new_page(H5PB_t *pb_ptr, haddr_t addr, size_t size, H5FD_mem_t type,
                                    hbool_t clean_image, H5PB_entry_t **entry_ptr_ptr);

static void H5PB__deallocate_page(H5PB_entry_t *entry_ptr);

static herr_t H5PB__evict_entry(H5F_shared_t *, H5PB_entry_t *, hbool_t, hbool_t);

static herr_t H5PB__flush_entry(H5F_shared_t *, H5PB_t *, H5PB_entry_t *);

static herr_t H5PB__load_page(H5F_shared_t *, H5PB_t *, haddr_t, H5FD_mem_t, H5PB_entry_t **);

static herr_t H5PB__make_space(H5F_shared_t *, H5PB_t *, H5FD_mem_t);

static herr_t H5PB__mark_entry_clean(H5PB_t *, H5PB_entry_t *);

static herr_t H5PB__mark_entry_dirty(H5F_shared_t *, H5PB_t *, H5PB_entry_t *);

static herr_t H5PB__read_meta(H5F_shared_t *, H5FD_mem_t, haddr_t, size_t, void *);

static herr_t H5PB__read_raw(H5F_shared_t *, H5FD_mem_t, haddr_t, size_t, void *);

static herr_t H5PB__write_meta(H5F_shared_t *, H5FD_mem_t, haddr_t, size_t, const void *);

static herr_t H5PB__write_raw(H5F_shared_t *, H5FD_mem_t, haddr_t, size_t, const void *);

static void H5PB_log_access_by_size_counts(const H5PB_t *);

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

/* Declare a free list to manage the H5PB_t struct */
H5FL_DEFINE_STATIC(H5PB_t);

/* Declare a free list to manage the H5PB_entry_t struct */
H5FL_DEFINE_STATIC(H5PB_entry_t);

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_reset_stats
 *
 * Purpose:     Reset statistics collected for the page buffer layer.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/12/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_reset_stats(H5PB_t *pb_ptr)
{
    int i;

    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);

    for (i = 0; i < H5PB__NUM_STAT_TYPES; i++) {

        pb_ptr->bypasses[i]   = 0;
        pb_ptr->accesses[i]   = 0;
        pb_ptr->hits[i]       = 0;
        pb_ptr->misses[i]     = 0;
        pb_ptr->loads[i]      = 0;
        pb_ptr->insertions[i] = 0;
        pb_ptr->flushes[i]    = 0;
        pb_ptr->evictions[i]  = 0;
        pb_ptr->clears[i]     = 0;
    }

    pb_ptr->max_lru_len                      = 0;
    pb_ptr->max_lru_size                     = 0;
    pb_ptr->lru_md_skips                     = 0;
    pb_ptr->lru_rd_skips                     = 0;
    pb_ptr->total_ht_insertions              = 0;
    pb_ptr->total_ht_deletions               = 0;
    pb_ptr->successful_ht_searches           = 0;
    pb_ptr->total_successful_ht_search_depth = 0;
    pb_ptr->failed_ht_searches               = 0;
    pb_ptr->total_failed_ht_search_depth     = 0;
    pb_ptr->max_index_len                    = 0;
    pb_ptr->max_clean_index_len              = 0;
    pb_ptr->max_dirty_index_len              = 0;
    pb_ptr->max_clean_index_size             = 0;
    pb_ptr->max_dirty_index_size             = 0;
    pb_ptr->max_index_size                   = 0;
    pb_ptr->max_rd_pages                     = 0;
    pb_ptr->max_md_pages                     = 0;
    pb_ptr->max_mpmde_count                  = 0;
    pb_ptr->lru_tl_skips                     = 0;
    pb_ptr->max_tl_len                       = 0;
    pb_ptr->max_tl_size                      = 0;
    pb_ptr->delayed_writes                   = 0;
    pb_ptr->total_delay                      = 0;
    pb_ptr->max_dwl_len                      = 0;
    pb_ptr->max_dwl_size                     = 0;
    pb_ptr->total_dwl_ins_depth              = 0;
    pb_ptr->md_read_splits                   = 0;
    pb_ptr->md_write_splits                  = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* H5PB_reset_stats() */

/*-------------------------------------------------------------------------
 * Function:	H5PB_get_stats
 *
 * Purpose:     This function was created without documentation.
 *              What follows is my best understanding of Mohamad's intent.
 *
 *              Retrieve statistics collected about page accesses for the
 *              page buffer layer.
 *
 *              --accesses: the number of metadata and raw data accesses
 *                          to the page buffer layer
 *
 *              --hits: the number of metadata and raw data hits in
 *                          the page buffer layer
 *
 *              --misses: the number of metadata and raw data misses in
 *                          the page buffer layer
 *
 *              --evictions: the number of metadata and raw data evictions
 *                          from the page buffer layer
 *
 *              --bypasses: the number of metadata and raw data accesses
 *                          that bypass the page buffer layer
 *
 *              TODO: The available stats have changed considerably
 *                    since Mohamad wrote this routine.  Update
 *                    the function once things settle down.
 *
 *                                              JRM -- 4/13/20
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_get_stats(const H5PB_t *pb_ptr, unsigned accesses[2], unsigned hits[2], unsigned misses[2],
               unsigned evictions[2], unsigned bypasses[2])
{
    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    HDassert(pb_ptr);

    accesses[0]  = (unsigned)pb_ptr->accesses[0];
    accesses[1]  = (unsigned)pb_ptr->accesses[1];
    accesses[2]  = (unsigned)pb_ptr->accesses[2];
    hits[0]      = (unsigned)pb_ptr->hits[0];
    hits[1]      = (unsigned)pb_ptr->hits[1];
    hits[2]      = (unsigned)pb_ptr->hits[2];
    misses[0]    = (unsigned)pb_ptr->misses[0];
    misses[1]    = (unsigned)pb_ptr->misses[1];
    misses[2]    = (unsigned)pb_ptr->misses[2];
    evictions[0] = (unsigned)pb_ptr->evictions[0];
    evictions[1] = (unsigned)pb_ptr->evictions[1];
    evictions[2] = (unsigned)pb_ptr->evictions[2];
    bypasses[0]  = (unsigned)pb_ptr->bypasses[0];
    bypasses[1]  = (unsigned)pb_ptr->bypasses[1];
    bypasses[2]  = (unsigned)pb_ptr->bypasses[2];

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5PB_get_stats */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_print_stats()
 *
 * Purpose:     Print out statistics collected for the page buffer layer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/12/18
 *
 * Changes:     Added support for md_read_splits and md_write_splits.
 *
 *                                            JRM -- 4/11/20
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_print_stats(const H5PB_t *pb_ptr)
{
    double ave_succ_search_depth       = 0.0L;
    double ave_failed_search_depth     = 0.0L;
    double ave_delayed_write           = 0.0L;
    double ave_delayed_write_ins_depth = 0.0L;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);

    HDfprintf(stdout, "\n\nPage Buffer Statistics (raw/meta/mpmde): \n\n");

    HDfprintf(stdout, "bypasses   = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->bypasses[0] + pb_ptr->bypasses[1] + pb_ptr->bypasses[2]), pb_ptr->bypasses[0],
              pb_ptr->bypasses[1], pb_ptr->bypasses[2]);

    HDfprintf(stdout, "acesses    = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->accesses[0] + pb_ptr->accesses[1] + pb_ptr->accesses[2]), pb_ptr->accesses[0],
              pb_ptr->accesses[1], pb_ptr->accesses[2]);

    HDfprintf(stdout, "hits       = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->hits[0] + pb_ptr->hits[1] + pb_ptr->hits[2]), pb_ptr->hits[0], pb_ptr->hits[1],
              pb_ptr->hits[2]);

    HDfprintf(stdout, "misses     = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->misses[0] + pb_ptr->misses[1] + pb_ptr->misses[2]), pb_ptr->misses[0],
              pb_ptr->misses[1], pb_ptr->misses[2]);

    HDfprintf(stdout, "loads      = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->loads[0] + pb_ptr->loads[1] + pb_ptr->loads[2]), pb_ptr->loads[0], pb_ptr->loads[1],
              pb_ptr->loads[2]);

    HDfprintf(stdout, "insertions = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->insertions[0] + pb_ptr->insertions[1] + pb_ptr->insertions[2]), pb_ptr->insertions[0],
              pb_ptr->insertions[1], pb_ptr->insertions[2]);

    HDfprintf(stdout, "flushes    = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->flushes[0] + pb_ptr->flushes[1] + pb_ptr->flushes[2]), pb_ptr->flushes[0],
              pb_ptr->flushes[1], pb_ptr->flushes[2]);

    HDfprintf(stdout, "evictions  = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->evictions[0] + pb_ptr->evictions[1] + pb_ptr->evictions[2]), pb_ptr->evictions[0],
              pb_ptr->evictions[1], pb_ptr->evictions[2]);

    HDfprintf(stdout, "clears     = %lld (%lld/%lld/%lld)\n",
              (pb_ptr->clears[0] + pb_ptr->clears[1] + pb_ptr->clears[2]), pb_ptr->clears[0],
              pb_ptr->clears[1], pb_ptr->clears[2]);

    HDfprintf(stdout, "max LRU len / size = %lld / %lld\n", pb_ptr->max_lru_len, pb_ptr->max_lru_size);

    HDfprintf(stdout, "LRU make space md/rd/tl skips = %lld/%lld/%lld\n", pb_ptr->lru_md_skips,
              pb_ptr->lru_rd_skips, pb_ptr->lru_tl_skips);

    HDfprintf(stdout, "hash table insertions / deletions = %lld / %lld\n", pb_ptr->total_ht_insertions,
              pb_ptr->total_ht_deletions);

    if (pb_ptr->successful_ht_searches > 0) {

        ave_succ_search_depth =
            (double)(pb_ptr->total_successful_ht_search_depth) / (double)(pb_ptr->successful_ht_searches);
    }
    HDfprintf(stdout, "successful ht searches / ave depth = %lld / %llf\n", pb_ptr->successful_ht_searches,
              ave_succ_search_depth);

    if (pb_ptr->failed_ht_searches > 0) {

        ave_failed_search_depth =
            (double)(pb_ptr->total_failed_ht_search_depth) / (double)(pb_ptr->failed_ht_searches);
    }
    HDfprintf(stdout, "failed ht searches / ave depth = %lld / %llf\n", pb_ptr->failed_ht_searches,
              ave_failed_search_depth);

    HDfprintf(stdout, "max index length / size = %lld / %lld\n", pb_ptr->max_index_len,
              pb_ptr->max_index_size);

    HDfprintf(stdout, "max rd / md / mpmde entries = %lld / %lld / %lld\n", pb_ptr->max_rd_pages,
              pb_ptr->max_md_pages, pb_ptr->max_mpmde_count);

    HDfprintf(stdout, "tick list max len / size = %lld / %lld\n", pb_ptr->max_tl_len, pb_ptr->max_tl_size);

    HDfprintf(stdout, "delayed write list max len / size = %lld / %lld\n", pb_ptr->max_dwl_len,
              pb_ptr->max_dwl_size);

    if (pb_ptr->delayed_writes > 0) {

        ave_delayed_write = (double)(pb_ptr->total_delay) / (double)(pb_ptr->delayed_writes);
        ave_delayed_write_ins_depth =
            (double)(pb_ptr->total_dwl_ins_depth) / (double)(pb_ptr->delayed_writes);
    }

    HDfprintf(stdout, "delayed writes / ave delay / ave ins depth = %lld / %llf / %llf\n",
              pb_ptr->delayed_writes, ave_delayed_write, ave_delayed_write_ins_depth);

    HDfprintf(stdout, "metadata read / write splits = %lld / %lld.\n", pb_ptr->md_read_splits,
              pb_ptr->md_write_splits);

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* H5PB_print_stats */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_add_new_page
 *
 * Purpose:	Insert a new blank page to the page buffer if the page
 *              buffer is configured to allow pages of the specified
 *              type.
 *
 *              This function is called by the MF layer when a new page
 *              is allocated to indicate to the page buffer layer that
 *              a read of the page from the file is not necessary since
 *              it's an empty page.
 *
 *              For purposes of the VFD SWMR writer, we also track pages
 *              that are inserted via this call, as the fact that the
 *              page was allocated implies that an earlier version does
 *              not exist in the HDF5 file, and thus we need not concern
 *              ourselves with delaying the write of this pages to avoid
 *              messages from the future on the reader.
 *
 *              Note that this function inserts the new page without
 *              attempting to make space.  This can result in the page
 *              buffer exceeding its maximum size.
 *
 *              Note also that it is possible that the page (marked clean)
 *              will be evicted before its first use.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/12/18
 *
 * Changes:     Modified function to function to prevent the insertion
 *              of raw data pages when operating in VFD SWMR mode.
 *
 *                                          JRM -- 3/25/20
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_add_new_page(H5F_shared_t *shared, H5FD_mem_t type, haddr_t page_addr)
{
    hbool_t       can_insert = TRUE;
    H5PB_t *      pb_ptr     = NULL;
    H5PB_entry_t *entry_ptr  = NULL;
    herr_t        ret_value  = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->pb_ptr);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);

    if (H5FD_MEM_DRAW == type) { /* raw data page insertion */

        if ((pb_ptr->min_md_pages == pb_ptr->max_pages) || (pb_ptr->vfd_swmr)) {

            can_insert = FALSE;
        }
    }
    else { /* metadata page insertion */

        if (pb_ptr->min_rd_pages == pb_ptr->max_pages) {

            can_insert = FALSE;
        }
    }

    if (can_insert) {

        if (H5PB__create_new_page(pb_ptr, page_addr, (size_t)(pb_ptr->page_size), type, TRUE, &entry_ptr) < 0)

            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "new page buffer page creation failed.")

        /* make note that this page was allocated, not loaded from file */
        entry_ptr->loaded = FALSE;

        /* updates stats */
        H5PB__UPDATE_STATS_FOR_INSERTION(pb_ptr, entry_ptr);
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_add_new_page */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_create
 *
 * Purpose:	Setup a page buffer for the supplied file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/11/18
 *
 * Changes:     Added initialization for the vfd_swmr field.  Also
 *              added code to force min_rd_pages to 0 if vfd_swrm is
 *              TRUE.  Do this since we now exclude raw data from the
 *              page buffer when operating in VFD SWMR mode.
 *
 *                                               JRM -- 3/28/20
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_create(H5F_shared_t *shared, size_t size, unsigned page_buf_min_meta_perc,
            unsigned page_buf_min_raw_perc)
{
    hbool_t vfd_swmr        = FALSE;
    hbool_t vfd_swmr_writer = FALSE;
    int     i;
    int32_t min_md_pages;
    int32_t min_rd_pages;
    H5PB_t *pb_ptr    = NULL;
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(page_buf_min_meta_perc <= 100);
    HDassert(page_buf_min_raw_perc <= 100);
    HDassert((page_buf_min_meta_perc + page_buf_min_raw_perc) <= 100);

    /* Check args */
    if (shared->fs_strategy != H5F_FSPACE_STRATEGY_PAGE)

        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "Enabling Page Buffering requires PAGE file space strategy")

    else if (size > shared->fs_page_size) {

        /* round size down to the next multiple of fs_page_size */

        hsize_t temp_size;

        temp_size = (size / shared->fs_page_size) * shared->fs_page_size;

        H5_CHECKED_ASSIGN(size, size_t, temp_size, hsize_t);

    } /* end if */
    else if (0 != size % shared->fs_page_size)

        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTINIT, FAIL, "Page Buffer size must be >= to the page size")

    /* Calculate the minimum page count for metadata and raw data
     * based on the fractions provided
     */
    min_md_pages = (int32_t)((size * page_buf_min_meta_perc) / (shared->fs_page_size * 100));
    min_rd_pages = (int32_t)((size * page_buf_min_raw_perc) / (shared->fs_page_size * 100));
    HDassert(min_md_pages >= 0);
    HDassert(min_rd_pages >= 0);
    HDassert((min_md_pages + min_rd_pages) <= (int32_t)(size / shared->fs_page_size));

    /* compute vfd_swrm and vfd_swmr_writer */
    if (H5F_SHARED_VFD_SWMR_CONFIG(shared)) {

        vfd_swmr = TRUE;

        /* force min_rd_pages to zero since raw data is exclued from
         * the page buffer in VFD SWMR mode.
         */
        min_rd_pages = 0;

        if (H5F_SHARED_INTENT(shared) & H5F_ACC_RDWR) {

            HDassert(shared->vfd_swmr_config.writer);
            vfd_swmr_writer = TRUE;
        }
    }

    /* Allocate the new page buffering structure */
    if (NULL == (pb_ptr = H5FL_MALLOC(H5PB_t)))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* initialize the new instance of H5PB_t */

    pb_ptr->magic     = H5PB__H5PB_T_MAGIC;
    pb_ptr->page_size = shared->fs_page_size;
    H5_CHECKED_ASSIGN(pb_ptr->page_size, size_t, shared->fs_page_size, hsize_t);
    pb_ptr->max_pages     = (int32_t)(size / shared->fs_page_size);
    pb_ptr->curr_pages    = 0;
    pb_ptr->curr_md_pages = 0;
    pb_ptr->curr_rd_pages = 0;
    pb_ptr->min_md_pages  = min_md_pages;
    pb_ptr->min_rd_pages  = min_rd_pages;

    pb_ptr->max_size      = size;
    pb_ptr->min_meta_perc = page_buf_min_meta_perc;
    pb_ptr->min_raw_perc  = page_buf_min_raw_perc;

    /* index */
    for (i = 0; i < H5PB__HASH_TABLE_LEN; i++)
        pb_ptr->ht[i] = NULL;
    pb_ptr->index_len        = 0;
    pb_ptr->clean_index_len  = 0;
    pb_ptr->dirty_index_len  = 0;
    pb_ptr->index_size       = 0;
    pb_ptr->clean_index_size = 0;
    pb_ptr->dirty_index_size = 0;
    pb_ptr->il_len           = 0;
    pb_ptr->il_size          = 0;
    pb_ptr->il_head          = NULL;
    pb_ptr->il_tail          = NULL;

    /* LRU */
    pb_ptr->LRU_len      = 0;
    pb_ptr->LRU_size     = 0;
    pb_ptr->LRU_head_ptr = NULL;
    pb_ptr->LRU_tail_ptr = NULL;

    /* VFD SWMR specific fields.
     * The following fields are defined iff vfd_swmr_writer is TRUE.
     */
    pb_ptr->vfd_swmr        = vfd_swmr;
    pb_ptr->vfd_swmr_writer = vfd_swmr_writer;
    pb_ptr->mpmde_count     = 0;
    pb_ptr->cur_tick        = 0;

    /* delayed write list */
    pb_ptr->max_delay    = 0;
    pb_ptr->dwl_len      = 0;
    pb_ptr->dwl_size     = 0;
    pb_ptr->dwl_head_ptr = NULL;
    pb_ptr->dwl_tail_ptr = NULL;

    /* tick list */
    pb_ptr->tl_len      = 0;
    pb_ptr->tl_size     = 0;
    pb_ptr->tl_head_ptr = NULL;
    pb_ptr->tl_tail_ptr = NULL;

    H5PB_reset_stats(pb_ptr);

    shared->pb_ptr = pb_ptr;

    /* if this is a VFD SWMR reader, inform the reader VFD that the
     * page buffer is configured.  Note that this is for sanity
     * checking, and only needed until we modify the file open
     * code to create the page buffer before any file reads in
     * the VFD SWMR reader case.  After that, this code should be
     * removed.
     *                               JRM -- 1/29/19
     */
    if ((H5F_SHARED_VFD_SWMR_CONFIG(shared)) && (0 == (H5F_SHARED_INTENT(shared) & H5F_ACC_RDWR))) {

        HDassert(shared->lf);
        HDassert(!shared->vfd_swmr_config.writer);

        H5FD_vfd_swmr_set_pb_configured(shared->lf);
    }

done:

    if (ret_value < 0) {

        if (pb_ptr != NULL) {

            pb_ptr = H5FL_FREE(H5PB_t, pb_ptr);
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_create */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_dest
 *
 * Purpose:	Flush (if necessary) and evict all entries in the page
 *              buffer, and then discard the page buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/22/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_dest(H5F_shared_t *shared)
{
    int           i;
    H5PB_t *      pb_ptr    = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    H5PB_entry_t *evict_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(shared);

    /* flush and destroy the page buffer, if it exists */
    if (shared->pb_ptr) {

        pb_ptr = shared->pb_ptr;

        H5PB_log_access_by_size_counts(pb_ptr);

        HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);

        /* the current implementation if very inefficient, and will
         * fail if there are any outstanding delayed writes -- must fix this
         */
        for (i = 0; i < H5PB__HASH_TABLE_LEN; i++) {

            entry_ptr = pb_ptr->ht[i];

            while (entry_ptr) {

                HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);

                evict_ptr = entry_ptr;
                entry_ptr = entry_ptr->ht_next;

                if (evict_ptr->is_dirty) {

                    if (H5PB__flush_entry(shared, pb_ptr, evict_ptr) < 0)

                        HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "Can't flush entry")
                }

                if (H5PB__evict_entry(shared, evict_ptr, TRUE, TRUE) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "forced eviction failed")

                entry_ptr = pb_ptr->ht[i];
            }
        }

        /* regular operations fields */
        HDassert(pb_ptr->curr_pages == 0);
        HDassert(pb_ptr->curr_md_pages == 0);
        HDassert(pb_ptr->curr_rd_pages == 0);
        HDassert(pb_ptr->index_len == 0);
        HDassert(pb_ptr->index_size == 0);
        HDassert(pb_ptr->LRU_len == 0);
        HDassert(pb_ptr->LRU_size == 0);
        HDassert(pb_ptr->LRU_head_ptr == NULL);
        HDassert(pb_ptr->LRU_tail_ptr == NULL);

        /* VFD SWMR fields */
        HDassert(pb_ptr->dwl_len == 0);
        HDassert(pb_ptr->dwl_size == 0);
        HDassert(pb_ptr->dwl_head_ptr == NULL);
        HDassert(pb_ptr->dwl_tail_ptr == NULL);

        HDassert(pb_ptr->tl_len == 0);
        HDassert(pb_ptr->tl_size == 0);
        HDassert(pb_ptr->tl_head_ptr == NULL);
        HDassert(pb_ptr->tl_tail_ptr == NULL);

        pb_ptr->magic  = 0;
        shared->pb_ptr = H5FL_FREE(H5PB_t, pb_ptr);
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_dest */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_flush
 *
 * Purpose:	If the page buffer is defined, flush all entries.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/22/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_flush(H5F_shared_t *shared)
{
    int           i;
    H5PB_t *      pb_ptr    = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    H5PB_entry_t *flush_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(shared);

    pb_ptr = shared->pb_ptr;

    if (pb_ptr) {

        HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);

        /* the current implementation is very inefficient, and will
         * fail if there are any delayed writes -- must fix this
         */
        for (i = 0; i < H5PB__HASH_TABLE_LEN; i++) {

            entry_ptr = pb_ptr->ht[i];

            while (entry_ptr) {

                HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);

                flush_ptr = entry_ptr;
                entry_ptr = entry_ptr->ht_next;

                if (flush_ptr->is_dirty) {

                    if (flush_ptr->delay_write_until != 0)
                        continue;

                    if (H5PB__flush_entry(shared, pb_ptr, flush_ptr) < 0)

                        HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "Can't flush entry")
                }
            }
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_flush */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_page_exists
 *
 * Purpose:	Test to see if a page buffer page exists at the specified
 *              address.  Set *page_exists_ptr to TRUE or FALSE accordingly.
 *
 *              This function exists for the convenience of the test
 *              code
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/22/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_page_exists(H5F_shared_t *shared, haddr_t addr, hbool_t *page_exists_ptr)
{
    uint64_t      page;
    H5PB_t *      pb_ptr    = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(shared);
    HDassert(shared->pb_ptr);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(page_exists_ptr);

    /* Calculate the page offset */
    page = (addr / pb_ptr->page_size);

    /* the supplied address should be page aligned */
    HDassert(addr == page * pb_ptr->page_size);

    /* Search for page in the hash table  */
    H5PB__SEARCH_INDEX(pb_ptr, page, entry_ptr, FAIL)

    HDassert((NULL == entry_ptr) || (entry_ptr->addr == addr));

    *page_exists_ptr = (entry_ptr != NULL);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_page_exists */

static void
H5PB_count_meta_access_by_size(H5PB_t *pb, size_t size)
{
    const size_t nslots = NELMTS(pb->access_size_count);
    size_t       i, hi;

    for (hi = pb->page_size, i = 0; i < nslots - 1; i++, hi *= 2) {
        if (size <= hi)
            break;
    }
    pb->access_size_count[i]++;
}

static void
H5PB_log_access_by_size_counts(const H5PB_t *pb)
{
    const size_t nslots = NELMTS(pb->access_size_count);
    size_t       i, lo, hi;

    for (lo = 0, hi = pb->page_size, i = 0; i < nslots - 1; i++, lo = hi + 1, hi *= 2) {
    }
}

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_read
 *
 * Purpose:	Satisfy the read from the page buffer if possible.
 *
 *              1) If the page buffer is disabled, simply read from the
 *                 HDF5 file and return.
 *
 *              2) If the read is for raw data, and the page buffer is
 *                 configured for metadata only (i.e. min_md_pages ==
 *                 max_pages), or if we are operating in VFD SWMR mode
 *                 (i.e. vfd_swmr == TRUE), simply read from the HDF5
 *                 file and return.
 *
 *              3) If the read is for raw data, and is of page size or
 *                 larger, read it directly from the HDF5 file.
 *
 *                 It is possible that the page buffer contains dirty pages
 *                 that intersect with the read -- test for this and update
 *                 the read buffer from the page buffer if any such pages
 *                 exist.
 *
 *                 Note that no pages are inserted into the page buffer in
 *                 this case.
 *
 *              4) If the read is for raw data, and it is of size less
 *                 than the page size, satisfy the read from the page
 *                 buffer, loading and inserting pages into the
 *                 page buffer as necessary
 *
 *              5) If the read is for metadata, and the page buffer is
 *                 configured for raw data only (i.e. min_rd_pages ==
 *                 max_pages), simply read from the HDF5 file and return.
 *
 *              The free space manager guarantees that allocations larger
 *              than one page will be page alligned, and that allocations
 *              of size less than or equal to page size will not cross page
 *              boundaries.  Further, unlike raw data, metadata is always
 *              written and read atomically.
 *
 *              In principle, this should make it easy to discriminate
 *              between small and multi-page metadata entries so that
 *              pages containing the former will be buffered and the
 *              latter be read directly from file.
 *
 *              Unfortunately, there are several flies in the ointment.
 *
 *              First, the fixed and extensible array on disk data
 *              structures allocate multiple metadata cache entries in
 *              a single block, and use this fact to make the addresses
 *              of all but the first entry in the block computable.  While
 *              this simplifies the fixed and extensible array on disk data
 *              structures, if complicates the metadata cache and the page
 *              buffer.  Needless to say, the correct solution to this
 *              problem is to remove the complexity at its source.  However,
 *              for now, we must code around the problem.
 *
 *              Thus, this function must examine each read request
 *              to determine if it crosses page boundaries and is not for
 *              two or more complete pages.  If it does, and it is one of
 *              the fixed or extensible array entries that is sub-allocated
 *              from a larger space allocation, the read request must be
 *              split into the minimal set of read requests that either
 *              don't cross page boundaries, or are page aligned and
 *              consist of an integral number of pages.
 *
 *
 *              Second, the metadata cache does not always know the
 *              size of metadata entries when it tries to read them.  In
 *              such cases, it issues speculative reads that may be either
 *              smaller or larger than the actual size of the piece of
 *              metadata that is finally read.
 *
 *              Since we are guaranteed that all metadata allocations larger
 *              that one page are page aligned (with the exception of those
 *              sub-allocated from larger allocations -- which we deal with
 *              by splitting I/O requests as discussed above), we can safely
 *              clip at the page boundary any non page aligned metadata
 *              read that crosses page boundaries.
 *
 *              However, page aligned reads could wind up being either
 *              small or multi-page.  This results in two scenarios that
 *              we must handle:
 *
 *                  a) A page aligned read of size less than one page
 *                     turns out to be mult-page.
 *
 *                     In this case, the initial speculative read will
 *                     result in a page load and insertion into the page
 *                     buffer.  This page must be evicted on the subsequent
 *                     read of size greater than page size.
 *
 *                     In the context of VFD SWMR, it is also possible that
 *                     that the multi-page metadata entry is already in the
 *                     page buffer -- in which case the initial read should
 *                     be satisfied from the multi-page page buffer entry.
 *
 *                  b) A page aligned, larger than one page read turns out
 *                     to be small (less than one page).
 *
 *                     If there is already a page in the page buffer with
 *                     same address, we can safely clip the original
 *                     read to page size
 *
 *              The above considerations resolve into the following cases:
 *
 *              6) If the read is for metadata and not page aligned, clip
 *                 the read to the end of the current page if necessary.
 *                 Load the relevant page if necessary and satisfy the
 *                 read from the page buffer.  Note that it there is an
 *                 existing page, it must not be a multi-page metadata
 *                 entry.  It it is, flag an error.
 *
 *              7) If the read is for metadata, is page aligned, is larger
 *                 than one page, and there is no entry in the page buffer,
 *                 satisfy the read from the file
 *
 *              8) If the read is for metadata, is page aligned, is larger
 *                 than one page, and there is a regular entry at the target
 *                 page address, test to see if the read is speculative.
 *
 *                 If it is not, evict the page, and satisfy the read from
 *                 file.  Flag an error if the page was dirty.
 *
 *                 If it is, clip the read to one page, and satisfy the
 *                 read from the existing regular entry.
 *
 *              9) If the read is for metadata, is page aligned, is larger
 *                 than one page, and there is a multi-page metadata entry
 *                 at the target page address, test to see if
 *                 pb_ptr->vfd_swmr_write is TRUE.
 *
 *                 If it is, satisfy the read from the multi-page metadata
 *                 entry, clipping the read if necessary.
 *
 *                 if pb_ptr->vfd_swmr_write is FALSE, flag an error.
 *
 *             10) If the read is for metadata, is page aligned, is no
 *                 larger than a page, test to see if the page buffer
 *                 contains a page at the target address.
 *
 *                 If it doesn't, load the page and satisfy the read
 *                 from it.
 *
 *                 If it contains a regular page entry, satisfy the read
 *                 from it.
 *
 *                 If it contains a multipage metadata entry at the target
 *                 address, satisfy the read from the multi-page metadata
 *                 entry if pb_ptr->vfd_swmr_write is TRUE, and flag an
 *                 error otherwise.
 *
 *              Observe that this function handles casses 1, 2, and 5
 *              directly, calls H5PB_read_raw() for cases 3 & 4, and
 *              calls H5PB_read_meta() for cases 6), 7, 8, 9), and 10).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/11/18
 *
 * Changes:     Updated for discovery of the fact that the fixed and
 *              extensible array data structures allocate multiple
 *              metadata cache entries in a single block, and thus
 *              violate that invarient that metadata entries either
 *              do not cross page boundaries, or are page aligned.
 *
 *                                               JRM -- 3/28/20
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5PB_read(H5F_shared_t *shared, H5FD_mem_t type, haddr_t addr, size_t size, void *buf /*out*/)
{
    H5PB_t *pb_ptr;               /* Page buffer for this file */
    hbool_t bypass_pb  = FALSE;   /* Whether to bypass page buffering */
    hbool_t split_read = FALSE;   /* whether the read must be split */
    herr_t  ret_value  = SUCCEED; /* Return value */

    /* the following six fields are defined iff split_read is TRUE */
    haddr_t prefix_addr = HADDR_UNDEF; /* addr of prefix -- if defined */
    haddr_t body_addr   = HADDR_UNDEF; /* addr of body -- if defined */
    haddr_t suffix_addr = HADDR_UNDEF; /* addr of suffix -- if defined */
    size_t  prefix_size = 0;           /* size of prefix */
    size_t  body_size   = 0;           /* size of body */
    size_t  suffix_size = 0;           /* size of suffix */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);

    pb_ptr = shared->pb_ptr;

    if (pb_ptr != NULL && type != H5FD_MEM_DRAW)
        H5PB_count_meta_access_by_size(pb_ptr, size);

    if (pb_ptr == NULL) {

        bypass_pb = TRUE; /* case 1) -- page buffer is disabled */
    }
    else {

        HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);

        if (H5FD_MEM_DRAW == type) { /* raw data read */

            if ((pb_ptr->min_md_pages == pb_ptr->max_pages) || (pb_ptr->vfd_swmr)) {

                /* case 2) -- page buffer configured for metadata only
                 *            or vfd swmr.
                 */
                bypass_pb = TRUE;
            }
        }
        else { /* metadata read */

            if (pb_ptr->min_rd_pages == pb_ptr->max_pages) {

                /* case 5) -- page buffer configured for raw data only */
                bypass_pb = TRUE;
            }
            else {
                /* determine whether the read request must be split,
                 * and if so, compute the start points and sizes of
                 * of the sections.
                 *
                 * Note: The following code is almost identical to the
                 *       similar code in H5PB_write().  Thus, on the surface,
                 *       it is an obvious candidate for refactoring into a
                 *       function 0r macro.
                 *
                 *       However, there are subtle differences between
                 *       the two pieces of code which are driven by the
                 *       possibility of speculative reads.
                 *
                 *       More to the point, further changes may be necessary.
                 *       Thus we should wait on refactoring until this code has
                 *       been in daily use for some time, and it is clear
                 *       that further changes are unlikely.
                 */
                int      mdc_client_id = -1; /* id of mdc client, or -1 if undef */
                uint64_t start_page;         /* page index of first page in read */
                uint64_t second_page;        /* page index of second page in read */
                uint64_t end_page;           /* page index of last page in read */
                uint64_t body_page;          /* page index of start of body */
                haddr_t  start_page_addr;    /* addr of first page in read */
                haddr_t  second_page_addr;   /* addr of second page in read */
                haddr_t  end_page_addr;      /* addr of last page in read */
                haddr_t  end_addr;           /* addr of last byte in read */

                /* Calculate the aligned address of the first page */
                start_page      = (addr / pb_ptr->page_size);
                start_page_addr = start_page * pb_ptr->page_size;

                /* Calculate the aligned address of the last page */
                end_addr      = addr + (haddr_t)(size - 1);
                end_page      = end_addr / (haddr_t)(pb_ptr->page_size);
                end_page_addr = end_page * pb_ptr->page_size;

                HDassert(start_page_addr <= addr);
                HDassert(addr < start_page_addr + (haddr_t)(pb_ptr->page_size));

                HDassert(start_page <= end_page);
                HDassert(end_page_addr <= ((addr + (haddr_t)size - 1)));
                HDassert((addr + (haddr_t)size - 1) < (end_page_addr + pb_ptr->page_size));

                /* test to see if the read crosses a page boundary, and
                 * does not start on a page boundary, and is not of an
                 * integral number of pages.
                 */
                if ((start_page < end_page) &&
                    (!((addr == start_page_addr) &&
                       (end_page_addr + (haddr_t)(pb_ptr->page_size) == end_addr + 1)))) {

                    /* the read crosses a page boundary and is not
                     * page aligned and of length some multiple of page size.
                     *
                     * Test to see if the read is for a metadata entry that
                     * is sub-allocated from a larger space allocation.
                     *
                     * Note that the following test may have to be
                     * adjusted.
                     */
                    mdc_client_id = H5C_get_curr_io_client_type(shared->cache);

                    if ((mdc_client_id == (int)H5AC_EARRAY_DBLK_PAGE_ID) ||
                        (mdc_client_id == (int)H5AC_FARRAY_DBLK_PAGE_ID)) {

                        split_read = TRUE;
                    }
                }

                if (split_read) {

                    /* compute the base addresses and length of the prefix,
                     * body, and suffix of the read, where these terms are
                     * defined as follows:
                     *
                     * prefix: All bytes from addr to the first page address
                     *         at or after addr.  If addr == start_page_addr,
                     *         the prefix is empty.
                     *
                     * body:   All bytes from the first page address covered
                     *         by the read up to but not including the last
                     *         page address in the read.  Note that the
                     *         length of the body must be a multiple of the
                     *         page size.  If only one page address is
                     *         included in the read, the body is empty.
                     *
                     * suffix: All bytes from the last page address in the
                     *         read until the end of the read.  If the
                     *         read ends on a page boundary, the suffix is
                     *         empty.
                     *
                     * Since we know that the read crosses at least one
                     * page boundary, and we have aleady filtered out the
                     * body only case, at least two of the above must be
                     * non-empty.
                     */

                    second_page      = start_page + 1;
                    second_page_addr = (haddr_t)(second_page * pb_ptr->page_size);

                    if (addr > start_page_addr) { /* prefix exists */

                        prefix_addr = addr;
                        prefix_size = (size_t)(second_page_addr - addr);

                        HDassert(prefix_addr > start_page_addr);
                        HDassert(prefix_size < pb_ptr->page_size);
                        HDassert(((size_t)(addr - start_page_addr) + prefix_size) == pb_ptr->page_size);
                    }

                    if (size - prefix_size >= pb_ptr->page_size) {

                        /* body exists */

                        if (addr == start_page_addr) {

                            body_page = start_page;
                            body_addr = start_page_addr;
                        }
                        else {

                            body_page = second_page;
                            body_addr = second_page_addr;
                        }

                        if (end_addr < end_page_addr + (haddr_t)(pb_ptr->page_size - 1)) {

                            /* suffix exists */
                            body_size = (size_t)(end_page - body_page) * pb_ptr->page_size;
                        }
                        else {

                            /* suffix is empty */
                            body_size = (size_t)(end_page - body_page + 1) * pb_ptr->page_size;
                        }

                        HDassert((body_page == start_page) || (body_page == start_page + 1));

                        HDassert(body_addr == (haddr_t)(body_page * pb_ptr->page_size));

                        HDassert(body_size < size);
                        HDassert(body_size >= pb_ptr->page_size);

                        HDassert(body_addr == addr + (haddr_t)prefix_size);
                        HDassert((body_addr + (haddr_t)body_size) <= (end_addr + 1));
                    }

                    if (end_addr < end_page_addr + (haddr_t)(pb_ptr->page_size - 1)) {

                        suffix_addr = end_page_addr;
                        suffix_size = (end_addr + 1) - end_page_addr;

                        HDassert(suffix_addr == addr + (haddr_t)(prefix_size + body_size));
                    }

                    HDassert(size == prefix_size + body_size + suffix_size);
                }
            }
        }
    }

#ifdef H5_HAVE_PARALLEL
    /* at present, the page buffer must be disabled in the parallel case.
     * However, just in case ...
     */
    if (H5F_SHARED_HAS_FEATURE(shared, H5FD_FEAT_HAS_MPI)) {

        bypass_pb = TRUE;

    }  /* end if */
#endif /* H5_HAVE_PARALLEL */

    if (bypass_pb) { /* cases 1, 2. and 5 */

        if (H5FD_read(shared->lf, type, addr, size, buf) < 0)

            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "read through  failed")

        /* Update statistics */
        if (pb_ptr) {

            H5PB__UPDATE_STATS_FOR_BYPASS(pb_ptr, type, size);
        }
    }
    else {

        if (H5FD_MEM_DRAW == type) { /* cases 3 and 4 */

            if (H5PB__read_raw(shared, type, addr, size, buf) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "H5PB_read_raw() failed")
        }
        else if (split_read) {

            /* handle the sub-allocated entry case */

            /* read prefix if it exists */
            if (prefix_size > 0) {

                if (H5PB__read_meta(shared, type, prefix_addr, prefix_size, buf) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "H5PB_read_meta() failed on prefix")
            }

            /* read body -- if it exists. */
            if (body_size > 0) {

                if (H5PB__read_meta(shared, type, body_addr, body_size,
                                    (void *)((uint8_t *)buf + prefix_size)) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "H5PB_read_meta() failed on body")
            }

            /* read suffix -- if it exists. */
            if (suffix_size > 0) {

                if (H5PB__read_meta(shared, type, suffix_addr, suffix_size,
                                    (void *)((uint8_t *)buf + prefix_size + body_size)) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "H5PB_read_meta() failed on suffix")
            }

            H5PB__UPDATE_STATS_FOR_READ_SPLIT(pb_ptr)
        }
        else { /* pass to H5PB_read_meta() -- cases 6, 7, 8, 9, & 10 */

            if (H5PB__read_meta(shared, type, addr, size, buf) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "H5PB_read_meta() failed")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_read() */

/* Remove the entry corresponding to lower-file page number `page`.
 * Return 0 if there was no such entry or if the entry was removed
 * successfully.  Return -1 on error.
 *
 * If `only_mark` is true, then the entry corresponding shadow-index
 * entry is not removed.  Instead, it is marked as garbage.  This is
 * a stop-gap fix for a performance problem in H5PB_dest(): deleting
 * all of the index entries took time quadratic in their number because
 * this routine performs an O(n) copy of index entries.
 */
static int
shadow_idx_entry_remove(H5F_shared_t *shared, uint64_t page, hbool_t only_mark)
{
    ptrdiff_t                  i;
    H5FD_vfd_swmr_idx_entry_t *entry;

    entry = vfd_swmr_pageno_to_mdf_idx_entry(shared->mdf_idx, shared->mdf_idx_entries_used, page, FALSE);

    if (entry == NULL)
        return 0;

    if (shared->vfd_swmr_writer && entry->md_file_page_offset != 0) {
        if (shadow_image_defer_free(shared, entry) != 0)
            return -1;
        entry->md_file_page_offset = 0;
    }

    if (only_mark) {
        entry->garbage = TRUE;
        return 0;
    }

    i = entry - shared->mdf_idx;

    if (shared->mdf_idx_entries_used > i + 1) {
        const size_t ntocopy = (size_t)(shared->mdf_idx_entries_used - (i + 1));
        HDmemmove(&shared->mdf_idx[i], &shared->mdf_idx[i + 1], ntocopy * sizeof(shared->mdf_idx[i + 1]));
    }
    shared->mdf_idx_entries_used--;
    return 0;
}

/*-------------------------------------------------------------------------
 *
 * Function:    H5PB_remove_entry
 *
 * Purpose:     Remove possible metadata entry with ADDR from the PB cache.
 *
 *              This is in response to the data corruption bug from fheap.c
 *              with page buffering + page strategy.
 *
 *              Note: Large metadata page bypasses the PB cache.
 *
 *              Note: Update of raw data page (large or small sized) is
 *                    handled by the PB cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Feb 2017
 *
 * Changes:     Reworked function for re-implementation of the page buffer.
 *
 *              In the context of VFD SWMR, it is possible that the
 *              discarded page or multi-page metadata entry has been
 *              modified during the current tick and/or is subject to a
 *              delayed write.  We must detect this, and remove the entry
 *              from the tick list and/or delayed write list before it is
 *              evicted.
 *
 *              Vailin: I think we need to do this for raw data as well.
 *
 *                                               JRM -- 10/23/18
 *
 *              We also need to evict modified pages from the page
 *              buffer in the VFD SWMR reader case to avoid message from
 *              the past bugs.  This function will serve for this for
 *              now, but for efficiency, we may want a version that takes
 *              a list of pages instead.
 *
 *                                               JRM -- 12/30/18
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_remove_entry(H5F_shared_t *shared, haddr_t addr)
{
    uint64_t      page;
    H5PB_t *      pb_ptr    = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->pb_ptr);

    pb_ptr = shared->pb_ptr;

    /* Calculate the page offset */
    page = (addr / pb_ptr->page_size);

    HDassert(addr == page * pb_ptr->page_size);

    /* Search for page in the hash table */
    H5PB__SEARCH_INDEX(pb_ptr, page, entry_ptr, FAIL)

    if (entry_ptr) {

        HDassert(entry_ptr->addr == addr);

        /* A page or a metadata multi-page with vfd_swmr_writer (case 7) */
        HDassert((entry_ptr->size == pb_ptr->page_size) ||
                 (entry_ptr->size > pb_ptr->page_size && entry_ptr->mem_type != H5FD_MEM_DRAW &&
                  pb_ptr->vfd_swmr_writer));

        if (entry_ptr->modified_this_tick) {

            H5PB__REMOVE_FROM_TL(pb_ptr, entry_ptr, FAIL);

            entry_ptr->modified_this_tick = FALSE;
        }

        if (entry_ptr->delay_write_until > 0) {

            entry_ptr->delay_write_until = 0;

            H5PB__REMOVE_FROM_DWL(pb_ptr, entry_ptr, FAIL)

            if (!(entry_ptr->is_mpmde)) {

                H5PB__UPDATE_RP_FOR_INSERTION(pb_ptr, entry_ptr, FAIL);
            }
        }

        /* if the entry is dirty, mark it clean before we evict */
        if ((entry_ptr->is_dirty) && (H5PB__mark_entry_clean(pb_ptr, entry_ptr) < 0))

            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry clean failed")

        if (H5PB__evict_entry(shared, entry_ptr, TRUE, FALSE) < 0)

            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "forced eviction failed")

        HDassert(!shared->vfd_swmr_writer ||
                 vfd_swmr_pageno_to_mdf_idx_entry(shared->mdf_idx, shared->mdf_idx_entries_used, page,
                                                  FALSE) == NULL);
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_remove_entry */

/*-------------------------------------------------------------------------
 *
 * Function:    H5PB_remove_entries
 *
 * Purpose:     Remove entries in the page buffer associated with a
 *              newly freed multi-page block of file space.
 *
 *              There are several possible situations here.
 *
 *              In the context of metadata, there are two possible cases.
 *
 *              1) The block of file space is associated with a metadata
 *                 entry.
 *
 *                 In regular operating mode, this entry will not be
 *                 cached in the page buffer, so there should be nothing
 *                 to do.
 *
 *                 In VFD SWMR mode, the entry may be cached in a single
 *                 multi-page entry.
 *
 *              2) The block of file space has been sub-allocated
 *                 into multiple metadata entries (i.e. fixed and extensible
 *                 array).  In this case, the individual entries may cross
 *                 boundaries without being page aligned -- however, for
 *                 purposes of the page buffer, I/O requests on these
 *                 entries will have been broken up into requests that
 *                 either do not cross page boundaries or are page aligned.
 *
 *              In the context of raw data, the page buffer may or may
 *              not contain regular entries scattered over the space
 *              touched by the newly freed file space.
 *
 *              In all contexts, there is no guarantee that the page buffer
 *              will contain any of the possible entries.
 *
 *              Space allocations larger than one page must be page alligned.
 *              Further, any space between the end of a multi-page allocation
 *              and the next page boundary will remain un-allocated until after
 *              the original allocation is freed.  This implies that:
 *
 *              1) The address passed into this call must be page aligned.
 *
 *              2) The page buffer may safely discard any page that
 *                 intersects with the newly freed file space allocation.
 *
 *              The bottom line here is that we must scan the page buffer
 *              index, and discard all entries that intersect the supplied
 *              address and length.  As a sanity check, we must verify that
 *              any such entries don't overlap.
 *
 *              Also, in the context of the VFD SWMR write, it is possible
 *              that the discarded pages will reside in the tick list or
 *              the delayed write list -- if so, they must be removed
 *              prior to eviction.
 *
 *              Note:
 *
 *              This function scans the page buffer hash table to
 *              find entries to remove.  While this is normally
 *              pretty in-expensive, a very large (i.e. GB) file
 *              space free may impose significant cost.
 *
 *              As best I understand it, such frees are rare, so
 *              the current solution should be good enough for now.
 *              However, if we determine that the current solution
 *              is too expensive, two alternate solutions come to mine.
 *
 *              a) Scan the index list instead of the hash table
 *                 if the free is sufficiently large.  Also, skip
 *                 entirely if the page buffer doesn't contain any
 *                 pages of the appropriate type.
 *
 *              b) Whenever writing a large metadata entry, scan for
 *                 intersecting entries and delete them.  (potential
 *                 issues with fixed and variable array entries are
 *                 dealt with via the splitting mechanism.)  In this
 *                 case we would also have to simply ignore writes
 *                 beyond EOA on flush or close.
 *
 *                 Note that we already scan for intersecting entries
 *                 on large raw data writes -- with possible performance
 *                 issues for large writes.
 *
 *                                            JRM -- 4/25/20
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer 4/25/20
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5PB_remove_entries(H5F_shared_t *shared, haddr_t addr, hsize_t size)
{
    uint64_t      i;
    uint64_t      start_page;
    uint64_t      end_page;
    int64_t       entry_pages = 0;
    hsize_t       entry_size;
    H5PB_t *      pb_ptr    = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->pb_ptr);

    pb_ptr = shared->pb_ptr;

    /* Calculate the start_page offset */
    start_page = (addr / pb_ptr->page_size);

    HDassert(addr == start_page * pb_ptr->page_size);

    /* Calculate the end_page offset */
    end_page = ((addr + (haddr_t)(size - 1)) / pb_ptr->page_size);

    HDassert(start_page <= end_page);
    HDassert(((end_page - start_page) * pb_ptr->page_size) <= size);
    HDassert(size <= ((end_page - start_page + 1) * pb_ptr->page_size));

    for (i = start_page; i <= end_page; i++) {
        /* test to see if page i exists */
        H5PB__SEARCH_INDEX(pb_ptr, i, entry_ptr, FAIL)

        if (entry_ptr) {

            /* verify that this entry doesn't overlap with a previously
             * visited entry.
             */
            HDassert(entry_pages <= 0);

            entry_size  = entry_ptr->size;
            entry_pages = (int64_t)(entry_size / pb_ptr->page_size);

            if ((uint64_t)entry_pages * pb_ptr->page_size < entry_size) {

                entry_pages++;
            }

            /* remove the entry */
            if (H5PB_remove_entry(shared, entry_ptr->addr) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "H5PB_remove_entry() failed")
        }
        entry_pages--;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_remove_entries() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_update_entry
 *
 * Purpose:	In PHDF5, metadata cache entries that are written by other
 *              processes are simply marked clean in the current process.
 *              However, if the page buffer is enabled, entries marked
 *              clean must still be written to the page buffer so as to
 *              keep the contents of metadata pages consistent on all
 *              processes.
 *
 *              Do this as follows:
 *
 *              1) Test to see if the page buffer is configured to accept
 *                 metadata pages.  If it isn't, return.
 *
 *              2) Test to see if the page buffer contains the page that
 *                 contains the supplied metadata cache entry.  If it
 *                 doesn't, return.
 *
 *              3) Write the supplied buffer to page at the appropriate
 *                 offset.
 *
 *              Note that at present, page buffering is disabled in the
 *              parallel case.  Thus this function has not been tested.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/23/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_update_entry(H5PB_t *pb_ptr, haddr_t addr, size_t size, const void *buf)
{
    uint64_t      page;
    size_t        offset;
    H5PB_entry_t *entry_ptr = NULL;
    haddr_t       page_addr;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(size > 0);
    HDassert(size <= pb_ptr->page_size);
    HDassert(buf);

    if (pb_ptr->min_rd_pages < pb_ptr->max_pages) {

        /* page buffer is configured to accept metadata pages */

        /* Calculate the aligned address of the containing page */
        page      = (addr / pb_ptr->page_size);
        page_addr = page * pb_ptr->page_size;

        H5PB__SEARCH_INDEX(pb_ptr, page, entry_ptr, FAIL)

        if (entry_ptr) {

            HDassert(entry_ptr->is_metadata);
            HDassert(!(entry_ptr->is_mpmde));
            HDassert(addr + size <= page_addr + pb_ptr->page_size);

            offset = addr - page_addr;

            HDmemcpy(((uint8_t *)(entry_ptr->image_ptr) + offset), buf, size);

            /* should we mark the page dirty?  If so, replace the following
             * with a call to H5PB__mark_entry_dirty()
             */
            H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_update_entry */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_vfd_swmr__release_delayed_writes
 *
 * Purpose:	After the tick list has been released, and before the
 *              beginning of the next tick, we must scan the delayed
 *              write list, and release those entries whose delays have
 *              expired.
 *
 *              Note that pages of metadata, and multi-page metadata entries
 *              are handled differently.
 *
 *              Regular pages are removed from the delayed write list and
 *              inserted in the replacement policy
 *
 *              In contrast, multi-page metadata entries are simply
 *              flushed and evicted.
 *
 *              Since the delayed write list is sorted in decreasing
 *              delay_write_until order, we start our scan at the bottom
 *              of the delayed write list and continue upwards until no
 *              expired entries remain.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 11/15/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_vfd_swmr__release_delayed_writes(H5F_shared_t *shared)
{
    H5PB_t *      pb_ptr    = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->vfd_swmr);
    HDassert(shared->vfd_swmr_writer);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->vfd_swmr_writer);

    while (pb_ptr->dwl_tail_ptr && pb_ptr->dwl_tail_ptr->delay_write_until <= shared->tick_num) {

        entry_ptr = pb_ptr->dwl_tail_ptr;

        HDassert(entry_ptr->is_dirty);

        entry_ptr->delay_write_until = 0;

        H5PB__REMOVE_FROM_DWL(pb_ptr, entry_ptr, FAIL)

        if (entry_ptr->is_mpmde) { /* flush and evict now */

            if (H5PB__flush_entry(shared, pb_ptr, entry_ptr) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "flush of mpmde failed")

            if (H5PB__evict_entry(shared, entry_ptr, TRUE, FALSE) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "eviction of mpmde failed")
        }
        else { /* insert it in the replacement policy */

            H5PB__UPDATE_RP_FOR_INSERT_APPEND(pb_ptr, entry_ptr, FAIL)
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_vfd_swmr__release_delayed_writes() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_vfd_swmr__release_tick_list
 *
 * Purpose:	After the metadata file has been updated, and before the
 *              beginning of the next tick, we must release the tick list.
 *
 *              This function performs this function.
 *
 *              In passing, flush and evict any multi-page metadata entries
 *              that are not subject to a delayed write.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 11/12/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_vfd_swmr__release_tick_list(H5F_shared_t *shared)
{
    H5PB_t *      pb_ptr    = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->vfd_swmr);
    HDassert(shared->vfd_swmr_writer);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->vfd_swmr_writer);

    /* remove all entries from the tick list */
    while (pb_ptr->tl_head_ptr) {

        entry_ptr = pb_ptr->tl_head_ptr;

        H5PB__REMOVE_FROM_TL(pb_ptr, entry_ptr, FAIL)

        entry_ptr->modified_this_tick = FALSE;

        if (entry_ptr->is_mpmde) {

            HDassert(entry_ptr->is_dirty);

            if (entry_ptr->delay_write_until == 0) {

                /* flush and evict the multi-page metadata entry immediately */
                if (H5PB__flush_entry(shared, pb_ptr, entry_ptr) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "flush of mpmde failed")

                if (H5PB__evict_entry(shared, entry_ptr, TRUE, FALSE) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "eviction of mpmde failed")
            }
        }
        /* if the entry is not a multi-page metadata entry, it must already
         * be on either the replacment policy or the delayed write list.
         * In either case, it will be flush when possible and necessary.
         */
    }

    HDassert(pb_ptr->tl_head_ptr == NULL);
    HDassert(pb_ptr->tl_tail_ptr == NULL);
    HDassert(pb_ptr->tl_len == 0);
    HDassert(pb_ptr->tl_size == 0);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_vfd_swmr__release_tick_list */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_vfd_swmr__set_tick
 *
 * Purpose:	At the beginning of each tick, the page buffer must be told
 *              to synchronize its copy of the current tick with that of
 *              the file to which the page buffer belongs.
 *
 *              This function performs this function.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 11/20/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_vfd_swmr__set_tick(H5F_shared_t *shared)
{
    H5PB_t *pb_ptr    = NULL;
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->vfd_swmr);
    HDassert(shared->vfd_swmr_writer);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->vfd_swmr_writer);

    /* the tick must always increase by 1 -- verify this */
    if (shared->tick_num != pb_ptr->cur_tick + 1)

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL,
                    "shared->tick_num (%" PRIu64 ") != (%" PRIu64 ") pb_ptr->cur_tick + 1 ?!?!",
                    shared->tick_num, pb_ptr->cur_tick)

    pb_ptr->cur_tick = shared->tick_num;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_vfd_swmr__release_tick_list */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_vfd_swmr__update_index
 *
 * Purpose:	In the VFD SWMR writer, all metadata writes to the page
 *              buffer during a tick are buffered in the page buffer in
 *              the tick list.  Further, the metadata cache is flushed
 *              to the page buffer at the end of the tick so that all
 *              metadata changes during the tick are reflected in the
 *              tick list.
 *
 *              Once this is done, the internal representation of the
 *              metadata file index must be updated from the tick list
 *              so that the metadata file can be updated, and the tick
 *              list can be emptied and prepared to buffer metadata changes
 *              in the next tick.
 *
 *              This function is called to accomplish this.  Its cycle of
 *              operation is as follows:
 *
 *              1) Scan the tick list.  For each entry (*entry), test
 *                 to see if it appears in the index.
 *
 *                 If it does the entry must have been modified in the
 *                 past tick.  Update the index entry (*ie_ptr) as follows:
 *
 *                 a) Set ie_ptr->entry_ptr = entry->image_ptr.  This
 *                    is needed to give the metadata file update code
 *                    access to the image of the target page or multi-page
 *                    multi-date entry.  Note that ie_ptr->entry_ptr will
 *                    be set to NULL as soon as the metadata file is updated,
 *                    so the buffer pointed to by entry->image_ptr can
 *                    be safely discarded at any time after the metadata
 *                    file update.
 *
 *                 b) Set ie_ptr->tick_of_last_change to the current tick.
 *
 *                 c) If entry->is_dirty, set ie_ptr->clean to FALSE.
 *                    If entry->is_dirty is FALSE, set ie_ptr->clean
 *                    to TRUE and set ie_ptr->tick_of_last_flush to the
 *                    current tick.
 *
 *                 If the tick list entry (*entry) doesn't appear in
 *                 the index, allocate a metadata file index entry (*ie_ptr),
 *                 and initialize it as follows:
 *
 *                     ie_ptr->hdf5_page_offset = entry->page
 *                     ie_ptr->length           = entry->size
 *                     ie_ptr->delayed_flush    = entry->delay_write_until
 *
 *                 and then update the new entry as per the existing entry
 *                 case described above.
 *
 *              2) Scan the internal representation of the metadata file
 *                 index for entries that do not appear in the tick list.
 *                 For each such entry (*ie_ptr), proceed as follows:
 *
 *                 1) If ie_ptr->clean, we are done -- proceed to the
 *                    next index entry that doesn't appear in the tick list.
 *
 *                 2) Test to see if the cognate entry appears in the page
 *                    buffer.  If it doesn't, it must have been flushed and
 *                    evicted in the past tick.  Set
 *
 *                        ie_ptr->clean = TRUE, and
 *
 *                        ie_ptr->tick_of_last_flush = current tick
 *
 *                    and proceed to the next index entry that doesn't
 *                    appear in the tick list.
 *
 *                 3) If the cognate entry does appear in the page buffer
 *                    and is clean, proceed as per 2) above.
 *
 *                 4) In all other cases, do nothing, and proceed to the
 *                    next index entry that does not appear in the tick list.
 *
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 11/9/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_vfd_swmr__update_index(H5F_t *f, uint32_t *idx_ent_added_ptr, uint32_t *idx_ent_modified_ptr,
                            uint32_t *idx_ent_not_in_tl_ptr, uint32_t *idx_ent_not_in_tl_flushed_ptr)
{
    H5F_shared_t *const        shared   = f->shared;
    const uint64_t             tick_num = shared->tick_num;
    uint32_t                   i;
    uint32_t                   idx_ent_added             = 0;
    uint32_t                   idx_ent_modified          = 0;
    uint32_t                   idx_ent_not_in_tl         = 0;
    uint32_t                   idx_ent_not_in_tl_flushed = 0;
    H5PB_t *                   pb_ptr                    = NULL;
    H5PB_entry_t *             entry;
    H5FD_vfd_swmr_idx_entry_t *ie_ptr    = NULL;
    H5FD_vfd_swmr_idx_entry_t *idx       = NULL;
    herr_t                     ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(shared->vfd_swmr);
    HDassert(shared->vfd_swmr_writer);

    idx = shared->mdf_idx;

    HDassert(idx);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->vfd_swmr_writer);

    HDassert(idx_ent_added_ptr);
    HDassert(idx_ent_modified_ptr);
    HDassert(idx_ent_not_in_tl_ptr);
    HDassert(idx_ent_not_in_tl_flushed_ptr);

    /* scan the tick list and insert or update metadata file index entries
     * as appropriate.
     */

    for (entry = pb_ptr->tl_head_ptr; entry != NULL; entry = entry->tl_next) {
        uint64_t target_page = entry->page;

        HDassert(entry->magic == H5PB__H5PB_ENTRY_T_MAGIC);

        /* see if the shadow index already contains an entry for *entry. */

        ie_ptr = vfd_swmr_pageno_to_mdf_idx_entry(idx, shared->mdf_idx_entries_used, target_page, FALSE);

        if (ie_ptr == NULL) { /* alloc new entry in the metadata file index*/
            uint32_t new_index_entry_index;

            new_index_entry_index = shared->mdf_idx_entries_used + idx_ent_added++;

            if (new_index_entry_index >= shared->mdf_idx_len &&
                (idx = vfd_swmr_enlarge_shadow_index(f)) == NULL) {
                HDfprintf(stderr, "\n\nmax mdf index len (%" PRIu32 ") exceeded.\n\n", shared->mdf_idx_len);
                HDfprintf(stderr, "tick = %" PRIu64 ".\n", tick_num);
                HDexit(EXIT_FAILURE);
            }

            ie_ptr = idx + new_index_entry_index;

            /* partial initialization of new entry -- rest done later */
            ie_ptr->hdf5_page_offset    = target_page;
            ie_ptr->md_file_page_offset = 0; /* undefined at this point */
            ie_ptr->chksum              = 0; /* undefined at this point */
            /* ie_ptr->entry_ptr            initialized below */
            /* ie_ptr->tick_of_last_change  initialized below */
            /* ie_ptr->clean                initialized below */
            /* ie_ptr->tick_of_last_flush   initialized below */
            ie_ptr->delayed_flush       = entry->delay_write_until;
            ie_ptr->moved_to_lower_file = FALSE;
            ie_ptr->garbage             = FALSE;
            ie_ptr->length              = (uint32_t)entry->size;
        }
        else {
            /* If entry->size changed, discard the too-small (too-big?)
             * shadow region and set the shadow-file page number to 0
             * so that H5F_update_vfd_swmr_metadata_file() will
             * allocate a new one.
             */
            if (ie_ptr->length != (uint32_t)entry->size) {
                int ret;

                ret = shadow_image_defer_free(shared, ie_ptr);
                HDassert(ret == 0);

                ie_ptr->md_file_page_offset = 0;
                ie_ptr->length              = (uint32_t)entry->size;
            }

            idx_ent_modified++;
        }

        ie_ptr->entry_ptr           = entry->image_ptr;
        ie_ptr->tick_of_last_change = tick_num;
        HDassert(entry->is_dirty);
        ie_ptr->clean              = FALSE;
        ie_ptr->tick_of_last_flush = 0;
    }

    /* scan the metadata file index for entries that don't appear in the
     * tick list.  If the index entry is dirty, and either doesn't appear
     * in the page buffer, or is clean in the page buffer, mark the index
     * entry clean and as having been flushed in the current tick.
     */
    for (i = 0; i < shared->mdf_idx_entries_used; i++) {

        HDassert(i == 0 || idx[i - 1].hdf5_page_offset < idx[i].hdf5_page_offset);

        ie_ptr = idx + i;

        if (ie_ptr->tick_of_last_change == tick_num)
            continue;

        idx_ent_not_in_tl++;

        if (ie_ptr->clean)
            continue;

        H5PB__SEARCH_INDEX(pb_ptr, ie_ptr->hdf5_page_offset, entry, FAIL);

        if (entry == NULL || !entry->is_dirty) {
            idx_ent_not_in_tl_flushed++;
            ie_ptr->clean              = TRUE;
            ie_ptr->tick_of_last_flush = tick_num;
        }
    }

    HDassert(idx_ent_modified + idx_ent_not_in_tl == shared->mdf_idx_entries_used);

    HDassert(idx_ent_modified + idx_ent_not_in_tl + idx_ent_added <= shared->mdf_idx_len);

    *idx_ent_added_ptr             = idx_ent_added;
    *idx_ent_modified_ptr          = idx_ent_modified;
    *idx_ent_not_in_tl_ptr         = idx_ent_not_in_tl;
    *idx_ent_not_in_tl_flushed_ptr = idx_ent_not_in_tl_flushed;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_vfd_swmr__update_index() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB_write
 *
 * Purpose:	Write data into the Page Buffer if practical, and to file
 *              otherwise.  Specifically:
 *
 *              1) If the page buffer is disabled, simply write to the
 *                 HDF5 file and return.
 *
 *              2) If the write is raw data, and the page buffer is
 *                 configured for metadata only (i.e. min_md_pages ==
 *                 max_pages), or if the page buffer is operating in
 *                 vfd_swmr mode, simply write to the HDF5 file and return.
 *
 *              3) If the write is raw data, and is of page size or
 *                 larger, write directly from the HDF5 file.
 *
 *                 It is possible that the write intersects one or more
 *                 pages in the page buffer -- test for this and update
 *                 any partially written pages, and evict any pages
 *                 that are completely overwritten.
 *
 *                 Note that no pages are inserted into the page buffer in
 *                 this case.
 *
 *              4) If the write is of raw data, and it is of size less
 *                 than the page size, write the page into the page
 *                 buffer, loading and inserting pages into the
 *                 page buffer as necessary
 *
 *              5) If the write is of metadata, and the page buffer is
 *                 configured for raw data only (i.e. min_rd_pages ==
 *                 max_pages), simply write to the HDF5 file and return.
 *
 *              The free space manager guarantees that allocations larger
 *              than one page will be page alligned, and that allocations
 *              of size less than or equal to page size will not cross page
 *              boundaries.  Further, unlike raw data, metadata is always
 *              written and read atomically.
 *
 *              In principle, this should make it easy to discriminate
 *              between small and multi-page metadata entries so that
 *              pages containing the former will be buffered and the
 *              latter be written directly to file.
 *
 *              Unfortunately, there is a fly in the ointment.
 *
 *              The fixed and extensible array on disk data
 *              structures allocate multiple metadata cache entries in
 *              a single block, and use this fact to make the addresses
 *              of all but the first entry in the block computable.  While
 *              this simplifies the fixed and extensible array on disk data
 *              structures, it complicates the metadata cache and the page
 *              buffer.
 *
 *              From the page buffer perspective, it breaks the invarient
 *              that metadata entries of less than page size don't cross
 *              page boundaries, and those of size greater than or equal
 *              to page size start on page boundaries -- which is important
 *              for VFD SWMR as it allows efficient management of multi-page
 *              metadata entries.
 *
 *              While it is tempting to repair the fixed and extensible
 *              array data structures so as to remove this irregularity,
 *              and remove the resulting complexity from both the metadata
 *              cache and the page buffer, this is a ticklish task, as there
 *              are already files in the wild that use the existing versions
 *              of these data structures.  Thus, due to resource constraints,
 *              we have to program around the issue for now.
 *
 *              Fortunately, for purposes of the page buffer, this is
 *              relatively easy -- when we encounter a metadata write
 *              that crosses one or more page boundaries, and is not
 *              both page aligned and an integral number of pages, we
 *              query the metadata cache to determine the type of the
 *              client whose data is being writtne.  If it is one of the
 *              mis-behaving types, we split it into two or three writes
 *              such that each write either doesn't cross page boundaries,
 *              or is page aligned and an integral number of pages.
 *
 *              This is done in this function, and is not reflected in
 *              the case analysis in the rest of this comment.
 *
 *              6) If the write is of metadata, the write is larger than
 *                 one page, and vfd_swmr_writer is FALSE, simply write
 *                 to the HDF5 file.  There is no need to check the
 *                 page buffer, as metadata is always read atomically,
 *                 and entries of this size are not buffered in the page
 *                 buffer.
 *
 *                 Observe that this write must be page aligned.  This
 *                 should be enforced by the free space manager, but
 *                 for now it is enforced by the above mentioned practice
 *                 of splitting writes from cache client that don't
 *                 allocate each entry separately.
 *
 *              7) If the write is of metadata, the write is larger than
 *                 one page, and vfd_swmr_writer is TRUE, the write must
 *                 buffered in the page buffer until the end of the tick.
 *
 *                 If it doesn't exist already, create a multi-page metadata
 *                 entry in the page buffer and copy the write into it.
 *                 Insert the new entry in the tick list if necessary.
 *
 *                 Test to see if the write of the multi-page metadata
 *                 entry must be delayed.  If so, place the entry in
 *                 the delayed write list.  Otherwise, the multi-page
 *                 metadata entry will be written to the HDF5 file and
 *                 evicted when the tick list is released at the of the
 *                 tick.
 *
 *
 *              8) If the write is of metadata, and the write is of size
 *                 less than or equal to the page size, write the data
 *                 into the page buffer, loading and inserting a page
 *                 if necessary.
 *
 *                 If, in addition, vfd_swmr_writer is TRUE, add the page
 *                 touched by the write to the tick list.
 *
 *              Observe that this function handles casses 1, 2, 5, and 6
 *              directly, calls H5PB_write_raw() for cases 3 & 4, and
 *              calls H5PB_read_meta() for cases 7, and 8.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/11/18
 *
 * Changes:     Updated to support splitting of metadata writes that
 *              are not page aligned and cross page boundaries into
 *              2 or 3 writes that are either page aligned or do not
 *              cross page boundaries.  Full details in the header
 *              comment above, that has been updated to document
 *              this change.
 *
 *              Also updated case 2 to bypass the page buffer for raw
 *              data writes in vfd swmr mode.
 *
 *                                      JRM -- 4/5/20
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_write(H5F_shared_t *shared, H5FD_mem_t type, haddr_t addr, size_t size, const void *buf)
{
    H5PB_t *pb_ptr;                /* Page buffer for this file */
    hbool_t bypass_pb   = FALSE;   /* Whether to bypass page buffering */
    hbool_t split_write = FALSE;   /* whether md write must be split */
    herr_t  ret_value   = SUCCEED; /* Return value */

    /* the following six fields are defined iff split_write is TRUE */
    haddr_t prefix_addr = HADDR_UNDEF; /* addr of prefix -- if defined */
    haddr_t body_addr   = HADDR_UNDEF; /* addr of body -- if defined */
    haddr_t suffix_addr = HADDR_UNDEF; /* addr of suffix -- if defined */
    size_t  prefix_size = 0;           /* size of prefix */
    size_t  body_size   = 0;           /* size of body */
    size_t  suffix_size = 0;           /* size of suffix */

    FUNC_ENTER_NOAPI(FAIL)

    pb_ptr = shared->pb_ptr;

    if (pb_ptr != NULL && type != H5FD_MEM_DRAW)
        H5PB_count_meta_access_by_size(pb_ptr, size);

    if (pb_ptr == NULL) {

        bypass_pb = TRUE; /* case 1) -- page buffer is disabled */
    }
    else {

        HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);

        if (H5FD_MEM_DRAW == type) { /* raw data write */

            if ((pb_ptr->min_md_pages == pb_ptr->max_pages) || (pb_ptr->vfd_swmr)) {

                /* case 2) -- page buffer configured for metadata only */
                bypass_pb = TRUE;
            }
        }
        else { /* metadata write */

            if (pb_ptr->min_rd_pages == pb_ptr->max_pages) {

                /* case 5) -- page buffer configured for raw data only */
                bypass_pb = TRUE;
            }
            else {

                /* determine whether the write request must be split,
                 * and if so, compute the start points and sizes of
                 * of the sections.
                 *
                 * Note: The following code is almost identical to the
                 *       similar code in H5PB_read().  Thus, on the surface,
                 *       it is an obvious candidate for refactoring into a
                 *       function or macro.
                 *
                 *       However, there are subtle differences between
                 *       the two pieces of code which are driven by the
                 *       possibility of speculative reads.
                 *
                 *       More to the point, further changes may be necessary.
                 *       Thus we should wait on refactoring until this code has
                 *       been in daily use for some time, and it is clear
                 *       that further changes are unlikely.
                 */
                int      mdc_client_id = -1; /* id of mdc client, or -1 if undef */
                uint64_t start_page;         /* page index of first page in read */
                uint64_t second_page;        /* page index of second page in read */
                uint64_t end_page;           /* page index of last page in read */
                uint64_t body_page;          /* page index of start of body */
                haddr_t  start_page_addr;    /* addr of first page in read */
                haddr_t  second_page_addr;   /* addr of second page in read */
                haddr_t  end_page_addr;      /* addr of last page in read */
                haddr_t  end_addr;           /* addr of last byte in read */

                /* Calculate the aligned address of the first page */
                start_page      = (addr / pb_ptr->page_size);
                start_page_addr = start_page * pb_ptr->page_size;

                /* Calculate the aligned address of the last page */
                end_addr      = addr + (haddr_t)(size - 1);
                end_page      = end_addr / (haddr_t)(pb_ptr->page_size);
                end_page_addr = end_page * pb_ptr->page_size;

                HDassert(start_page_addr <= addr);
                HDassert(addr < start_page_addr + (haddr_t)(pb_ptr->page_size));

                HDassert(start_page <= end_page);
                HDassert(end_page_addr <= ((addr + (haddr_t)size - 1)));
                HDassert((addr + (haddr_t)size - 1) < (end_page_addr + pb_ptr->page_size));

                /* test to see if the write crosses a page boundary, and
                 * does not start on a page boundary, and is not of an
                 * integral number of pages.
                 */
                if ((start_page < end_page) &&
                    (!((addr == start_page_addr) &&
                       (end_page_addr + (haddr_t)(pb_ptr->page_size) == end_addr + 1)))) {

                    /* the read crosses a page boundary and is not
                     * page aligned and of length some multiple of page size.
                     *
                     * Test to see if the read is for a metadata entry that
                     * is sub-allocated from a larger space allocation.
                     *
                     * Note that the following test may have to be
                     * adjusted.
                     */
                    mdc_client_id = H5C_get_curr_io_client_type(shared->cache);

                    if ((mdc_client_id == (int)H5AC_EARRAY_DBLK_PAGE_ID) ||
                        (mdc_client_id == (int)H5AC_FARRAY_DBLK_PAGE_ID)) {

                        split_write = TRUE;
                    }
                    else {

                        HDassert(addr == start_page_addr);
                        HDassert(size > pb_ptr->page_size);

                        if (!pb_ptr->vfd_swmr_writer) {

                            /* case 6) -- multi-page entry with fixed /
                             * extensible array filtered out, and no
                             * no VFD swmr.
                             */
                            bypass_pb = TRUE;
                        }
                    }
                }
                else if ((size > pb_ptr->page_size) && (!pb_ptr->vfd_swmr_writer)) {

                    /* write is larger than page size and we are not
                     * in VFD SWMR mode -- bypass the page buffer.
                     * This is also case 6.  We catch it here as
                     * the code to determine whether to split only
                     * looks at I/O requests that cross page bundaries
                     * and are not both page aligned and an integral
                     * number of pages in length.
                     */
                    HDassert(start_page_addr == addr);
                    bypass_pb = TRUE;
                }

                if (split_write) {

                    /* compute the base addresses and length of the prefix,
                     * body, and suffix of the write, where these terms are
                     * defined as follows:
                     *
                     * prefix: All bytes from addr to the first page address
                     *         at or after addr.  If addr == start_page_addr,
                     *         the prefix is empty.
                     *
                     * body:   All bytes from the first page address covered
                     *         by the write up to but not including the last
                     *         page address in the write.  Note that the
                     *         length of the body must be a multiple of the
                     *         page size.  If only one page address is
                     *         included in the write, the body is empty.
                     *
                     * suffix: All bytes from the last page address in the
                     *         write until the end of the write.  If the
                     *         write ends on a page boundary, the suffix is
                     *         empty.
                     *
                     * Since we know that the write crosses at least one
                     * page boundary, and we have aleady filtered out the
                     * body only case, at least two of the above must be
                     * non-empty.
                     */

                    second_page      = start_page + 1;
                    second_page_addr = (haddr_t)(second_page * pb_ptr->page_size);

                    if (addr > start_page_addr) { /* prefix exists */

                        prefix_addr = addr;
                        prefix_size = (size_t)(second_page_addr - addr);

                        HDassert(prefix_addr > start_page_addr);
                        HDassert(prefix_size < pb_ptr->page_size);
                        HDassert(((size_t)(addr - start_page_addr) + prefix_size) == pb_ptr->page_size);
                    }

                    if (size - prefix_size >= pb_ptr->page_size) {

                        /* body exists */

                        if (addr == start_page_addr) {

                            body_page = start_page;
                            body_addr = start_page_addr;
                        }
                        else {

                            body_page = second_page;
                            body_addr = second_page_addr;
                        }

                        if (end_addr < end_page_addr + (haddr_t)(pb_ptr->page_size - 1)) {

                            /* suffix exists */
                            body_size = (size_t)(end_page - body_page) * pb_ptr->page_size;
                        }
                        else {

                            /* suffix is empty */
                            body_size = (size_t)(end_page - body_page + 1) * pb_ptr->page_size;
                        }

                        HDassert((body_page == start_page) || (body_page == start_page + 1));

                        HDassert(body_addr == (haddr_t)(body_page * pb_ptr->page_size));

                        HDassert(body_size < size);
                        HDassert(body_size >= pb_ptr->page_size);

                        HDassert(body_addr == addr + (haddr_t)prefix_size);
                        HDassert((body_addr + (haddr_t)body_size) <= (end_addr + 1));
                    }

                    if (end_addr < end_page_addr + (haddr_t)(pb_ptr->page_size - 1)) {

                        suffix_addr = end_page_addr;
                        suffix_size = (end_addr + 1) - end_page_addr;

                        HDassert(suffix_addr == addr + (haddr_t)(prefix_size + body_size));
                    }

                    HDassert(size == prefix_size + body_size + suffix_size);
                }
            }
        }
    }

#ifdef H5_HAVE_PARALLEL
    /* at present, the page buffer must be disabled in the parallel case.
     * However, just in case ...
     */
    if (H5F_SHARED_HAS_FEATURE(shared, H5FD_FEAT_HAS_MPI)) {

        bypass_pb = TRUE;

    }  /* end if */
#endif /* H5_HAVE_PARALLEL */

    if (bypass_pb) { /* cases 1, 2. 5, and 6 */

        if (H5FD_write(shared->lf, type, addr, size, buf) < 0)

            HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "write through lower VFD failed")

        /* Update statistics */
        if (pb_ptr) {

            H5PB__UPDATE_STATS_FOR_BYPASS(pb_ptr, type, size);
        }
    }
    else {

        if (H5FD_MEM_DRAW == type) { /* cases 3 and 4 */

            if (H5PB__write_raw(shared, type, addr, size, buf) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "H5PB_read_raw() failed")
        }
        else if (split_write) {

            /* handle the sub-allocated entry case */

            /* write prefix if it exists */
            if (prefix_size > 0) {

                if (H5PB__write_meta(shared, type, addr, prefix_size, buf) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "H5PB__write_meta() failed on prefix")
            }

            /* write the body if it exists */
            if (body_size > 0) {

                /* The "body_size == pb_ptr->page_size" clause in the
                 * following if is required since in normal operating
                 * mode, the page buffer buffers metadata I/O
                 * requests of page size or less.
                 *
                 * Thus this clause ensures that a single page body
                 * does not bypass the page buffer, setting the potential
                 * for an older version to shadow the most recent version.
                 *
                 * Note: The page buffer really shouldn't buffer page
                 *       aligned single page metadata I/O requests, as it
                 *       creates extra overhead to no purpose.  However,
                 *       fixing this is a bit tricky, and the case doesn't
                 *       appear to be common.  Thus, while it should be
                 *       fixed,  I don't think it is urgent.
                 *
                 *                           JRM 4/19/20
                 */
                if ((pb_ptr->vfd_swmr) || (body_size == pb_ptr->page_size)) {

                    if (H5PB__write_meta(shared, type, body_addr, body_size,
                                         (const void *)((const uint8_t *)buf + prefix_size)) < 0)

                        HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "H5PB__write_meta() failed on body")
                }
                else {

                    if (H5FD_write(shared->lf, type, body_addr, body_size,
                                   (const void *)((const uint8_t *)buf + prefix_size)) < 0)

                        HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "write through of body failed")

                    H5PB__UPDATE_STATS_FOR_BYPASS(pb_ptr, type, size);
                }
            }

            /* write the suffix if it exists */
            if (suffix_size > 0) {

                if (H5PB__write_meta(shared, type, suffix_addr, suffix_size,
                                     (const void *)((const uint8_t *)buf + prefix_size + body_size)) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "H5PB_write_meta() failed on suffix")
            }

            H5PB__UPDATE_STATS_FOR_WRITE_SPLIT(pb_ptr)
        }
        else { /* cases 7, and 8 */

            if (H5PB__write_meta(shared, type, addr, size, buf) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "H5PB_write_meta() failed")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5PB_write() */

/**************************************************************************/
/***************************** STATIC FUNCTIONS ***************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__allocate_page
 *
 * Purpose:	Allocate an instance of H5PB_entry_t and its associated
 *              buffer.  The supplied size must be greater than or
 *              equal to pb_ptr->page_size, and equal to that value if
 *              pb_ptr->vfd_swmr_writer is FALSE.
 *
 *              The associated buffer is zeroed if clean_image is TRUE.
 *
 * Return:	Pointer to the newly allocated instance of H5PB_entry_t
 *              on success, and NULL on failure.
 *
 * Programmer:	John Mainzer -- 10/12/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static H5PB_entry_t *
H5PB__allocate_page(H5PB_t *pb_ptr, size_t size, hbool_t clean_image)
{
    H5PB_entry_t *entry_ptr = NULL;
    void *        image_ptr = NULL;
    H5PB_entry_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* sanity checks */
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(size >= pb_ptr->page_size);
    HDassert((size == pb_ptr->page_size) || (pb_ptr->vfd_swmr_writer));

    /* allocate the entry and its associated image buffer */
    if (NULL == (entry_ptr = H5FL_MALLOC(H5PB_entry_t)))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, NULL, "memory allocation for H5PB_entry_t failed")

    if (clean_image) {

        image_ptr = H5MM_calloc(size);
    }
    else {

        image_ptr = H5MM_malloc(size);
    }

    if (NULL == image_ptr)

        HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, NULL, "memory allocation for page image failed")

    /* initialize the new page buffer entry */
    entry_ptr->magic       = H5PB__H5PB_ENTRY_T_MAGIC;
    entry_ptr->pb_ptr      = pb_ptr;
    entry_ptr->addr        = HADDR_UNDEF;
    entry_ptr->page        = 0;
    entry_ptr->size        = size;
    entry_ptr->image_ptr   = image_ptr;
    entry_ptr->mem_type    = H5FD_MEM_DEFAULT;
    entry_ptr->is_metadata = FALSE;
    entry_ptr->is_mpmde    = FALSE;
    entry_ptr->is_dirty    = FALSE;

    /* fields supporting the hash table */
    entry_ptr->ht_prev = NULL;
    entry_ptr->ht_next = NULL;
    entry_ptr->il_prev = NULL;
    entry_ptr->il_next = NULL;

    /* fields supporting replacement policise */
    entry_ptr->next = NULL;
    entry_ptr->prev = NULL;

    /* fields supporting VFD SWMR */
    entry_ptr->is_mpmde           = FALSE;
    entry_ptr->loaded             = FALSE;
    entry_ptr->modified_this_tick = FALSE;
    entry_ptr->delay_write_until  = 0;
    entry_ptr->tl_next            = NULL;
    entry_ptr->tl_prev            = NULL;

    ret_value = entry_ptr;

done:

    if (NULL == ret_value) {

        if (entry_ptr) {

            entry_ptr->magic = 0;
            entry_ptr        = H5FL_FREE(H5PB_entry_t, entry_ptr);
        }

        if (image_ptr) {

            image_ptr = H5MM_xfree(image_ptr);
        }
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB__allocate_page() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__create_new_page
 *
 * Purpose:	Create a new page and insert it in the page buffer with
 *              the specified address and type.  If entry_ptr_ptr is not
 *              NULL, return a pointer to the new entry in *entry_ptr_ptr.
 *
 *              Throw an error if a page already exists at the specified
 *              address.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/12/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB__create_new_page(H5PB_t *pb_ptr, haddr_t addr, size_t size, H5FD_mem_t type, hbool_t clean_image,
                      H5PB_entry_t **entry_ptr_ptr)
{
    hbool_t       inserted_in_index = FALSE;
    hbool_t       inserted_in_lru   = FALSE;
    uint64_t      page;
    H5PB_entry_t *entry_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);

    page = (uint64_t)addr / (uint64_t)(pb_ptr->page_size);

    HDassert((uint64_t)(addr) == (page * (uint64_t)(pb_ptr->page_size)));

    HDassert(size >= pb_ptr->page_size);
    HDassert((size == pb_ptr->page_size) || ((pb_ptr->vfd_swmr_writer) && (type != H5FD_MEM_DRAW)));
    HDassert((NULL == entry_ptr_ptr) || (NULL == *entry_ptr_ptr));

    H5PB__SEARCH_INDEX(pb_ptr, page, entry_ptr, FAIL);

    if (entry_ptr != NULL) {

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL,
                    "page buffer already contains a page at the specified address")
    }

    entry_ptr = H5PB__allocate_page(pb_ptr, size, clean_image);

    if (NULL == entry_ptr)
        HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, FAIL, "Can't allocate new page buffer entry")

    /* perform additional initialization */
    HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
    HDassert(entry_ptr->pb_ptr == pb_ptr);
    entry_ptr->addr = addr;
    entry_ptr->page = page;
    HDassert(entry_ptr->size == size);
    HDassert(entry_ptr->image_ptr);
    entry_ptr->mem_type    = type;
    entry_ptr->is_metadata = (type != H5FD_MEM_DRAW);
    entry_ptr->is_mpmde    = ((entry_ptr->is_metadata) && (size > pb_ptr->page_size));
    entry_ptr->is_dirty    = FALSE;

    /* insert in the hash table */
    H5PB__INSERT_IN_INDEX(pb_ptr, entry_ptr, FAIL)
    inserted_in_index = TRUE;

    /* insert at the head of the LRU if it isn't a multi-page metadata entry */
    if (!entry_ptr->is_mpmde) {

        H5PB__UPDATE_RP_FOR_INSERTION(pb_ptr, entry_ptr, FAIL)
        inserted_in_lru = TRUE;
    }

    /* updates stats */
    H5PB__UPDATE_STATS_FOR_INSERTION(pb_ptr, entry_ptr);

    if (entry_ptr_ptr) {

        *entry_ptr_ptr = entry_ptr;
    }

done:

    if (ret_value < 0) {

        if (entry_ptr) {

            if (inserted_in_lru) {

                H5PB__UPDATE_RP_FOR_EVICTION(pb_ptr, entry_ptr, FAIL);
            }

            if (inserted_in_index) {

                H5PB__DELETE_FROM_INDEX(pb_ptr, entry_ptr, FAIL)
            }

            H5PB__deallocate_page(entry_ptr);
            entry_ptr = NULL;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB_add_new_page */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__deallocate_page
 *
 * Purpose:	Free the supplied instance of H5PB_entry_t and its
 *              associated buffer.  The entry must be clean and removed
 *              from the page buffer before this function is called.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer -- 10/12/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static void
H5PB__deallocate_page(H5PB_entry_t *entry_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* sanity checks */
    HDassert(entry_ptr);
    HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
    HDassert(entry_ptr->size > 0);
    HDassert(entry_ptr->image_ptr);
    HDassert(!(entry_ptr->is_dirty));
    HDassert(entry_ptr->ht_next == NULL);
    HDassert(entry_ptr->ht_prev == NULL);
    HDassert(entry_ptr->il_next == NULL);
    HDassert(entry_ptr->il_prev == NULL);
    HDassert(entry_ptr->next == NULL);
    HDassert(entry_ptr->prev == NULL);
    HDassert(entry_ptr->tl_next == NULL);
    HDassert(entry_ptr->tl_prev == NULL);

    entry_ptr->magic     = 0;
    entry_ptr->image_ptr = H5MM_xfree(entry_ptr->image_ptr);
    entry_ptr            = H5FL_FREE(H5PB_entry_t, entry_ptr);

    FUNC_LEAVE_NOAPI_VOID

} /* H5PB__deallocate_page() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__evict_entry
 *
 * Purpose:	Evict the target entry from the from the page buffer, and
 *              de-allocate its associated image and instance of
 *              H5PB_entry_t.
 *
 *              In general, entries must be clean before they can be
 *              evicted, and the minimum metadata and raw data limits
 *              must be respected.  Attempts to evict an entry that
 *              that do not respect these constraints will generate
 *              and error unless the force parameter is TRUE, in which
 *              case, these constraints are ignored.
 *
 *              If `only_mark` is true, then the page-table entry's
 *              corresponding shadow-index entry is not removed.  Instead,
 *              it is marked as garbage.  This is a stop-gap fix for a
 *              performance problem in H5PB_dest(): deleting all of the
 *              index entries took time quadratic in their number.
 *
 *              In the context of VFD SWMR, there is also the requirement
 *              that entries to be evicted not be on the tick list, and
 *              also not reside on the delayed write list.  In the rare
 *              case in which such a page is discarded by the free space
 *              manager, it must be removed from the tick list and/or the
 *              delayed write list before being evicted by this function.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/14/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__evict_entry(H5F_shared_t *shared, H5PB_entry_t *entry_ptr, hbool_t force, hbool_t only_mark)
{
    H5PB_t *pb_ptr    = shared->pb_ptr;
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* sanity checks */
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(entry_ptr);
    HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
    HDassert(entry_ptr->size > 0);
    HDassert(entry_ptr->image_ptr);
    /* entries on either the tick list or the delayed write
     * list may not be evicted -- verify this.
     */
    HDassert(!(entry_ptr->modified_this_tick));
    HDassert(entry_ptr->delay_write_until == 0);

    if ((!force) && (entry_ptr->is_dirty))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "Attempt to evict a dirty entry");

    if (!force) {

        /* it is OK to evict an metadata page if pb_ptr->curr_md_pages ==
         * pb_ptr->min_md_pages - 1 if we are about to replace it with another
         * metadata page.
         *
         * Similarly, it is OK to evict an raw data page if
         * pb_ptr->curr_rd_pages == pb_ptr->min_rd_pages - 1 if we are
         * about to replace it with another raw data page.
         *
         * Assume sanity checks have been made before this call, and
         * allow the above without testing the intended replacement.
         */
        if ((entry_ptr->is_metadata) && (pb_ptr->curr_md_pages < pb_ptr->min_md_pages)) {

            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "Attempt to violate min_md_pages");
        }
        else if ((!entry_ptr->is_metadata) && (pb_ptr->curr_rd_pages < pb_ptr->min_rd_pages)) {

            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "Attempt to violate min_rd_pages");
        }
    }
    else if ((entry_ptr->is_dirty) && (H5PB__mark_entry_clean(pb_ptr, entry_ptr) < 0)) {

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry clean failed")
    }

    /* if the entry is in the replacement policy, remove it */
    if (!(entry_ptr->is_mpmde)) {

        H5PB__UPDATE_RP_FOR_EVICTION(pb_ptr, entry_ptr, FAIL)
    }

    /* remove the entry from the hash table */
    H5PB__DELETE_FROM_INDEX(pb_ptr, entry_ptr, FAIL)

    /* We need to remove the entry from the shadow file index in
     * the VFD SWMR case.
     *
     * If a multipage metadata entry is deallocated, and a new, single-page
     * metadata entry is allocated at the same base address, then
     * the old shadow index entry will still tell the size of the previous
     * image, which is greater than a page, and a shadow-file flush will
     * access bytes past the end of the entry's image.
     *
     * When we add code to allow entries
     * to age out of the metadata file index, that may provide
     * code that we can reuse to perform this invalidation.
     *
     * It's also possible (I think) for the index-entry size to be set
     * to one page, and then for a multipage entry to appear later at that
     * same index entry. The recorded size will still say the same, but
     * the image will be bigger.  So the shadow file will never see the
     * entire image written, just the first page of the image.
     */
    if (shared->vfd_swmr_writer && shadow_idx_entry_remove(shared, entry_ptr->page, only_mark) == -1) {
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "failed to remove shadow index entry")
    }

    /* update stats for eviction */
    H5PB__UPDATE_STATS_FOR_EVICTION(pb_ptr, entry_ptr)

    /* deallocate the page */
    H5PB__deallocate_page(entry_ptr);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB__evict_entry() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__flush_entry
 *
 * Purpose:	Flush the target entry to file.
 *
 *              Under normal circumstances, the entry will be in the
 *              replacement policy.  In this, also update the replacement
 *              policy for flush.
 *
 *              If pb_ptr->vfd_swmr_writer, it is possible that the target
 *              is a multi-page metadata entry.  In this case, the entry
 *              is not in the replacement policy, and thus the policy
 *              should not be updated.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/14/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__flush_entry(H5F_shared_t *shared, H5PB_t *pb_ptr, H5PB_entry_t *const entry_ptr)
{
    haddr_t eoa;                 /* Current EOA for the file */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* sanity checks */
    HDassert(shared);
    HDassert(shared->lf);
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(entry_ptr);
    HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
    HDassert(entry_ptr->size > 0);
    HDassert(entry_ptr->size >= pb_ptr->page_size);
    HDassert((entry_ptr->size == pb_ptr->page_size) || (entry_ptr->is_mpmde));
    HDassert(entry_ptr->image_ptr);
    HDassert(entry_ptr->is_dirty);
    HDassert((pb_ptr->vfd_swmr_writer) || (!(entry_ptr->is_mpmde)));
    HDassert(0 == entry_ptr->delay_write_until);

    /* Retrieve the 'eoa' for the file */
    if (HADDR_UNDEF == (eoa = H5FD_get_eoa(shared->lf, entry_ptr->mem_type)))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "driver get_eoa request failed")

    /* TODO: update the free space manager to inform the page buffer when
     *       space is de-allocated so that the following assertions will be
     *       true in all cases.
     */

    /* Verify that the base addresss of the page is within the EOA.  If it
     * isn't, the associated page has been discarded and should have been
     * removed from the page buffer.  This is a bug in the HDF5 library, so
     * an assertion is adequate here.
     */
    HDassert(eoa > entry_ptr->addr);

    /* Space at the end of the file should be allocate in increments of
     * pages.  Thus the entire page should be within the EOA.  Again,
     * an assertion is adequate here.
     */
    HDassert(eoa >= entry_ptr->addr + entry_ptr->size);

    /* flush the entry */
    if (H5FD_write(shared->lf, entry_ptr->mem_type, entry_ptr->addr, entry_ptr->size, entry_ptr->image_ptr) <
        0)

        HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "file write failed")

    /* mark the entry clean */
    if (H5PB__mark_entry_clean(pb_ptr, entry_ptr) < 0)

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry clean failed")

    /* if the entry is on the LRU, update the replacement policy */
    if (!entry_ptr->is_mpmde) {
        HDassert(entry_ptr->delay_write_until == 0);

        H5PB__UPDATE_RP_FOR_FLUSH(pb_ptr, entry_ptr, FAIL)
    }

    /* update stats for flush */
    H5PB__UPDATE_STATS_FOR_FLUSH(pb_ptr, entry_ptr)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB__flush_entry() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__load_page
 *
 * Purpose:	Load the page with the specified base address and insert
 *              it into the page buffer.  If necessary and possible, make
 *              space for the new page first.
 *
 *              Note that the size of the page is always pb_ptr->page_size,
 *              even in the VFD SWMR case, as in this context, multi-page
 *              metadata entries are always written in full, and they
 *              may only enter the page buffer as the result of a write.
 *
 *              In the context of VFD SWMR, when a page is loaded from
 *              file, it is possible that the VFD SWMR writer must delay
 *              writes to the page to avoid the possibility of message from
 *              the future bugs on the VFD SWMR reader.  For this reason,
 *              make note of the fact that the entry has been loaded from
 *              from file, so that the necessary checks can be made when
 *              writing to the page.
 *
 * Return:	SUCCEED if no errors are encountered, and
 *              FAIL otherwise.
 *
 * Programmer:	John Mainzer -- 10/18/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__load_page(H5F_shared_t *shared, H5PB_t *pb_ptr, haddr_t addr, H5FD_mem_t type,
                H5PB_entry_t **entry_ptr_ptr)
{
    hbool_t       skip_read = FALSE;
    haddr_t       eof       = HADDR_UNDEF;
    H5PB_entry_t *entry_ptr = NULL;
    void *        image_ptr = NULL;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* sanity checks */
    HDassert(shared);
    HDassert(shared->lf);
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert((entry_ptr_ptr == NULL) || (*entry_ptr_ptr == NULL));

#if 0  /* JRM */
    haddr_t eoa;
    /* Retrieve the 'eoa' for the file */
    if ( HADDR_UNDEF == (eoa = H5FD_get_eoa(shared->lf, type)))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, \
                    "driver get_eoa request failed")
    if ( addr + ((haddr_t)(pb_ptr->page_size)) > eoa )

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, \
                    "Attempt to load page that extends past EOA")
#endif /* JRM */
    if (HADDR_UNDEF == (eof = H5FD_get_eof(shared->lf, H5FD_MEM_DEFAULT)))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "driver get_eof request failed")

#if 0
    /* It is possible that this page been allocated but not
     * written.  Skip the read if addr > EOF.  In this case, tell 
     * H5PB__create_new_page() to zero the page image.
     *
     * Don't set "skip_read = (addr >= eof);" when accumulator is used.
     */
    skip_read = (addr >= eof);
#endif

    /* make space in the page buffer if necessary */
    if ((pb_ptr->curr_pages >= pb_ptr->max_pages) && (H5PB__make_space(shared, pb_ptr, type) < 0))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "H5PB__make_space() reports an error")

    /* Create a new page buffer page and insert it into the page buffer */
    if (H5PB__create_new_page(pb_ptr, addr, (size_t)(pb_ptr->page_size), type, skip_read, &entry_ptr) < 0)

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "can't create new page buffer page")

    HDassert(entry_ptr);
    HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
    HDassert(entry_ptr->addr == addr);

    image_ptr = entry_ptr->image_ptr;

    HDassert(image_ptr);

    /* Read the contents of the page from file, and store it in the
     * image buffer associated with the new entry.
     */
    if ((!skip_read) && (H5FD_read(shared->lf, type, addr, entry_ptr->size, image_ptr) < 0))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "driver read request failed")

    /* If in fact the page was read from file, make note of this fact
     * for purposes of VFD SWMR delayed writes in the VFD SWMR writer.
     */
    entry_ptr->loaded = !skip_read;

    H5PB__UPDATE_STATS_FOR_LOAD(pb_ptr, entry_ptr)

    if (entry_ptr_ptr) {

        *entry_ptr_ptr = entry_ptr;
    }

done:

    /* add cleanup in case of failure */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB__load_page() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__make_space
 *
 * Purpose:	Evict one or more pages from the page buffer so as to
 *              reduce the size of the page buffer to pb_ptr->max_pages - 1.
 *              if possible.
 *
 *              Note that the function must not be called under
 *              nonsensical conditions -- thus if either
 *
 *                  1) the inserted type is metadata and min_rd_pages ==
 *                     max_pages, or
 *
 *                  2) the inserted type is raw data and min_md_pages ==
 *                     max_pages
 *
 *              holds, the function has been called in error, and an
 *              assertion failure is appropriate.
 *
 *              If the page buffer is below its maximum size, we are
 *              done, and the function simply returns.
 *
 *              Otherwise, scan upwards from the bottom of the LRU list,
 *              examining each entry in turn.
 *
 *              If the entry is dirty, flush it, move it to the top of the
 *              LRU, and continue with the scan.  Note in the VFD SWMR case,
 *              we do not have to concern ourselves with delayed writes in
 *              this context, as all entries which are subject to delayed
 *              writes must reside on the delayed write list, not the LRU list.
 *
 *              If the entry is:
 *
 *                 1) clean
 *
 *                 2) either:
 *
 *                    a) the target entry is metadata and
 *                       curr_md_pages > min_md_pages.
 *
 *                    b) the target entry is raw data and
 *                       curr_rd_pages > min_rd_pages.
 *
 *                    c) the target entry is metadata, the inserted_type
 *                       is metadata, and curr_md_pages == min_md_pages.
 *
 *                    d) the target entry is raw data, the inserted_type
 *                       is raw data, and curr_rd_pages == min_rd_pages.
 *
 *                 3) The entry is not on the tick list (which can only
 *                    happen if pb_ptr->vfd_swmr_writer is TRUE).
 *
 *              evict the entry and test to see if pb_ptr->curr_pages <
 *              pb_ptr->max_pages.  If it is, return.  Otherwise, continue
 *              the scan until either the above condidtion is fulfilled,
 *              or the head of the LRU is reach.
 *
 *              Under normal circumstances, it should always be possible
 *              to reduce the size of the page buffer below pb_ptr->max_pages.
 *              However, due to prohibition on evicting entries on the
 *              tick list, and either flushing or evicting entries on the
 *              delayed write list, this will not in general be the case
 *              if pb_ptr->vfd_swmr_writer is TRUE.  In this case, the
 *              page buffer may exceed its maximum size by an arbitrary
 *              amount.
 *
 *              If this situation occurs with any regularity, we will
 *              need a mechanism to avoid attempts to make space when
 *              it is not possible to do so.
 *
 * Return:	SUCCEED if no errors are encountered, and
 *              FAIL otherwise.
 *
 * Programmer:	John Mainzer -- 10/14/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__make_space(H5F_shared_t *shared, H5PB_t *pb_ptr, H5FD_mem_t inserted_type)
{
    hbool_t       inserting_md;
    H5PB_entry_t *search_ptr;
    H5PB_entry_t *flush_ptr;
    H5PB_entry_t *evict_ptr;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* sanity checks */
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->min_md_pages + pb_ptr->min_rd_pages <= pb_ptr->max_pages);

    inserting_md = (H5FD_MEM_DRAW != inserted_type);

    if ((inserting_md) && (pb_ptr->min_rd_pages == pb_ptr->max_pages))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL,
                    "can't make space for metadata -- pb config for raw data only")

    if ((!inserting_md) && (pb_ptr->min_md_pages == pb_ptr->max_pages))

        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL,
                    "can't make space for raw data -- pb config for metadata only")

    search_ptr = pb_ptr->LRU_tail_ptr;

    while ((search_ptr) && (pb_ptr->curr_pages >= pb_ptr->max_pages)) {

        HDassert(search_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);

        if (search_ptr->modified_this_tick) { /* entry is on tick list */

            search_ptr = search_ptr->prev;
            H5PB__UPDATE_STATS_FOR_LRU_TL_SKIP(pb_ptr);
        }
        else if ((inserting_md) && (!(search_ptr->is_metadata)) &&
                 (pb_ptr->curr_rd_pages <= pb_ptr->min_rd_pages)) {

            search_ptr = search_ptr->prev;
            H5PB__UPDATE_STATS_FOR_LRU_RD_SKIP(pb_ptr);
        }
        else if ((!inserting_md) && (search_ptr->is_metadata) &&
                 (pb_ptr->curr_md_pages <= pb_ptr->min_md_pages)) {

            search_ptr = search_ptr->prev;
            H5PB__UPDATE_STATS_FOR_LRU_MD_SKIP(pb_ptr);
        }
        else if (search_ptr->is_dirty) {

            /* One can make the argument that we should test for dirty
             * entries first, instead of skipping potentially dirty
             * entries in the above clauses.  However, I suspect that
             * this would result in excessive flushes.  Lets try it
             * this way for now.
             */

            flush_ptr = search_ptr;

            /* if the *search_ptr has a predecessor in the LRU,
             * set set search_ptr equal to search_ptr->prev.  Otherwise,
             * leave search_ptr unchanged, so that it can be examined
             * on the next pass through the while loop after it has been
             * flushed.
             */
            if (search_ptr->prev) {

                search_ptr = search_ptr->prev;
            }

            if (H5PB__flush_entry(shared, pb_ptr, flush_ptr) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "Can't flush entry")
        }
        else { /* evict the entry */

            evict_ptr  = search_ptr;
            search_ptr = search_ptr->prev;
            if (H5PB__evict_entry(shared, evict_ptr, FALSE, FALSE) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "Can't evict entry")
        }
    }

    HDassert((search_ptr == NULL) || (pb_ptr->curr_pages < pb_ptr->max_pages));

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB__make_space() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__mark_entry_clean
 *
 * Purpose:	Mark the target entry clean
 *
 *              This function is typically used when an entry has been
 *              completely overwritten and is about to be evicted.  In
 *              this case, the entry must be marked clean to avoid
 *              sanity check failures on evictions.
 *
 *              While this function does update the index for the
 *              entry clean, it does not update the replacement policy.
 *              If this is desired, it must be done by the caller.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/14/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__mark_entry_clean(H5PB_t *pb_ptr, H5PB_entry_t *entry_ptr)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* sanity checks */
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(entry_ptr);
    HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
    HDassert(entry_ptr->size > 0);
    HDassert(entry_ptr->size >= pb_ptr->page_size);
    HDassert((entry_ptr->size == pb_ptr->page_size) || (entry_ptr->is_mpmde));
    HDassert(entry_ptr->image_ptr);
    HDassert((pb_ptr->vfd_swmr_writer) || (!(entry_ptr->is_mpmde)));

    /* mark the entry clean */
    entry_ptr->is_dirty = FALSE;

    /* update the index for the entry clean */
    H5PB__UPDATE_INDEX_FOR_ENTRY_CLEAN(pb_ptr, entry_ptr)

    /* don't update the replacement policy -- this will be done by
     * the caller if desired.
     */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB__mark_entry_clean() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__mark_entry_dirty
 *
 * Purpose:	Mark the target entry as dirty.
 *
 *              If pb_ptr->vfd_swmr_writer is FALSE, the entry will be
 *              in the replacement policy.  In this, we simply mark the
 *              entry as dirty, and update the replacement policy for an
 *              access.
 *
 *              If pb_ptr->vfd_swmr_writer, it is possible that we must
 *              delay writes to the target page or multi-page metadata
 *              entry to avoid message from the future bugs on the VFD
 *              SWMR readers.  In such cases we must set the
 *              delay_write_until field and insert the entry on the
 *              delayed write list instead of the replacement policy.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/14/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__mark_entry_dirty(H5F_shared_t *shared, H5PB_t *pb_ptr, H5PB_entry_t *entry_ptr)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* sanity checks */
    HDassert(pb_ptr);
    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(entry_ptr);
    HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
    HDassert(entry_ptr->size > 0);
    HDassert(entry_ptr->size >= pb_ptr->page_size);
    HDassert((entry_ptr->size == pb_ptr->page_size) || (entry_ptr->is_mpmde));
    HDassert(entry_ptr->image_ptr);
    HDassert((pb_ptr->vfd_swmr_writer) || (!(entry_ptr->is_mpmde)));

    /* mark the entry dirty if necessary */
    if (!(entry_ptr->is_dirty)) {

        entry_ptr->is_dirty = TRUE;

        H5PB__UPDATE_INDEX_FOR_ENTRY_DIRTY(pb_ptr, entry_ptr)

        /* since the entry was clean, there can be no pending delayed write */
        HDassert(entry_ptr->delay_write_until == 0);

        if ((pb_ptr->vfd_swmr_writer) && (entry_ptr->loaded) && (entry_ptr->mem_type != H5FD_MEM_DRAW) &&
            (H5F_vfd_swmr_writer__delay_write(shared, entry_ptr->page, &(entry_ptr->delay_write_until)) < 0))

            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "get delayed write request failed")

        if (entry_ptr->delay_write_until > 0) {

            if (!(entry_ptr->is_mpmde)) {

                /* remove the entry from the replacement policy */

                H5PB__UPDATE_RP_FOR_REMOVE(pb_ptr, entry_ptr, FAIL)
            }

            H5PB__INSERT_IN_DWL(pb_ptr, entry_ptr, FAIL)
        }
        else if (!(entry_ptr->is_mpmde)) {

            H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)
        }
        else {

            /* the entry should be a multi-page metadata entry that
             * has been modified this tick.  Thus no action is required.
             */
            HDassert(entry_ptr->is_mpmde);
            HDassert(pb_ptr->vfd_swmr_writer);
        }
    }
    else if ((!(entry_ptr->is_mpmde)) && (entry_ptr->delay_write_until == 0)) {

        /* the entry is dirty and on the replacement policy -- just update
         * the replacement policy for an access
         */
        H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5PB__mark_entry_dirty() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__read_meta
 *
 * Purpose:	Satisfy a metadata read in cases 7, 8, 9, and 10)
 *              H5PB_read().  Specifically:
 *
 *              6) If the read is for metadata and not page aligned, clip
 *                 the read to the end of the current page if necessary.
 *                 Load the relevant page if necessary and satisfy the
 *                 read from the page buffer.  Note that it there is an
 *                 existing page, it must not be a multi-page metadata
 *                 entry.  It it is, flag an error.
 *
 *                 Recall that by the time we get to this function,
 *                 un-aligned page reads from the fixed and variable
 *                 length array structures that cross page boundaries
 *                 have already been split into two or three reads
 *                 that conform to the usual pattern of metadata reads.
 *
 *              7) If the read is for metadata, is page aligned, is larger
 *                 than one page, and there is no entry in the page buffer,
 *                 satisfy the read from the file
 *
 *              8) If the read is for metadata, is page aligned, is larger
 *                 than one page, and there is a regular entry at the target
 *                 page address, test to see if the read is speculative.
 *
 *                 If it is not, evict the page, and satisfy the read from
 *                 file.  Flag an error if the page was dirty.
 *
 *                 If it is, clip the read to one page, and satisfy the
 *                 read from the existing regular entry.
 *
 *              9) If the read is for metadata, is page aligned, is larger
 *                 than one page, and there is a multi-page metadata entry
 *                 at the target page address, test to see if
 *                 pb_ptr->vfd_swmr_write is TRUE.
 *
 *                 If it is, satisfy the read from the multi-page metadata
 *                 entry, clipping the read if necessary.
 *
 *                 if pb_ptr->vfd_swmr_write is FALSE, flag an error.
 *
 *             10) If the read is for metadata, is page aligned, is no
 *                 larger than a page, test to see if the page buffer
 *                 contains a page at the target address.
 *
 *                 If it doesn't, load the page and satisfy the read
 *                 from it.
 *
 *                 If it contains a regular page entry, satisfy the read
 *                 from it.
 *
 *                 If it contains a multipage metadata entry at the target
 *                 address, satisfy the read from the multi-page metadata
 *                 entry if pb_ptr->vfd_swmr_write is TRUE, and flag an
 *                 error otherwise.
 *
 *              The above case analysis may be a bit hard to read.  If so,
 *              the table shown below may help to clarify.  Here:
 *
 *                P/A       == page aligned
 *                size > PL == size > page length
 *                Spec      == speculative read
 *                A         == current address
 *
 *              In the entry exists column:
 *
 *                N         == no entry
 *                R         == regular (1 page) entry
 *                MPMDE     == multi-page metadata entry
 *
 *       | size | entry  | VFD  |         |
 *  P/A: | > PL | exists | SWMR |  Spec   | Comments:
 * ------+------+--------+------+---------+-------------------------------------
 *   N   |  X   | N || R |  X   |    X    | Clip read to page boundary if
 *       |      |        |      |         | necessary
 *       |      |        |      |         | Load entry if necessary
 *       |      |        |      |         | Satisfy read from entry (case 6)
 * ------+------+--------+------+---------+-------------------------------------
 *   N   |  X   | MPMDE  |  X   |    X    | Error (case 6)
 * ------+------+--------+------+---------+-------------------------------------
 *       |      |        |      |         |
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  Y   |   N    |  X   |    X    | Satisfy read from file (case 7)
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  Y   |   R    |  X   |    Y    | Clip read to page boundary
 *       |      |        |      |         | Satisfy read from entry  (case 8)
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  Y   |   R    |  X   |    N    | Evict entry
 *       |      |        |      |         | (must be clean -- flag error if not)
 *       |      |        |      |         | Satisfy read from file (case 8)
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  Y   | MPMDE  |  N   |    X    | Error (case 9)
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  Y   | MPMDE  |  Y   |    X    | Clip read to MPE size if required.
 *       |      |        |      |         | Satify read from MPE (case 9)
 * ------+------+--------+------+---------+-------------------------------------
 *       |      |        |      |         |
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  N   |   N    |  X   |    X    | Load entry
 *       |      |        |      |         | Satisfy read from entry (case 10)
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  N   |   R    |  X   |    X    | Satisfy read from entry (case 10)
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  N   | MPMDE  |  Y   |    X    | Satisfy read from entry (case 10)
 * ------+------+--------+------+---------+-------------------------------------
 *   Y   |  N   | MPMDE  |  N   |    X    | Error (case 10)
 * ------+------+--------+------+---------+-------------------------------------
 *
 *              Observe that the above cases imply that:
 *
 *              1) The page buffer is defined.
 *
 *              2) The page buffer has been configured to accept at least
 *                 one page of metadata.
 *
 *              3) This is a metadata read.
 *
 *              Note also that if the metadata read is of size
 *              no larger than page size, it may not cross page
 *              boundaries.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/11/18
 *
 * Changes:     Updated to use the speculative read hint from the
 *              metadata cache, and remove the static variable
 *              containing the base address of the last read.
 *
 *                                             JRM -- 4/5/20
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__read_meta(H5F_shared_t *shared, H5FD_mem_t type, haddr_t addr, size_t size, void *buf /*out*/)
{
    hbool_t       bypass      = FALSE; /* flag indicating PB bypassed */
    hbool_t       speculative = FALSE; /* speculative read hint from mdc */
    H5PB_t *      pb_ptr;              /* Page buffer for this file */
    H5PB_entry_t *entry_ptr;           /* Pointer to page buffer entry */
    H5FD_t *      file;                /* File driver pointer */
    uint64_t      page;                /* page offset of addr */
    haddr_t       page_addr;           /* page containing addr */
    size_t        offset;              /* offset of read in page */
    size_t        clipped_size;        /* possibley clipped size */
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->pb_ptr);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->min_rd_pages < pb_ptr->max_pages);
    HDassert(shared->lf);

    file = shared->lf;

    HDassert(H5FD_MEM_DRAW != type);
    HDassert(buf);

    /* Calculate the aligned address of the first page */
    page      = (addr / pb_ptr->page_size);
    page_addr = page * pb_ptr->page_size;

    if (page_addr != addr) { /* case 6 */

        /* If the read is for metadata and not page aligned, clip
         * the read to the end of the current page if necessary.
         * Load the relevant page if necessary and satisfy the
         * read from the page buffer.  Note that it there is an
         * existing page, it must not be a multi-page metadata
         * entry.  It it is, flag an error.
         */

        offset = addr - page_addr;

        if ((offset + size) <= pb_ptr->page_size) {

            clipped_size = size;
        }
        else {

            clipped_size = size - ((offset + size) - pb_ptr->page_size);
        }

        HDassert(clipped_size > 0);
        HDassert(clipped_size <= size);
        HDassert((offset + clipped_size) <= pb_ptr->page_size);

        /* get the containing page */
        H5PB__SEARCH_INDEX(pb_ptr, page, entry_ptr, FAIL)

        /* update hit rate stats */
        H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, ((entry_ptr) != NULL), TRUE, FALSE)

        if ((NULL == entry_ptr) && (H5PB__load_page(shared, pb_ptr, page_addr, type, &entry_ptr) < 0))

            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "page buffer page load request failed (1)")

        HDassert(entry_ptr);
        HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
        HDassert(entry_ptr->addr == page_addr);
        HDassert(entry_ptr->is_metadata);
        HDassert(!(entry_ptr->is_mpmde));

        /* copy data from the page into read buffer */
        HDmemcpy((uint8_t *)buf, (uint8_t *)(entry_ptr->image_ptr) + offset, clipped_size);

        /* if the entry is on the LRU, update the replacement policy */
        if ((!(entry_ptr->is_mpmde)) && (entry_ptr->delay_write_until == 0)) {

            H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)
        }
    }
    else {

        HDassert(page_addr == addr);

        if (size > pb_ptr->page_size) {

            /* search the page buffer for an entry at page */
            H5PB__SEARCH_INDEX(pb_ptr, page, entry_ptr, FAIL)

            if (entry_ptr == NULL) { /* case 7 */

                /* update hit rate stats */
                H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, FALSE, TRUE, size > pb_ptr->page_size)

                /* If the read is for metadata, is page aligned, is larger
                 * than page size, and there is no entry in the page buffer,
                 * satisfy the read from the file
                 */
                if (H5FD_read(file, type, addr, size, buf) < 0)

                    HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "driver read request failed (1)")

                bypass = TRUE;

                H5PB__UPDATE_STATS_FOR_BYPASS(pb_ptr, type, size);
            }
            else {

                HDassert(entry_ptr);
                HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
                HDassert(entry_ptr->is_metadata);

                if (!(entry_ptr->is_mpmde)) { /* case 8 */

                    /* If the read is for metadata, is page aligned, is larger
                     * than one page, and there is a regular entry at the target
                     * page address, test to see if the read is speculative.
                     *
                     * If it is not, evict the page, and satisfy the read from
                     * file.  Flag an error if the page was dirty.
                     *
                     * If it is, clip the read to one page, and satisfy
                     * the read from the existing regular entry.
                     */

                    HDassert(entry_ptr->size == pb_ptr->page_size);

                    speculative = H5C_get_curr_read_speculative(shared->cache);

                    if (!speculative) {

                        /* since this is likely a second try, don't update
                         * hit rate stats.
                         */

                        HDassert(!(entry_ptr->is_dirty));

                        if (H5PB__evict_entry(shared, entry_ptr, TRUE, FALSE) < 0)

                            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "forced eviction failed (1)")
                        if (H5FD_read(file, type, addr, size, buf) < 0)

                            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "driver read request failed (2)")

                        bypass = TRUE;
                        H5PB__UPDATE_STATS_FOR_BYPASS(pb_ptr, type, size);
                    }
                    else {

                        HDassert(entry_ptr->image_ptr);

                        /* copy data from the page into read buffer */
                        HDmemcpy((uint8_t *)buf, (uint8_t *)(entry_ptr->image_ptr), entry_ptr->size);

                        /* if the entry is on the LRU, update the replacement
                         * policy
                         */
                        if ((!(entry_ptr->is_mpmde)) && (entry_ptr->delay_write_until == 0)) {

                            H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)
                        }

                        /* update hit rate stats */
                        H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, TRUE, TRUE, FALSE)
                    }
                }
                else { /* case 9 */

                    /* If the read is for metadata, is page aligned, is larger
                     * than one page, and there is a multi-page metadata entry
                     * at the target page address, test to see if
                     * pb_ptr->vfd_swmr_write is TRUE.
                     *
                     * If it is, satisfy the read from the multi-page metadata
                     * entry, clipping the read if necessary.
                     *
                     * if pb_ptr->vfd_swmr_write is FALSE, flag an error.
                     */
                    HDassert(entry_ptr->is_mpmde);
                    HDassert(pb_ptr->vfd_swmr_writer);

                    if (size > entry_ptr->size) {

                        clipped_size = entry_ptr->size;
                    }
                    else {

                        clipped_size = size;
                    }

                    /* copy data from the page into read buffer */
                    HDmemcpy((uint8_t *)buf, (uint8_t *)(entry_ptr->image_ptr), clipped_size);

                    /* if the entry is on the LRU, update the replacement
                     * policy
                     */
                    if ((!(entry_ptr->is_mpmde)) && (entry_ptr->delay_write_until == 0)) {

                        H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)
                    }

                    /* update hit rate stats */
                    H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, TRUE, TRUE, TRUE)
                }
            }
        }
        else { /* case 10 */

            /* If the read is for metadata, is page aligned, is no
             * larger than a page, test to see if the page buffer
             * contains a page at the target address.
             *
             * If it doesn't, load the page and satisfy the read
             * from it.
             *
             * If it contains a regular page entry, satisfy the read
             * from it.
             *
             * If it contains a multipage metadata entry at the target
             * address, satisfy the read from the multi-page metadata
             * entry if pb_ptr->vfd_swmr_write is TRUE, and flag an
             * error otherwise.
             */
            HDassert(size <= pb_ptr->page_size);

            /* get the containing page */
            H5PB__SEARCH_INDEX(pb_ptr, page, entry_ptr, FAIL)

            /* update hit rate stats */
            H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), TRUE, FALSE)

            if ((NULL == entry_ptr) && (H5PB__load_page(shared, pb_ptr, page_addr, type, &entry_ptr) < 0))

                HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "page buffer page load request failed (2)")

            HDassert(entry_ptr);
            HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
            HDassert(entry_ptr->is_metadata);
            HDassert((!(entry_ptr->is_mpmde)) || (pb_ptr->vfd_swmr_writer));

            /* copy data from the page into read buffer */
            HDmemcpy((uint8_t *)buf, (uint8_t *)(entry_ptr->image_ptr), size);

            /* if the entry is on the LRU, update the replacement policy */
            if ((!(entry_ptr->is_mpmde)) && (entry_ptr->delay_write_until == 0)) {

                H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)
            }
        }
    }

    if (!bypass)
        H5PB__UPDATE_STATS_FOR_ACCESS(pb_ptr, type, size);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5PB__read_meta() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__read_raw
 *
 * Purpose:	Satisfy a raw data read in cases 3 and 4 from H5PB_read().
 *              Specifically:
 *
 *              3) If the read is for raw data, and it is larger than the
 *                 page size, read it directly from the HDF5 file.
 *
 *                 It is possible that the page buffer contains dirty pages
 *                 that intersect with the read -- test for this and update
 *                 the read buffer from the page buffer if any such pages
 *                 exist.
 *
 *                 Note that no pages are inserted into the page buffer in
 *                 this case.
 *
 *              4) If the read is for raw data, and it is of size less
 *                 than or equal to the page size, satisfy the read from
 *                 the page buffer, loading and inserting pages into the
 *                 page buffer as necessary
 *
 *              Observe that this implies that:
 *
 *              1) The page buffer is defined.
 *
 *              2) The page buffer has been configured to accept at least
 *                 one page of raw data.
 *
 *              2) This is a raw data read.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/11/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__read_raw(H5F_shared_t *shared, H5FD_mem_t type, haddr_t addr, size_t size, void *buf /*out*/)
{
    H5PB_t *      pb_ptr;              /* Page buffer for this file */
    H5PB_entry_t *entry_ptr;           /* Pointer to page buffer entry */
    uint64_t      first_page;          /* page offset of first I/O */
    uint64_t      last_page;           /* page offset of last I/O */
    uint64_t      search_page;         /* page offset of current page */
    haddr_t       first_page_addr;     /* address of first page of I/O */
    haddr_t       last_page_addr;      /* address of last page of I/O */
    haddr_t       search_addr;         /* Address of current page */
    hsize_t       num_touched_pages;   /* Number of pages accessed */
    size_t        offset;              /* offset of read in page */
    size_t        length;              /* length of read in page */
    hsize_t       i;                   /* Local index variable */
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->pb_ptr);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->min_md_pages < pb_ptr->max_pages);
    HDassert(H5FD_MEM_DRAW == type);

    /* Calculate the aligned address of the first page */
    first_page      = (addr / pb_ptr->page_size);
    first_page_addr = first_page * pb_ptr->page_size;

    /* Calculate the aligned address of the last page */
    last_page      = ((addr + size - 1) / pb_ptr->page_size);
    last_page_addr = last_page * pb_ptr->page_size;

    /* Calculate number of pages that this read spans. */
    num_touched_pages = last_page - first_page + 1;

    if (first_page_addr == last_page_addr) {

        HDassert(1 == num_touched_pages);
        last_page_addr = HADDR_UNDEF;
    }

    /* case 3) raw data read of page size or greater. */
    if (size >= pb_ptr->page_size) {

        if (H5FD_read(shared->lf, type, addr, size, buf) < 0)

            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "read failed")

        H5PB__UPDATE_STATS_FOR_BYPASS(pb_ptr, type, size);

        /* For each page that intersects with the above read, check to see
         * if it exists in the page buffer, and if so, if it is dirty.
         *
         * If it does and is, update the read buffer with the contents
         * of the page so we get the up to date data into the buffer
         * after the big read from the file.
         */
        search_page = first_page;
        search_addr = first_page_addr;

        for (i = 0; i < num_touched_pages; i++) {

            H5PB__SEARCH_INDEX(pb_ptr, search_page, entry_ptr, FAIL)

            /* update hit rate stats */
            H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), FALSE, FALSE)

            if (entry_ptr) {

                HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
                HDassert(!(entry_ptr->is_metadata));
                HDassert(entry_ptr->page == search_page);
                HDassert(entry_ptr->addr == search_addr);
                HDassert(entry_ptr->size == pb_ptr->page_size);
                HDassert(entry_ptr->delay_write_until == 0);
                /* This page and [addr, addr + size) should NOT be disjoint. */
                HDassert(!(addr + size <= entry_ptr->addr || entry_ptr->addr + entry_ptr->size <= addr));

                if (entry_ptr->is_dirty) {

                    if (i == 0) {

                        /* handle the possible partial access of the
                         * first page.
                         */

                        HDassert(search_addr == first_page_addr);
                        HDassert(search_page == first_page);

                        offset = addr - first_page_addr;

                        HDassert(((offset == 0) && (search_addr == addr)) ||
                                 ((offset > 0) && (search_addr < addr)));

                        HDassert(pb_ptr->page_size >= offset);

                        HDassert(size >= pb_ptr->page_size - (size_t)offset);

                        HDmemcpy(buf, (uint8_t *)entry_ptr->image_ptr + offset,
                                 pb_ptr->page_size - (size_t)offset);
                    }
                    else if (i == num_touched_pages - 1) {

                        /* handle the possible partial access of the
                         * last page.
                         */
                        HDassert(i > 0);
                        HDassert(search_addr == last_page_addr);
                        HDassert(search_page == last_page);
                        HDassert(addr < last_page_addr);
                        HDassert(last_page_addr < addr + size);

                        offset = (num_touched_pages - 2) * pb_ptr->page_size +
                                 (pb_ptr->page_size - (addr - first_page_addr));

                        HDmemcpy((uint8_t *)buf + offset, entry_ptr->image_ptr,
                                 (size_t)((addr + size) - last_page_addr));
                    }
                    else {

                        /* this is an internal page -- copy it in its
                         * entireity.
                         */

                        offset = (i - 1) * pb_ptr->page_size + (pb_ptr->page_size - (addr - first_page_addr));

                        HDassert(addr + offset == search_addr);
                        HDassert(offset + pb_ptr->page_size <= size);

                        HDmemcpy((uint8_t *)buf + offset, entry_ptr->image_ptr, pb_ptr->page_size);
                    }

                    /* we have touched the entry -- move it to the top
                     * of the LRU if it resides there.
                     *
                     * The entry will be on the LRU if both it is not
                     * a multi-page metadata entry and it is not
                     * subject to a delayed write.
                     *
                     * As this is a raw data page buffer entry, both of
                     * these must be true, and are asserted above.
                     *
                     * Thus, just update the LRU.
                     */
                    H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)

                } /* if ( entry_ptr->is_dirty ) */
            }     /* if ( entry_ptr ) */

            search_page++;
            search_addr += pb_ptr->page_size;

        } /* end for */
    }
    else {
        /* case 4: Raw data read of size less than page size.
         *
         * In this case, read the desired data from the page buffer, loading
         * pages if necessary.
         */
        HDassert(size < pb_ptr->page_size);

        /* first page */
        offset = addr - first_page_addr;

        if ((offset + size) <= pb_ptr->page_size) {

            HDassert(num_touched_pages == 1);
            length = size;
        }
        else {

            HDassert(num_touched_pages == 2);
            length = size - (pb_ptr->page_size - offset);
        }

        /* get the first page */
        H5PB__SEARCH_INDEX(pb_ptr, first_page, entry_ptr, FAIL)

        /* update hit rate stats */
        H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), FALSE, FALSE)

        if ((NULL == entry_ptr) && (H5PB__load_page(shared, pb_ptr, first_page_addr, type, &entry_ptr) < 0))

            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "page buffer page load request failed (1)")

        HDassert(entry_ptr);
        HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
        HDassert(entry_ptr->addr == first_page_addr);

        /* copy data from first page into read buffer */
        HDmemcpy((uint8_t *)buf, ((uint8_t *)(entry_ptr->image_ptr) + offset), length);

        H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)

        /* second page, if it exists */
        if (num_touched_pages == 2) {

            offset = length;
            length = size - offset;

            HDassert(offset + length == size);

            /* get the second page */
            H5PB__SEARCH_INDEX(pb_ptr, last_page, entry_ptr, FAIL)

            /* update hit rate stats */
            H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), FALSE, FALSE)

            if ((NULL == entry_ptr) &&
                (H5PB__load_page(shared, pb_ptr, last_page_addr, type, &entry_ptr) < 0))

                HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "page buffer page load request failed (2)")

            HDassert(entry_ptr);
            HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
            HDassert(entry_ptr->addr == last_page_addr);
            HDassert(entry_ptr->page == last_page);

            /* copy data from second page into read buffer */
            HDmemcpy(((uint8_t *)(buf) + offset), (uint8_t *)(entry_ptr->image_ptr), length);

            H5PB__UPDATE_RP_FOR_ACCESS(pb_ptr, entry_ptr, FAIL)
        }
    } /* end else */

    H5PB__UPDATE_STATS_FOR_ACCESS(pb_ptr, type, size);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5PB__read_raw() */

/*-------------------------------------------------------------------------
 *
 * Function:	H5PB__write_meta
 *
 * Purpose:	Satisfy a metadata write in cases 7 and 8 from H5PB_write().
 *              Specifically:
 *
 *              7) If the write is of metadata, the write is larger than
 *                 one page, and vfd_swmr_writer is TRUE, the write must
 *                 buffered in the page buffer until the end of the tick.
 *
 *                 If it doesn't exist already, create a multi-page metadata
 *                 entry in the page buffer and copy the write into it.
 *                 Insert the new entry in the tick list if necessary.
 *
 *                 Test to see if the write of the multi-page metadata
 *                 entry must be delayed.  If so, place the entry in
 *                 the delayed write list.  Otherwise, the multi-page
 *                 metadata entry will be written to the HDF5 file and
 *                 evicted when the tick list is released at the of the
 *                 tick.
 *
 *              8) If the write is of metadata, and the write is of size
 *                 less than or equal to the page size, write the data
 *                 into the page buffer, loading and inserting a page
 *                 if necessary.
 *
 *                 If, in addition, vfd_swmr_writer is TRUE, we must:
 *
 *                  * add the page touched by the write to the tick list
 *                    so that it will be buffered until the end of the
 *                    tick.
 *
 *                  * test to see if the write must be delayed, and
 *                    add the page to the delayed write list if so.
 *
 *              Observe that this implies that:
 *
 *              1) The page buffer is defined.
 *
 *              2) The page buffer has been configured to accept at least
 *                 one page of metadata.
 *
 *              3) This is a metadata write.
 *
 *              Note also that if the metadata write is of size
 *              no larger than page size, it may not cross page
 *              boundaries.
 *
 *              Further, for writes larger than page size (case 7 only),
 *              the base address must be page aligned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/11/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__write_meta(H5F_shared_t *shared, H5FD_mem_t type, haddr_t addr, size_t size, const void *buf /*in*/)
{
    H5PB_t *      pb_ptr;              /* Page buffer for this file */
    H5PB_entry_t *entry_ptr;           /* Pointer to page buffer entry */
    uint64_t      page;                /* page offset of addr */
    haddr_t       page_addr;           /* page containg addr */
    size_t        offset;              /* offset of write in page */
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->pb_ptr);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->min_rd_pages < pb_ptr->max_pages);
    HDassert(H5FD_MEM_DRAW != type);
    HDassert(buf);

    /* Calculate the aligned address of the first page */
    page      = (addr / pb_ptr->page_size);
    page_addr = page * pb_ptr->page_size;

    /* if size > pb_ptr->page_size, addr must be page aligned */
    HDassert((size <= pb_ptr->page_size) || (addr == page_addr));

    H5PB__SEARCH_INDEX(pb_ptr, page, entry_ptr, FAIL)

    /* case 7) metadata write of size greater than page size. */
    if (size > pb_ptr->page_size) {

        offset = 0;

        /* The write must be for a multi-page metadata entry, and
         * we must be running as a VFD SWMR writer.
         *
         * This requires the following actions:
         *
         * 1) If the multi-page metadata entry is not already in the
         *    page buffer, create an entry for it.
         *
         * 2) Overwrite the image of the entry with the write buffer.
         *
         * 3) If the entry is not already on the tick list, add it to
         *    the tick list.
         *
         * 4) If the entry is not already on the delayed write list,
         *    test to see if it should be, and move it from the
         *    LRU to the delayed write list and set the delay_write_until
         *    field appropriately.
         *
         *    This is done via the call to H5PB__mark_entry_dirty()
         */
        HDassert(pb_ptr->vfd_swmr_writer);
        HDassert(addr == page_addr);

        /* If we're about to overwrite a single-page entry with multiple
         * pages, lengthen the entry.
         */
        if (entry_ptr != NULL && entry_ptr->size < size) {
            H5PB_entry_t *overlap;
            void *        new_image = H5MM_malloc(size);
            uint64_t      iter_page;
            uint64_t      last_page = page + roundup(size, pb_ptr->page_size) / pb_ptr->page_size;

            for (iter_page = page + 1; iter_page < last_page; iter_page++) {
                H5PB__SEARCH_INDEX(pb_ptr, iter_page, overlap, FAIL)
                HDassert(overlap == NULL);
            }
            if (new_image == NULL) {
                HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "couldn't extend entry");
            }
            H5PB__UPDATE_RP_FOR_REMOVE(pb_ptr, entry_ptr, FAIL)

            /* To keep statistics for the index and the tick-list up-to-date,
             * it's expedient to remove and re-insert entries there.
             */
            H5PB__DELETE_FROM_INDEX(pb_ptr, entry_ptr, FAIL)
            if (entry_ptr->modified_this_tick)
                H5PB__REMOVE_FROM_TL(pb_ptr, entry_ptr, FAIL)

            entry_ptr->image_ptr = H5MM_xfree(entry_ptr->image_ptr);
            entry_ptr->image_ptr = new_image;
            entry_ptr->is_mpmde  = TRUE;
            entry_ptr->size      = size;

            if (entry_ptr->modified_this_tick)
                H5PB__INSERT_IN_TL(pb_ptr, entry_ptr, FAIL)
            H5PB__INSERT_IN_INDEX(pb_ptr, entry_ptr, FAIL)
        }

        /* update hit rate stats */
        H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), TRUE, TRUE)

        if (NULL == entry_ptr) {

            /* the multi-page metadata entry is not currently in the page
             * buffer.  Create an entry for it, and insert it into the LRU.
             *
             * Don't bother to try to make space for it, as VFD SWMR
             * ignores the limits on page buffer size.
             */
            if (H5PB__create_new_page(pb_ptr, addr, size, type, FALSE, &entry_ptr) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "can't create new page buffer page")

            /* set entry_ptr->loaded to TRUE so as to trigger the
             * the delayed write test in H5PB__mark_entry_dirty().
             */
            entry_ptr->loaded = TRUE;
        }

        /* at this point, one way or the other, the multi-page metadata
         * entry must be in the page buffer.
         */
        HDassert(entry_ptr->is_mpmde);
        HDassert(size == entry_ptr->size);
        HDassert(type == entry_ptr->mem_type);
    }
    else {
        /* case 8) metadata write of size no larger than page size */

        offset = addr - page_addr;

        /* write cannot cross page boundaries. */
        HDassert((offset + size) <= pb_ptr->page_size);

        /* update hit rate stats */
        H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), TRUE, FALSE)

        if (NULL == entry_ptr && H5PB__load_page(shared, pb_ptr, page_addr, type, &entry_ptr) < 0) {
            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "page buffer page load request failed (1)")
        }

        HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
        HDassert(entry_ptr->addr == page_addr);
        HDassert(!(entry_ptr->is_mpmde));
        HDassert(entry_ptr->size == pb_ptr->page_size);
        HDassert(size <= entry_ptr->size);
    }

    HDassert(entry_ptr->is_metadata);

    /* copy data from the write buffer into the page image */
    HDmemcpy((uint8_t *)(entry_ptr->image_ptr) + offset, buf, size);

    if (H5PB__mark_entry_dirty(shared, pb_ptr, entry_ptr) < 0)
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry dirty failed")

    /* Force the page buffer to retain the page until the end of
     * the tick: add the entry to the tick list if it is not
     * already present.
     */
    if (pb_ptr->vfd_swmr_writer && !entry_ptr->modified_this_tick) {
        entry_ptr->modified_this_tick = TRUE;
        H5PB__INSERT_IN_TL(pb_ptr, entry_ptr, FAIL)
    }

    H5PB__UPDATE_STATS_FOR_ACCESS(pb_ptr, type, size);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5PB__write_meta() */

/*-------------------------------------------------------------------------
 *
 * Function:    H5PB__write_raw
 *
 * Purpose:     Satisfy a raw data write in cases 3 and 4 from H5PB_write().
 *              Specifically:
 *
 *              3) If the write is raw data, and it of page size or
 *                 larger, write directly to the HDF5 file.
 *
 *                 It is possible that the write intersects one or more
 *                 pages in the page buffer -- test for this and update
 *                 any partially written pages, and evict any pages
 *                 that are completely overwritten.
 *
 *                 Note that no pages are inserted into the page buffer in
 *                 this case.
 *
 *              4) If the write is of raw data, and it is of size less
 *                 than the page size, write the page into the page
 *                 buffer, loading and inserting pages into the
 *                 page buffer as necessary
 *
 *              Observe that this implies that:
 *
 *              1) The page buffer is defined.
 *
 *              2) The page buffer has been configured to accept at least
 *                 one page of raw data.
 *
 *              2) This is a raw data write.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer -- 10/11/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__write_raw(H5F_shared_t *shared, H5FD_mem_t type, haddr_t addr, size_t size, const void *buf /*out*/)
{
    H5PB_t *      pb_ptr;              /* Page buffer for this file */
    H5PB_entry_t *entry_ptr;           /* Pointer to page buffer entry */
    uint64_t      first_page;          /* page offset of first I/O */
    uint64_t      last_page;           /* page offset of last I/O */
    uint64_t      search_page;         /* page offset of current page */
    haddr_t       first_page_addr;     /* address of first page of I/O */
    haddr_t       last_page_addr;      /* address of last page of I/O */
    haddr_t       search_addr;         /* Address of current page */
    hsize_t       num_touched_pages;   /* Number of pages accessed */
    hsize_t       i;                   /* Local index variable */
    size_t        length;              /* length of write in a page */
    size_t        offset;              /* offset of write in a page */
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shared);
    HDassert(shared->pb_ptr);

    pb_ptr = shared->pb_ptr;

    HDassert(pb_ptr->magic == H5PB__H5PB_T_MAGIC);
    HDassert(pb_ptr->min_md_pages < pb_ptr->max_pages);
    HDassert(shared->lf);

    HDassert(H5FD_MEM_DRAW == type);

    /* Calculate the aligned address of the first page */
    first_page      = (addr / pb_ptr->page_size);
    first_page_addr = first_page * pb_ptr->page_size;

    /* Calculate the aligned address of the last page */
    last_page      = ((addr + size - 1) / pb_ptr->page_size);
    last_page_addr = last_page * pb_ptr->page_size;

    /* Calculate number of pages that this read spans. */
    num_touched_pages = last_page - first_page + 1;

    if (first_page_addr == last_page_addr) {

        HDassert(1 == num_touched_pages);
        last_page_addr = HADDR_UNDEF;
    }

    /* case 3) raw data write of page size or greater. */
    if (size >= pb_ptr->page_size) {
        if (H5FD_write(shared->lf, type, addr, size, buf) < 0)

            HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "write through metadata accumulator failed")

        H5PB__UPDATE_STATS_FOR_BYPASS(pb_ptr, type, size);

        /* For each page that intersects with the above write, check to see
         * if it exists in the page buffer.
         *
         * If it does and is, and if the write overwrites page fully,
         * mark the page clean and evict it.
         *
         * If the write only partially intersects a page, update the
         * page and mark it dirty.
         */
        search_page = first_page;
        search_addr = first_page_addr;

        for (i = 0; i < num_touched_pages; i++) {

            H5PB__SEARCH_INDEX(pb_ptr, search_page, entry_ptr, FAIL)

            /* update hit rate stats */
            H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), FALSE, FALSE)

            if (entry_ptr) {

                HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
                HDassert(!(entry_ptr->is_metadata));
                HDassert(entry_ptr->page == search_page);
                HDassert(entry_ptr->addr == search_addr);
                HDassert(entry_ptr->size == pb_ptr->page_size);
                HDassert(entry_ptr->delay_write_until == 0);
                HDassert(entry_ptr->addr <= addr + size);

                if ((addr <= entry_ptr->addr) && (entry_ptr->addr + entry_ptr->size <= addr + size)) {

                    /* the page is completely overwritten -- mark it clean
                     * and evict it.
                     */
                    if ((entry_ptr->is_dirty) && (H5PB__mark_entry_clean(pb_ptr, entry_ptr) < 0))

                        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry clean failed")

                    if (H5PB__evict_entry(shared, entry_ptr, TRUE, FALSE) < 0)

                        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "forced eviction failed (1)")
                }
                else if (i == 0) {

                    /* handle partial overwrite of the first page.  */

                    HDassert(search_addr == first_page_addr);
                    HDassert(search_page == first_page);
                    HDassert(search_addr < addr);
                    HDassert(entry_ptr->addr + entry_ptr->size <= addr + size);

                    offset = addr - first_page_addr;

                    HDassert(offset > 0);
                    HDassert(pb_ptr->page_size >= offset);
                    HDassert(size >= pb_ptr->page_size - (size_t)offset);

                    HDmemcpy((uint8_t *)entry_ptr->image_ptr + offset, buf,
                             pb_ptr->page_size - (size_t)offset);

                    if (H5PB__mark_entry_dirty(shared, pb_ptr, entry_ptr) < 0)

                        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry dirty failed (1)")
                }
                else if (i == num_touched_pages - 1) {

                    /* handle partial overwrite of the last page.  */
                    HDassert(i > 0);
                    HDassert(search_addr == last_page_addr);
                    HDassert(search_page == last_page);
                    HDassert(addr < last_page_addr);
                    HDassert(last_page_addr < addr + size);

                    offset = (num_touched_pages - 2) * pb_ptr->page_size +
                             (pb_ptr->page_size - (addr - first_page_addr));

                    HDmemcpy(entry_ptr->image_ptr, (const uint8_t *)buf + offset,
                             (size_t)((addr + size) - last_page_addr));

                    if (H5PB__mark_entry_dirty(shared, pb_ptr, entry_ptr) < 0)

                        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry dirty failed (2)")
                }
                else {

                    /* this should be un-reachable */
                    HDassert(FALSE);
                }
            } /* if ( entry_ptr ) */

            search_page++;
            search_addr += pb_ptr->page_size;

        } /* end for */
    }
    else {
        /* case 4: Raw data write of size less than page size.
         *
         * In this case, write the data to the page buffer, loading
         * pages if necessary.
         */
        HDassert(size < pb_ptr->page_size);

        /* first page */
        offset = addr - first_page_addr;

        if ((offset + size) <= pb_ptr->page_size) {

            HDassert(num_touched_pages == 1);
            length = size;
        }
        else {

            HDassert(num_touched_pages == 2);
            length = pb_ptr->page_size - offset;
            HDassert(offset + length == pb_ptr->page_size);
        }

        /* get the first page */
        H5PB__SEARCH_INDEX(pb_ptr, first_page, entry_ptr, FAIL)

        /* update hit rate stats */
        H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), FALSE, FALSE)

        if ((NULL == entry_ptr) && (H5PB__load_page(shared, pb_ptr, first_page_addr, type, &entry_ptr) < 0))

            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "page buffer page load request failed (1)")

        HDassert(entry_ptr);
        HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
        HDassert(entry_ptr->addr == first_page_addr);

        /* copy data from the write buffer into the first page */
        HDmemcpy(((uint8_t *)(entry_ptr->image_ptr)) + offset, (const uint8_t *)buf, length);

        if (H5PB__mark_entry_dirty(shared, pb_ptr, entry_ptr) < 0)

            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry dirty failed (3)")

        /* second page, if it exists */
        if (num_touched_pages == 2) {

            offset = length;
            length = size - offset;

            HDassert(offset + length == size);

            /* get the first page */
            H5PB__SEARCH_INDEX(pb_ptr, last_page, entry_ptr, FAIL)

            /* update hit rate stats */
            H5PB__UPDATE_PB_HIT_RATE_STATS(pb_ptr, (entry_ptr != NULL), FALSE, FALSE)

            if ((NULL == entry_ptr) &&
                (H5PB__load_page(shared, pb_ptr, last_page_addr, type, &entry_ptr) < 0))

                HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "page buffer page load request failed (2)")

            HDassert(entry_ptr);
            HDassert(entry_ptr->magic == H5PB__H5PB_ENTRY_T_MAGIC);
            HDassert(entry_ptr->addr == last_page_addr);
            HDassert(entry_ptr->page == last_page);

            /* copy data from the write buffer into the first page */
            HDmemcpy((uint8_t *)(entry_ptr->image_ptr), ((const uint8_t *)(buf) + offset), length);

            if (H5PB__mark_entry_dirty(shared, pb_ptr, entry_ptr) < 0)

                HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "mark entry dirty failed (3)")
        }
    }

    H5PB__UPDATE_STATS_FOR_ACCESS(pb_ptr, type, size);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5PB__write_raw() */
