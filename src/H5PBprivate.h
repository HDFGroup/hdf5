/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * File:        H5PBprivate.h
 *
 * Purpose:     This file contains declarations which are normally visible
 *              within the HDF5 library, but are not visible at the user
 *              level
 *
 * Programmer: John Mainzer -- 10/07/18
 */

#ifndef H5PBprivate_H
#define H5PBprivate_H

/* Private headers needed by this header */
#include "H5private.h" /* Generic Functions			*/

/**************************/
/* Library Private Macros */
/**************************/

#define H5PB__HASH_TABLE_LEN 4096 /* must be a power of 2 */

/****************************/
/* Library Private Typedefs */
/****************************/

/* Typedef for the page buffer entry structure (defined in H5PBpkg.h) */
typedef struct H5PB_entry_t H5PB_entry_t;

/******************************************************************************
 *
 * structure H5PB_t
 *
 * Catchall structure for all variables specific to an instance of the page
 * buffer.
 *
 * At present, the page buffer serves two purposes in the HDF5 library.
 *
 * Under normal operating conditions, it serves as a normal page buffer whose
 * purpose is to minimize and optimize file I/O by aggregating small metadata
 * and raw data writes into pages, and by caching frequently used pages.
 *
 * In addition, when a file is opened for VFD SWMR writing, the page buffer is
 * used to retain copies of all metadata pages and multi-page metadata entries
 * that are written in a given tick, and under certain cases, to delay metadata
 * page and/or multi-page metadata entry writes for some number of ticks.
 * If the entry has not appeared in the VFD SWMR index for at least max_lag
 * ticks, this is necessary to avoid message from the future bugs.  See the
 * VFD SWMR RFC for further details.
 *
 * To reflect this, the fields of this structure are divided into three
 * sections.  Specifically fields needed for general operations, fields needed
 * for VFD SWMR, and statistics.
 *
 * FIELDS FOR GENERAL OPERATIONS:
 *
 * magic:       Unsigned 32 bit integer that must always be set to
 *              H5PB__H5PB_T_MAGIC.  This field is used to validate pointers to
 *              instances of H5PB_t.
 *
 * page_size:   size_t containing the page buffer page size in bytes.
 *
 * max_pages:   64 bit integer containing the nominal maximum number
 *              of pages in the page buffer.  Note that on creation, the page
 *              buffer is empty, and that under certain circumstances (mostly
 *              related to VFD SWMR) this limit can be exceeded by large
 *              amounts.
 *
 * curr_pages:  64 bit integer containing the current number of pages
 *              in the page buffer.  curr_pages must always equal the sum of
 *              curr_md_pages + curr_rd_pages.
 *
 *              Note that in the context of VFD SWMR, this count does NOT
 *              include multi-page metadata entries.
 *
 * curr_md_pages: 64 bit integer containing the current number of
 *              metadata pages in the page buffer.
 *
 *              Note that in the context of VFD SWMR, this count does NOT
 *              include multi-page metadata entries.
 *
 * curr_rd_pages: 64 bit integer containing the current number of
 *              raw data pages in the page buffer.
 *
 * min_md_pages: 64 bit integer containing the number of pages in the
 *              page buffer reserved for metadata.  No metadata page may be
 *              evicted from the page buffer if curr_md_pages is less than or
 *              equal to this value.
 *
 * min_rd_pages: 64 bin integer containing the number of pages in the
 *              page buffer reserved for raw data.  No page or raw data may be
 *              evicted from the page buffer if curr_rd_pages is less than or
 *              equal to this value.
 *
 * The FAPL fields are used to store the page buffer configuration data
 * provided to the page buffer in the H5PB_create() call.
 *
 * max_size:    Maximum page buffer size supplied by the FAPL.
 *
 * min_meta_perc: Percent of the page buffer reserved for metadata as
 *              supplied in the FAPL.
 *
 * min_raw_perc: Percent of the page buffer reserved for metadata as
 *              supplied in the FAPL.
 *
 * The purpose of the index is to allow us to efficiently look up all pages
 * (and multi-page metadata entries in the context of VFD SWMR) in the
 * page buffer.
 *
 * This function is provided by a hash table with chaining, albeit with one
 * un-unusual feature.
 *
 * Specifically hash table size must be a power of two, and the hash function
 * simply clips the high order bits off the page offset of the entry.
 *
 * This should work, as space is typically allocated sequentually, and thus
 * via a reverse principle of locality argument, hot pages are unlikely to
 * hash to the same bucket.  That said, we must collect statistics to alert
 * us should this not be the case.
 *
 * We also maintain a linked list of all entries in the index to facilitate
 * flush operations.
 *
 * index        Array of pointer to H5PB_entry_t of size
 *              H5PB__HASH_TABLE_LEN.  This size must ba a power of 2,
 *              not the usual prime number.
 *
 * index_len:   Number of entries currently in the hash table used to index
 *              the page buffer.  index_len should always equal
 *              clean_index_len + dirty_index_len.
 *
 * clean_index_len: Number of clean entries currently in the hash table
 *              used to index the page buffer.
 *
 * dirty_index_len: Number of dirty entries currently in the hash table
 *              used to index the page buffer.
 *
 * index_size:  Number of bytes currently stored in the hash table used to
 *              index the page buffer.  Under normal circumstances, this
 *              value will be index_len * page size.  However, if
 *              vfd_swmr_writer is TRUE, it may be larger.
 *
 *              index_size should always equal clean_index_size +
 *              dirty_index_size.
 *
 * clean_index_size: Number of bytes of clean entries currently stored in
 *              the hash table used to index the page buffer.
 *
 * dirty_index_size: Number of bytes of dirty entries currently stored in
 *              the hash table used to index the page buffer.
 *
 * il_len:      Number of entries on the index list.
 *
 *              This must always be equal to index_len.  As such, this
 *              field is redundant.  However, the existing linked list
 *              management macros expect to maintain a length field, so
 *              this field exists primarily to avoid adding complexity to
 *              these macros.
 *
 * il_size:     Number of bytes of cache entries currently stored in the
 *              index list.
 *
 *              This must always be equal to index_size.  As such, this
 *              field is redundant.  However, the existing linked list
 *              management macros expect to maintain a size field, so
 *              this field exists primarily to avoid adding complexity to
 *              these macros.
 *
 * il_head:     Pointer to the head of the doubly linked list of entries in
 *              the index list.  Note that cache entries on this list are
 *              linked by their il_next and il_prev fields.
 *
 *              This field is NULL if the index is empty.
 *
 * il_tail:     Pointer to the tail of the doubly linked list of entries in
 *              the index list.  Note that cache entries on this list are
 *              linked by their il_next and il_prev fields.
 *
 *              This field is NULL if the index is empty.
 *
 *
 * Fields supporting the modified LRU policy:
 *
 * See most any OS text for a discussion of the LRU replacement policy.
 *
 * Under normal operating circumstances (i.e. vfd_swmr_writer is FALSE)
 * all entries will reside both in the index and in the LRU.  Further,
 * all entries will be of size page_size.
 *
 * The VFD SWMR writer case (i.e. vfd_swmr_writer is TRUE) is complicated
 * by the requirements that we:
 *
 * 1) buffer all metadat writes (including multi-page metadata writes) that
 *    occur during a tick, and
 *
 * 2) when necessary, delay metadata writes for up to max_lag ticks to
 *    avoid message from the future bugs on the VFD SWMR readers.
 *
 * See discussion of fields supporting VFD SWMR below for details.
 *
 * Discussions of the individual fields used by the modified LRU replacement
 * policy follow:
 *
 * LRU_len:     Number of page buffer entries currently on the LRU.
 *
 *              Observe that LRU_len + dwl_len must always equal
 *              index_len.
 *
 * LRU_size:    Number of bytes of page buffer entries currently residing
 *              on the LRU list.
 *
 *              Observe that LRU_size + dwl_size must always equal
 *              index_size.
 *
 * LRU_head_ptr:  Pointer to the head of the doubly linked LRU list.  Page
 *              buffer entries on this list are linked by their next and
 *              prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * LRU_tail_ptr:  Pointer to the tail of the doubly linked LRU list.  Page
 *              buffer entries on this list are linked by their next and
 *              prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 *
 * FIELDS SUPPORTING VFD SWMR:
 *
 * If the file is opened in VFD SWMR mode (i.e. vfd_swmr == TRUE), all
 * raw data I/O must be passed through to the HDF5 file
 *
 * If the file is opened as a VFD SWMR writer (i.e. vfd_swmr_writer == TRUE),
 * the page buffer must retain the data necessary to update the metadata
 * file at the end of each tick, and also delay writes as necessary so as
 * to avoid message from the future bugs on the VFD SWMR readers.
 *
 * The tick list exists to allow us to buffer copies of all metadata writes
 * during a tick, and the delayed write list supports delayed writes.
 *
 * If a regular page is written to during a tick, it is placed on the tick
 * list.  If there is no reason to delay its write to file (i.e. either
 * it was just allocated, or it has existed in the metadata file index for
 * at least max_lag ticks), it is also placed on the LRU, where it may be
 * flushed, but not evicted.  If its write must be delayed, it is placed on
 * the delayed write list, where it must remain until its write delay is
 * satisfied -- at which point it is moved to the LRU.
 *
 * If a multi-page metadata entry is written during a tick, it is placed on
 * the tick list.  If, in addition, the write of the entry must be delayed,
 * it is also place on the delayed write list.  Note that multi-page metadata
 * entries may never appear on the LRU.
 *
 * At the end of each tick, the tick list is emptied.
 *
 * Regular pages are simply removed from the tick list, as they must already
 * appear on either the LRU or the delayed write list.
 *
 * Multi-page metadata entries that are not also on the delayed write list
 * are simply flushed and evicted.
 *
 * The delayed write list is also scanned at the end of each tick.  Regular
 * entries that are now flushable are placed at the head of the LRU.  Multi-
 * page metadata entries that are flushable are flushed and evicted.
 *
 * The remainder of this sections contains discussions of the fields and
 * data structures used to support the above operations.
 *
 * vfd_swmr:    Boolean flag that is set to TRUE IFF the file is opened
 *              in VFD SWMR mode -- either reader or writer.  This field
 *              is used to exclude raw data from the page buffer.
 *
 * vfd_swmr_writer: Boolean flag that is set to TRUE iff the file is
 *              is opened in VFD SWMR writer mode.  The remaining
 *              VFD SWMR fields are defined iff vfd_swmr_writer is TRUE.
 *
 * mpmde_count: int64_t containing the number of multi-page metadata
 *              entries currently resident in the page buffer.  Observe
 *              that index_len should always equal curr_pages + mpmde_count.
 *
 * cur_tick:    uint64_t containing the current tick.  This is a copy of
 *              the same field in the associated instance of H5F_file_t,
 *              and is maintained as a convenience.
 *
 * In the context of VFD SWMR the delayed write list allows us to delay
 * metadata writes to the HDF5 file until it appears in all indexes in the
 * last max_lag ticks.  This is essential if a version of the page or
 * multi-page metadata entry already exists in the HDF5 file -- failure to
 * delay the write can result in a message from the future which will
 * likely be perciived as file corruption by the reader.
 *
 * To facilitate identification of entries that must be removed from the
 * DWL during the end of tick scan, the list always observes the following
 * invariant for any entry on the list:
 *
 *    entry_ptr->next == NULL ||
 *    entry_ptr->delay_write_until >= entry_ptr->next->delay_write_until
 *
 * Discussion of the fields used to implement the delayed write list follows:
 *
 * max_delay:   Maximum of the delay_write_until fields of the entries on
 *              the delayed write list.  This must never be more than max_lag
 *              ticks in advance of the current tick, and should be set to
 *              zero if the delayed write list is empty.
 *
 * dwl_len:     Number of page buffer entries currently on the delayed
 *              write list.
 *
 *              Observe that LRU_len + dwl_len must always equal
 *              index_len.
 *
 * dwl_size:    Number of bytes of page buffer entries currently residing
 *              on the LRU list.
 *
 *              Observe that LRU_size + dwl_size must always equal
 *              index_size.
 *
 * dwl_head_ptr:  Pointer to the head of the doubly linked delayed write list.
 *              Page buffer entries on this list are linked by their next and
 *              prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * dwl_tail_ptr:  Pointer to the tail of the doubly linked delayed write list.
 *              Page buffer entries on this list are linked by their next and
 *              prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * For VFD SWMR to function, copies of all pages modified during a tick must
 * be retained in the page buffer to allow correct updates to the index and
 * metadata file at the end of tick.
 *
 * To implement this, all entries modified during the current tick are placed
 * on the tick list.  Entries are removed from the tick list during end of
 * tick processing, so each tick starts with an empty tick list.
 *
 * Unless the entry also resides on the delayed write list, entries on the
 * tick list may be flushed, but they may not be evicted.
 *
 * Discussion of the fields used to implement the tick list follows:
 *
 * tl_len:      Number of page buffer entries currently on the tick list
 *
 * tl_size:     Number of bytes of page buffer entries currently residing
 *              on the tick list.
 *
 * tl_head_ptr:  Pointer to the head of the doubly linked tick list.
 *              Page buffer entries on this list are linked by their tl_next
 *              and tl_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * tl_tail_ptr:  Pointer to the tail of the doubly linked tick list.
 *              Page buffer entries on this list are linked by their tl_next
 *              and tl_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 *
 * STATISTICS:
 *
 * Multi-page metadata entries (which may only appear in VFD
 * SWMR mode) are NOT counted in the following statistics.
 *
 * Note that all statistics fields contain only data since the last time
 * that statistics were reset.
 *
 * bypasses:    Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of times that the page buffer has been
 *              bypassed for raw data, metadata, and for multi-page
 *              metadata entries (VFD SWMR only) as indexed by 5PB__STATS_MD,
 *              H5PB__STATS_RD, and H5PB__STATS_MPMDE respectively.
 *
 * accesses:    Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of page buffer accesses for raw data, metadata,
 *              and for multi-page metadata entries (VFD SWMR only) as
 *              indexed by 5PB__STATS_MD, H5PB__STATS_RD, and
 *              H5PB__STATS_MPMDE respectively.
 *
 * hits:        Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of page buffer hits for raw data, metadata,
 *              and for multi-page metadata entries (VFD SWMR only) as
 *              indexed by 5PB__STATS_MD, H5PB__STATS_RD, and
 *              H5PB__STATS_MPMDE respectively.
 *
 * misses:      Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of page buffer misses for raw data, metadata,
 *              and for multi-page metadata entries (VFD SWMR only) as
 *              indexed by 5PB__STATS_MD, H5PB__STATS_RD, and
 *              H5PB__STATS_MPMDE respectively.
 *
 * loads:       Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of page buffer loads for raw data, metadata,
 *              and for multi-page metadata entries (VFD SWMR only) as
 *              indexed by 5PB__STATS_MD, H5PB__STATS_RD, and
 *              H5PB__STATS_MPMDE respectively.
 *
 * insertions:  Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of page buffer insertions of raw data, metadata,
 *              and for multi-page metadata entries (VFD SWMR only) as
 *              indexed by 5PB__STATS_MD, H5PB__STATS_RD, and
 *              H5PB__STATS_MPMDE respectively.
 *
 * flushes:     Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of page buffer flushes of raw data, metadata,
 *              and for multi-page metadata entries (VFD SWMR only) as
 *              indexed by 5PB__STATS_MD, H5PB__STATS_RD, and
 *              H5PB__STATS_MPMDE respectively.
 *
 * evictions:   Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of page buffer evictions of raw data, metadata,
 *              and for multi-page metadata entries (VFD SWMR only) as
 *              indexed by 5PB__STATS_MD, H5PB__STATS_RD, and
 *              H5PB__STATS_MPMDE respectively.
 *
 * clears:      Array of int64_t of length H5PB__NUM_STAT_TYPES containing
 *              the number of page buffer entry clears of raw data, metadata,
 *              and for multi-page metadata entries (VFD SWMR only) as
 *              indexed by 5PB__STATS_MD, H5PB__STATS_RD, and
 *              H5PB__STATS_MPMDE respectively.
 *
 * max_lru_len: int64_t containing the maximum number of entries that
 *              have appeared in the LRU.
 *
 * max_lru_size: int64_t containing the maximum size of the LRU.
 *
 * lru_md_skips: When searching for an entry to evict, metadata entries on
 *              the LRU must be skipped if the number of metadata pages
 *              in the page buffer fails to exceed min_md_pages.
 *
 *              This int64_t is used to keep a count of these skips.
 *
 *              If this number becomes excessive, it will be necessary to
 *              add a holding tank for such entries.
 *
 * lru_rd_skips: When searching for an entry to evict, raw data entries on
 *              the LRU must be skipped if the number of raw data pages
 *              in the page buffer fails to exceed min_rd_pages.
 *
 *              This int64_t is used to keep a count of these skips.
 *
 *              If this number becomes excessive, it will be necessary to
 *              add a holding tank for such entries.
 *
 * Multi-page metadata entries (which appear only in VFD SWMR mode) are
 * listed in the hash take, and thus they are counted in the following
 * statistics.
 *
 * total_ht_insertions: Number of times entries have been inserted into the
 *              hash table.
 *
 * total_ht_deletions: Number of times entries have been deleted from the
 *              hash table.
 *
 * successful_ht_searches: int64 containing the total number of successful
 *              searches of the hash table.
 *
 * total_successful_ht_search_depth: int64 containing the total number of
 *              entries other than the targets examined in successful
 *              searches of the hash table.
 *
 * failed_ht_searches: int64 containing the total number of unsuccessful
 *              searches of the hash table.
 *
 * total_failed_ht_search_depth: int64 containing the total number of
 *              entries examined in unsuccessful searches of the hash
 *              table.
 *
 * max_index_len:  Largest value attained by the index_len field.
 *
 * max_clean_index_len:  Largest value attained by the clean_index_len field.
 *
 * max_dirty_index_len:  Largest value attained by the dirty_index_len field.
 *
 * max_index_size:  Largest value attained by the index_size field.
 *
 * max_clean_index_size:  Largest value attained by the clean_index_size field.
 *
 * max_dirty_index_size:  Largest value attained by the dirty_index_size field.
 *
 * max_rd_pages: Maximum number of raw data pages in the page buffer.
 *
 * max_md_pages: Maximum number of metadata pages in the page buffer.
 *
 *
 * Statistics pretaining to VFD SWMR.
 *
 * max_mpmde_count: Maximum number of multi-page metadata entries in the
 *              page buffer.
 *
 * lru_tl_skips: When searching for an entry to evict, metadata entries on
 *              the LRU must be skipped if they also reside on the tick list.
 *
 *              This int64_t is used to keep a count of these skips.
 *
 *              If this number becomes excessive, it will be necessary to
 *              add a holding tank for such entries.
 *
 * max_tl_len:  int64_t containing the maximum value of tl_len.
 *
 * max_tl_size: int64_t containing the maximum value of tl_size.
 *
 * delayed_writes: int64_t containing the total number of delayed writes.
 *
 * total_delay: int64_t containing the total number of ticks by which
 *              entry writes have been delayed.
 *
 * max_dwl_len: int64_t containing the maximum value of dwl_len.
 *
 * max_dwl_size: int64_t containing the maximum value of dwl_size.
 *
 * total_dwl_ins_depth: int64_t containing the total insertion depth
 *              required to maintain the odering invariant on the
 *              delayed write list.
 *
 * md_read_splits:  int64_t containing the number of metadata reads that
 *              are split into two or three sub-reads to manage the
 *              case in which a group of metadata cache clients
 *              sub-allocate entries from a single file space allocationn.
 *
 * md_write_splits:  int64_t containing the number of metadata writes that
 *              are split into two or three sub-writes to manage the
 *              case in which a group of metadata cache clients
 *              sub-allocate entries from a single file space allocationn.
 *
 ******************************************************************************/

#define H5PB__H5PB_T_MAGIC 0x01020304

#define H5PB__STATS_MD       0
#define H5PB__STATS_RD       1
#define H5PB__STATS_MPMDE    2
#define H5PB__NUM_STAT_TYPES 3

typedef struct H5PB_t {

    /* Fields for general operations: */

    uint32_t magic;
    size_t   page_size;
    int64_t  max_pages;
    int64_t  curr_pages;
    int64_t  curr_md_pages;
    int64_t  curr_rd_pages;
    int64_t  min_md_pages;
    int64_t  min_rd_pages;

    /* FAPL fields */
    size_t   max_size;
    unsigned min_meta_perc;
    unsigned min_raw_perc;

    /* index */
    H5PB_entry_t *(ht[H5PB__HASH_TABLE_LEN]);
    int64_t       index_len;
    int64_t       clean_index_len;
    int64_t       dirty_index_len;
    int64_t       index_size;
    int64_t       clean_index_size;
    int64_t       dirty_index_size;
    int64_t       il_len;
    int64_t       il_size;
    H5PB_entry_t *il_head;
    H5PB_entry_t *il_tail;

    /* LRU */
    int64_t       LRU_len;
    int64_t       LRU_size;
    H5PB_entry_t *LRU_head_ptr;
    H5PB_entry_t *LRU_tail_ptr;

    /* Fields for VFD SWMR operations: */

    hbool_t  vfd_swmr;
    hbool_t  vfd_swmr_writer;
    int64_t  mpmde_count;
    uint64_t cur_tick;

    /* delayed write list */
    uint64_t      max_delay;
    int64_t       dwl_len;
    int64_t       dwl_size;
    H5PB_entry_t *dwl_head_ptr;
    H5PB_entry_t *dwl_tail_ptr;

    /* tick list */
    int64_t       tl_len;
    int64_t       tl_size;
    H5PB_entry_t *tl_head_ptr;
    H5PB_entry_t *tl_tail_ptr;

    /* Statistics: */

    /* general operations statistics: */
    /* these statistics count pages only, not multi-page metadata entries
     * (that occur only in the VFD SWMR writer case).
     */
    int64_t  bypasses[H5PB__NUM_STAT_TYPES];
    int64_t  accesses[H5PB__NUM_STAT_TYPES];
    int64_t  hits[H5PB__NUM_STAT_TYPES];
    int64_t  misses[H5PB__NUM_STAT_TYPES];
    int64_t  loads[H5PB__NUM_STAT_TYPES];
    int64_t  insertions[H5PB__NUM_STAT_TYPES];
    int64_t  flushes[H5PB__NUM_STAT_TYPES];
    int64_t  evictions[H5PB__NUM_STAT_TYPES];
    int64_t  clears[H5PB__NUM_STAT_TYPES];
    uint64_t access_size_count[6];
    int64_t  max_lru_len;
    int64_t  max_lru_size;
    int64_t  lru_md_skips;
    int64_t  lru_rd_skips;

    /* In the VFD SWMR case, both pages and multi-page metadata entries
     * are stored in the index.  Thus mult-page metadata entries are
     * included in the index related statistics.
     */
    int64_t total_ht_insertions;
    int64_t total_ht_deletions;
    int64_t successful_ht_searches;
    int64_t total_successful_ht_search_depth;
    int64_t failed_ht_searches;
    int64_t total_failed_ht_search_depth;
    int64_t max_index_len;
    int64_t max_clean_index_len;
    int64_t max_dirty_index_len;
    int64_t max_index_size;
    int64_t max_clean_index_size;
    int64_t max_dirty_index_size;
    int64_t max_rd_pages;
    int64_t max_md_pages;

    /* vfd swmr statistics */
    int64_t max_mpmde_count;
    int64_t lru_tl_skips;
    int64_t max_tl_len;
    int64_t max_tl_size;
    int64_t delayed_writes;
    int64_t total_delay;
    int64_t max_dwl_len;
    int64_t max_dwl_size;
    int64_t total_dwl_ins_depth;
    int64_t md_read_splits;
    int64_t md_write_splits;

} H5PB_t;

/*****************************/
/* Library-private Variables */
/*****************************/

/***************************************/
/* Library-private Function Prototypes */
/***************************************/

/* General routines */
H5_DLL herr_t H5PB_create(H5F_shared_t *shared, size_t page_buffer_size, unsigned page_buf_min_meta_perc,
                          unsigned page_buf_min_raw_perc);

H5_DLL herr_t H5PB_flush(H5F_shared_t *);

H5_DLL herr_t H5PB_dest(H5F_shared_t *);

H5_DLL herr_t H5PB_add_new_page(H5F_shared_t *, H5FD_mem_t, haddr_t);

H5_DLL herr_t H5PB_update_entry(H5PB_t *, haddr_t, size_t, const void *);

H5_DLL herr_t H5PB_remove_entry(H5F_shared_t *, haddr_t);

H5_DLL herr_t H5PB_remove_entries(H5F_shared_t *, haddr_t, hsize_t);

H5_DLL herr_t H5PB_read(H5F_shared_t *, H5FD_mem_t, haddr_t, size_t, void * /*out*/);

H5_DLL herr_t H5PB_write(H5F_shared_t *, H5FD_mem_t, haddr_t, size_t, const void *);

/* VFD SWMR specific routines */
H5_DLL herr_t H5PB_vfd_swmr__release_delayed_writes(H5F_shared_t *);

H5_DLL herr_t H5PB_vfd_swmr__release_tick_list(H5F_shared_t *);

H5_DLL herr_t H5PB_vfd_swmr__set_tick(H5F_shared_t *);

H5_DLL herr_t H5PB_vfd_swmr__update_index(H5F_t *f, uint32_t *idx_ent_added_ptr,
                                          uint32_t *idx_ent_modified_ptr, uint32_t *idx_ent_not_in_tl_ptr,
                                          uint32_t *idx_ent_not_in_tl_flushed_ptr);

/* Statistics routines */
H5_DLL herr_t H5PB_reset_stats(H5PB_t *);

H5_DLL herr_t H5PB_get_stats(const H5PB_t *page_buf, unsigned accesses[2], unsigned hits[2],
                             unsigned misses[2], unsigned evictions[2], unsigned bypasses[2]);

H5_DLL herr_t H5PB_print_stats(const H5PB_t *);

/* test & debug functions */
H5_DLL herr_t H5PB_page_exists(H5F_shared_t *, haddr_t, hbool_t *);

#endif /* H5PBprivate_H */
