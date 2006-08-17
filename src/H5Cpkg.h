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
 * Programmer: John Mainzer -- 10/12/04
 *
 * Purpose:     This file contains declarations which are normally visible
 *              only within the H5C package (just H5C.c at present).
 *
 *		Source files outside the H5C package should include
 *		H5Cprivate.h instead.
 *
 *		The one exception to this rule is test/cache.c.  The test
 *		code is easier to write if it can look at the cache's
 *		internal data structures.  Indeed, this is the main
 *		reason why this file was created.
 */

#ifndef H5C_PACKAGE
#error "Do not include this file outside the H5HL package!"
#endif

#ifndef _H5Cpkg_H
#define _H5Cpkg_H


/* Get package's private header */
#include "H5Cprivate.h"


/* Get needed headers */
#include "H5SLprivate.h"        /* Skip lists */


#define H5C__HASH_TABLE_LEN     (64 * 1024) /* must be a power of 2 */


/****************************************************************************
 *
 * structure H5C_t
 *
 * Catchall structure for all variables specific to an instance of the cache.
 *
 * While the individual fields of the structure are discussed below, the
 * following overview may be helpful.
 *
 * Entries in the cache are stored in an instance of H5TB_TREE, indexed on
 * the entry's disk address.  While the H5TB_TREE is less efficient than
 * hash table, it keeps the entries in address sorted order.  As flushes
 * in parallel mode are more efficient if they are issued in increasing
 * address order, this is a significant benefit.  Also the H5TB_TREE code
 * was readily available, which reduced development time.
 *
 * While the cache was designed with multiple replacement policies in mind,
 * at present only a modified form of LRU is supported.
 *
 *                                              JRM - 4/26/04
 *
 * Profiling has indicated that searches in the instance of H5TB_TREE are
 * too expensive.  To deal with this issue, I have augmented the cache
 * with a hash table in which all entries will be stored.  Given the
 * advantages of flushing entries in increasing address order, the TBBT
 * is retained, but only dirty entries are stored in it.  At least for
 * now, we will leave entries in the TBBT after they are flushed.
 *
 * Note that index_size and index_len now refer to the total size of
 * and number of entries in the hash table.
 *
 *						JRM - 7/19/04
 *
 * The TBBT has since been replaced with a skip list.  This change
 * greatly predates this note.
 *
 *						JRM - 9/26/05
 *
 * magic:	Unsigned 32 bit integer always set to H5C__H5C_T_MAGIC.  This
 *		field is used to validate pointers to instances of H5C_t.
 *
 * trace_file_ptr:  File pointer pointing to the trace file, which is used
 *              to record cache operations for use in simulations and design
 *              studies.  This field will usually be NULL, indicating that
 *              no trace file should be recorded.
 *
 *              Since much of the code supporting the parallel metadata
 *              cache is in H5AC, we don't write the trace file from 
 *              H5C.  Instead, H5AC reads the trace_file_ptr as needed.
 *
 *              When we get to using H5C in other places, we may add
 *              code to write trace file data at the H5C level as well.
 *
 * aux_ptr:	Pointer to void used to allow wrapper code to associate
 *		its data with an instance of H5C_t.  The H5C cache code
 *		sets this field to NULL, and otherwise leaves it alone.
 *
 * max_type_id:	Integer field containing the maximum type id number assigned
 *		to a type of entry in the cache.  All type ids from 0 to
 *		max_type_id inclusive must be defined.  The names of the
 *		types are stored in the type_name_table discussed below, and
 *		indexed by the ids.
 *
 * type_name_table_ptr: Pointer to an array of pointer to char of length
 *		max_type_id + 1.  The strings pointed to by the entries
 *		in the array are the names of the entry types associated
 *		with the indexing type IDs.
 *
 * max_cache_size:  Nominal maximum number of bytes that may be stored in the
 *              cache.  This value should be viewed as a soft limit, as the
 *              cache can exceed this value under the following circumstances:
 *
 *              a) All entries in the cache are protected, and the cache is
 *                 asked to insert a new entry.  In this case the new entry
 *                 will be created.  If this causes the cache to exceed
 *                 max_cache_size, it will do so.  The cache will attempt
 *                 to reduce its size as entries are unprotected.
 *
 *              b) When running in parallel mode, the cache may not be
 *		   permitted to flush a dirty entry in response to a read.
 *		   If there are no clean entries available to evict, the
 *		   cache will exceed its maximum size.  Again the cache
 *                 will attempt to reduce its size to the max_cache_size
 *                 limit on the next cache write.
 *
 *		c) When an entry increases in size, the cache may exceed
 *		   the max_cache_size limit until the next time the cache
 *		   attempts to load or insert an entry.
 *
 * min_clean_size: Nominal minimum number of clean bytes in the cache.
 *              The cache attempts to maintain this number of bytes of
 *              clean data so as to avoid case b) above.  Again, this is
 *              a soft limit.
 *
 *
 * In addition to the call back functions required for each entry, the
 * cache requires the following call back functions for this instance of
 * the cache as a whole:
 *
 * check_write_permitted:  In certain applications, the cache may not
 *		be allowed to write to disk at certain time.  If specified,
 *		the check_write_permitted function is used to determine if
 *		a write is permissible at any given point in time.
 *
 *		If no such function is specified (i.e. this field is NULL),
 *		the cache uses the following write_permitted field to
 *		determine whether writes are permitted.
 *
 * write_permitted: If check_write_permitted is NULL, this boolean flag
 *		indicates whether writes are permitted.
 *
 * log_flush:	If provided, this function is called whenever a dirty
 *		entry is flushed to disk.
 *
 *
 * The cache requires an index to facilitate searching for entries.  The
 * following fields support that index.
 *
 * index_len:   Number of entries currently in the hash table used to index
 *		the cache.
 *
 * index_size:  Number of bytes of cache entries currently stored in the
 *              hash table used to index the cache.
 *
 *              This value should not be mistaken for footprint of the
 *              cache in memory.  The average cache entry is small, and
 *              the cache has a considerable overhead.  Multiplying the
 *              index_size by two should yield a conservative estimate
 *              of the cache's memory footprint.
 *
 * index:	Array of pointer to H5C_cache_entry_t of size
 *		H5C__HASH_TABLE_LEN.  At present, this value is a power
 *		of two, not the usual prime number.
 *
 *		I hope that the variable size of cache elements, the large
 *		hash table size, and the way in which HDF5 allocates space
 *		will combine to avoid problems with periodicity.  If so, we
 *		can use a trivial hash function (a bit-and and a 3 bit left
 *		shift) with some small savings.
 *
 *		If not, it will become evident in the statistics. Changing
 *		to the usual prime number length hash table will require
 *		changing the H5C__HASH_FCN macro and the deletion of the
 *		H5C__HASH_MASK #define.  No other changes should be required.
 *
 *
 * When we flush the cache, we need to write entries out in increasing
 * address order.  An instance of a skip list is used to store dirty entries in
 * sorted order.  Whether it is cheaper to sort the dirty entries as needed,
 * or to maintain the list is an open question.  At a guess, it depends
 * on how frequently the cache is flushed.  We will see how it goes.
 *
 * For now at least, I will not remove dirty entries from the list as they
 * are flushed. (this has been changed -- dirty entries are now removed from
 * the skip list as they are flushed.  JRM - 10/25/05)
 *
 * slist_len:   Number of entries currently in the skip list
 *              used to maintain a sorted list of dirty entries in the
 *              cache.
 *
 * slist_size:  Number of bytes of cache entries currently stored in the
 *              skip list used to maintain a sorted list of
 *              dirty entries in the cache.
 *
 * slist_ptr:   pointer to the instance of H5SL_t used maintain a sorted
 *              list of dirty entries in the cache.  This sorted list has
 *              two uses:
 *
 *              a) It allows us to flush dirty entries in increasing address
 *                 order, which results in significant savings.
 *
 *              b) It facilitates checking for adjacent dirty entries when
 *                 attempting to evict entries from the cache.  While we
 *                 don't use this at present, I hope that this will allow
 *                 some optimizations when I get to it.
 *
 *
 * When a cache entry is protected, it must be removed from the LRU
 * list(s) as it cannot be either flushed or evicted until it is unprotected.
 * The following fields are used to implement the protected list (pl).
 *
 * pl_len:      Number of entries currently residing on the protected list.
 *
 * pl_size:     Number of bytes of cache entries currently residing on the
 *              protected list.
 *
 * pl_head_ptr: Pointer to the head of the doubly linked list of protected
 *              entries.  Note that cache entries on this list are linked
 *              by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * pl_tail_ptr: Pointer to the tail of the doubly linked list of protected
 *              entries.  Note that cache entries on this list are linked
 *              by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 *
 * For very frequently used entries, the protect/unprotect overhead can
 * become burdensome.  To avoid this overhead, I have modified the cache
 * to allow entries to be "pinned".  A pinned entry is similar to a
 * protected entry, in the sense that it cannot be evicted, and that
 * the entry can be modified at any time.
 *
 * Pinning an entry has the following implications:
 *
 *	1) A pinned entry cannot be evicted.  Thus unprotected
 *         pinned entries reside in the pinned entry list, instead
 *         of the LRU list(s) (or other lists maintained by the current
 *         replacement policy code).
 *
 *      2) A pinned entry can be accessed or modified at any time.
 *         Therefore, the cache must check with the entry owner
 *         before flushing it.  If permission is denied, the
 *         cache just skips the entry in the flush.
 *
 *      3) A pinned entry can be marked as dirty (and possibly
 *         change size) while it is unprotected.
 *
 *      4) The flush-destroy code must allow pinned entries to
 *         be unpinned (and possibly unprotected) during the
 *         flush.
 *
 * Since pinned entries cannot be evicted, they must be kept on a pinned
 * entry list, instead of being entrusted to the replacement policy code.
 *
 * Maintaining the pinned entry list requires the following fields:
 *
 * pel_len:	Number of entries currently residing on the pinned
 * 		entry list.
 *
 * pel_size:	Number of bytes of cache entries currently residing on
 * 		the pinned entry list.
 *
 * pel_head_ptr: Pointer to the head of the doubly linked list of pinned
 * 		but not protected entries.  Note that cache entries on
 * 		this list are linked by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * pel_tail_ptr: Pointer to the tail of the doubly linked list of pinned
 * 		but not protected entries.  Note that cache entries on
 * 		this list are linked by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 *
 * The cache must have a replacement policy, and the fields supporting this
 * policy must be accessible from this structure.
 *
 * While there has been interest in several replacement policies for
 * this cache, the initial development schedule is tight.  Thus I have
 * elected to support only a modified LRU policy for the first cut.
 *
 * To further simplify matters, I have simply included the fields needed
 * by the modified LRU in this structure.  When and if we add support for
 * other policies, it will probably be easiest to just add the necessary
 * fields to this structure as well -- we only create one instance of this
 * structure per file, so the overhead is not excessive.
 *
 *
 * Fields supporting the modified LRU policy:
 *
 * See most any OS text for a discussion of the LRU replacement policy.
 *
 * When operating in parallel mode, we must ensure that a read does not
 * cause a write.  If it does, the process will hang, as the write will
 * be collective and the other processes will not know to participate.
 *
 * To deal with this issue, I have modified the usual LRU policy by adding
 * clean and dirty LRU lists to the usual LRU list.
 *
 * The clean LRU list is simply the regular LRU list with all dirty cache
 * entries removed.
 *
 * Similarly, the dirty LRU list is the regular LRU list with all the clean
 * cache entries removed.
 *
 * When reading in parallel mode, we evict from the clean LRU list only.
 * This implies that we must try to ensure that the clean LRU list is
 * reasonably well stocked at all times.
 *
 * We attempt to do this by trying to flush enough entries on each write
 * to keep the cLRU_list_size >= min_clean_size.
 *
 * Even if we start with a completely clean cache, a sequence of protects
 * without unprotects can empty the clean LRU list.  In this case, the
 * cache must grow temporarily.  At the next write, we will attempt to
 * evict enough entries to reduce index_size to less than max_cache_size.
 * While this will usually be possible, all bets are off if enough entries
 * are protected.
 *
 * Discussions of the individual fields used by the modified LRU replacement
 * policy follow:
 *
 * LRU_list_len:  Number of cache entries currently on the LRU list.
 *
 *              Observe that LRU_list_len + pl_len must always equal
 *              index_len.
 *
 * LRU_list_size:  Number of bytes of cache entries currently residing on the
 *              LRU list.
 *
 *              Observe that LRU_list_size + pl_size must always equal
 *              index_size.
 *
 * LRU_head_ptr:  Pointer to the head of the doubly linked LRU list.  Cache
 *              entries on this list are linked by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * LRU_tail_ptr:  Pointer to the tail of the doubly linked LRU list.  Cache
 *              entries on this list are linked by their next and prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * cLRU_list_len: Number of cache entries currently on the clean LRU list.
 *
 *              Observe that cLRU_list_len + dLRU_list_len must always
 *              equal LRU_list_len.
 *
 * cLRU_list_size:  Number of bytes of cache entries currently residing on
 *              the clean LRU list.
 *
 *              Observe that cLRU_list_size + dLRU_list_size must always
 *              equal LRU_list_size.
 *
 * cLRU_head_ptr:  Pointer to the head of the doubly linked clean LRU list.
 *              Cache entries on this list are linked by their aux_next and
 *              aux_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * cLRU_tail_ptr:  Pointer to the tail of the doubly linked clean LRU list.
 *              Cache entries on this list are linked by their aux_next and
 *              aux_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * dLRU_list_len: Number of cache entries currently on the dirty LRU list.
 *
 *              Observe that cLRU_list_len + dLRU_list_len must always
 *              equal LRU_list_len.
 *
 * dLRU_list_size:  Number of cache entries currently on the dirty LRU list.
 *
 *              Observe that cLRU_list_len + dLRU_list_len must always
 *              equal LRU_list_len.
 *
 * dLRU_head_ptr:  Pointer to the head of the doubly linked dirty LRU list.
 *              Cache entries on this list are linked by their aux_next and
 *              aux_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 * dLRU_tail_ptr:  Pointer to the tail of the doubly linked dirty LRU list.
 *              Cache entries on this list are linked by their aux_next and
 *              aux_prev fields.
 *
 *              This field is NULL if the list is empty.
 *
 *
 * Automatic cache size adjustment:
 *
 * While the default cache size is adequate for most cases, we can run into
 * cases where the default is too small.  Ideally, we will let the user
 * adjust the cache size as required.  However, this is not possible in all
 * cases.  Thus I have added automatic cache size adjustment code.
 *
 * The configuration for the automatic cache size adjustment is stored in
 * the structure described below:
 *
 * size_increase_possible:  Depending on the configuration data given
 *		in the resize_ctl field, it may or may not be possible
 *		to increase the size of the cache.  Rather than test for
 *		all the ways this can happen, we simply set this flag when
 *		we receive a new configuration.
 *
 * size_decrease_possible:  Depending on the configuration data given
 *              in the resize_ctl field, it may or may not be possible
 *              to decrease the size of the cache.  Rather than test for
 *              all the ways this can happen, we simply set this flag when
 *              we receive a new configuration.
 *
 * cache_full:	Boolean flag used to keep track of whether the cache is
 *		full, so we can refrain from increasing the size of a
 *		cache which hasn't used up the space alotted to it.
 *
 *		The field is initialized to FALSE, and then set to TRUE
 *		whenever we attempt to make space in the cache.
 *
 * resize_enabled:  This is another convenience flag which is set whenever
 *		a new set of values for resize_ctl are provided.  Very
 *		simply,
 *
 *		    resize_enabled = size_increase_possible ||
 *                                   size_decrease_possible;
 *
 * size_decreased:  Boolean flag set to TRUE whenever the maximun cache
 *		size is decreased.  The flag triggers a call to
 *		H5C_make_space_in_cache() on the next call to H5C_protect().
 *
 * resize_ctl:	Instance of H5C_auto_size_ctl_t containing configuration
 * 		data for automatic cache resizing.
 *
 * epoch_markers_active:  Integer field containing the number of epoch
 *		markers currently in use in the LRU list.  This value
 *		must be in the range [0, H5C__MAX_EPOCH_MARKERS - 1].
 *
 * epoch_marker_active:  Array of boolean of length H5C__MAX_EPOCH_MARKERS.
 *		This array is used to track which epoch markers are currently
 *		in use.
 *
 * epoch_marker_ringbuf:  Array of int of length H5C__MAX_EPOCH_MARKERS + 1.
 *
 *		To manage the epoch marker cache entries, it is necessary
 *		to track their order in the LRU list.  This is done with
 *		epoch_marker_ringbuf.  When markers are inserted at the
 *		head of the LRU list, the index of the marker in the
 *		epoch_markers array is inserted at the tail of the ring
 *		buffer.  When it becomes the epoch_marker_active'th marker
 *		in the LRU list, it will have worked its way to the head
 *		of the ring buffer as well.  This allows us to remove it
 *		without scanning the LRU list if such is required.
 *
 * epoch_marker_ringbuf_first: Integer field containing the index of the
 *		first entry in the ring buffer.
 *
 * epoch_marker_ringbuf_last: Integer field containing the index of the
 *		last entry in the ring buffer.
 *
 * epoch_marker_ringbuf_size: Integer field containing the number of entries
 *		in the ring buffer.
 *
 * epoch_markers:  Array of instances of H5C_cache_entry_t of length
 *		H5C__MAX_EPOCH_MARKERS.  The entries are used as markers
 *		in the LRU list to identify cache entries that haven't
 *		been accessed for some (small) specified number of
 *		epochs.  These entries (if any) can then be evicted and
 *		the cache size reduced -- ideally without evicting any
 *		of the current working set.  Needless to say, the epoch
 *		length and the number of epochs before an unused entry
 *		must be chosen so that all, or almost all, the working
 *		set will be accessed before the limit.
 *
 *		Epoch markers only appear in the LRU list, never in
 *		the index or slist.  While they are of type
 *		H5C__EPOCH_MARKER_TYPE, and have associated class
 *		functions, these functions should never be called.
 *
 *		The addr fields of these instances of H5C_cache_entry_t
 *		are set to the index of the instance in the epoch_markers
 *		array, the size is set to 0, and the type field points
 *		to the constant structure epoch_marker_class defined
 *		in H5C.c.  The next and prev fields are used as usual
 *		to link the entry into the LRU list.
 *
 *		All other fields are unused.
 *
 *
 * Cache hit rate collection fields:
 *
 * We supply the current cache hit rate on request, so we must keep a
 * simple cache hit rate computation regardless of whether statistics
 * collection is enabled.  The following fields support this capability.
 *
 * cache_hits: Number of cache hits since the last time the cache hit
 *	rate statistics were reset.  Note that when automatic cache
 *	re-sizing is enabled, this field will be reset every automatic
 *	resize epoch.
 *
 * cache_accesses: Number of times the cache has been accessed while
 *	since the last since the last time the cache hit rate statistics
 *	were reset.  Note that when automatic cache re-sizing is enabled,
 *	this field will be reset every automatic resize epoch.
 *
 *
 * Statistics collection fields:
 *
 * When enabled, these fields are used to collect statistics as described
 * below.  The first set are collected only when H5C_COLLECT_CACHE_STATS
 * is true.
 *
 * hits:        Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type id
 *		equal to the array index has been in cache when requested in
 *		the current epoch.
 *
 * misses:      Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type id
 *		equal to the array index has not been in cache when
 *		requested in the current epoch.
 *
 * insertions:  Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been inserted into the
 *		cache in the current epoch.
 *
 * pinned_insertions:  Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  
 * 		The cells are used to record the number of times an entry 
 * 		with type id equal to the array index has been inserted 
 * 		pinned into the cache in the current epoch.
 *
 * clears:      Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been cleared in the current
 *		epoch.
 *
 * flushes:     Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type id
 *		equal to the array index has been written to disk in the
 *              current epoch.
 *
 * evictions:   Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type id
 *		equal to the array index has been evicted from the cache in
 *		the current epoch.
 *
 * renames:     Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been renamed in the current
 *		epoch.
 *
 * pins:        Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been pinned in the current
 *		epoch.
 *
 * unpins:      Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been unpinned in the current
 *		epoch.
 *
 * dirty_pins:	Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the number of times an entry with type
 *		id equal to the array index has been marked dirty while pinned
 *		in the current epoch.
 *
 * pinned_flushes:  Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The
 * 		cells are used to record the number of times an  entry
 * 		with type id equal to the array index has been flushed while
 * 		pinned in the current epoch.
 *
 * pinned_cleared:  Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.  The
 * 		cells are used to record the number of times an  entry
 * 		with type id equal to the array index has been cleared while
 * 		pinned in the current epoch.
 *
 *
 * size_increases:  Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.
 *		The cells are used to record the number of times an entry
 *		with type id equal to the array index has increased in
 *		size in the current epoch.
 *
 * size_decreases:  Array of int64 of length H5C__MAX_NUM_TYPE_IDS + 1.
 *		The cells are used to record the number of times an entry
 *		with type id equal to the array index has decreased in
 *		size in the current epoch.
 *
 * total_ht_insertions: Number of times entries have been inserted into the
 *		hash table in the current epoch.
 *
 * total_ht_deletions: Number of times entries have been deleted from the
 *              hash table in the current epoch.
 *
 * successful_ht_searches: int64 containing the total number of successful
 *		searches of the hash table in the current epoch.
 *
 * total_successful_ht_search_depth: int64 containing the total number of
 *		entries other than the targets examined in successful
 *		searches of the hash table in the current epoch.
 *
 * failed_ht_searches: int64 containing the total number of unsuccessful
 *              searches of the hash table in the current epoch.
 *
 * total_failed_ht_search_depth: int64 containing the total number of
 *              entries examined in unsuccessful searches of the hash
 *		table in the current epoch.
 *
 * max_index_len:  Largest value attained by the index_len field in the
 *              current epoch.
 *
 * max_index_size:  Largest value attained by the index_size field in the
 *              current epoch.
 *
 * max_slist_len:  Largest value attained by the slist_len field in the
 *              current epoch.
 *
 * max_slist_size:  Largest value attained by the slist_size field in the
 *              current epoch.
 *
 * max_pl_len:  Largest value attained by the pl_len field in the
 *              current epoch.
 *
 * max_pl_size: Largest value attained by the pl_size field in the
 *              current epoch.
 *
 * max_pel_len: Largest value attained by the pel_len field in the
 *              current epoch.
 *
 * max_pel_size: Largest value attained by the pel_size field in the
 *              current epoch.
 *
 * The remaining stats are collected only when both H5C_COLLECT_CACHE_STATS
 * and H5C_COLLECT_CACHE_ENTRY_STATS are true.
 *
 * max_accesses: Array of int32 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the maximum number of times any single
 *		entry with type id equal to the array index has been
 *		accessed in the current epoch.
 *
 * min_accesses: Array of int32 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the minimum number of times any single
 *		entry with type id equal to the array index has been
 *		accessed in the current epoch.
 *
 * max_clears:  Array of int32 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the maximum number of times any single
 *		entry with type id equal to the array index has been cleared
 *		in the current epoch.
 *
 * max_flushes: Array of int32 of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *		are used to record the maximum number of times any single
 *		entry with type id equal to the array index has been
 *		flushed in the current epoch.
 *
 * max_size:	Array of size_t of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *              are used to record the maximum size of any single entry
 *		with type id equal to the array index that has resided in
 *		the cache in the current epoch.
 *
 * max_pins:	Array of size_t of length H5C__MAX_NUM_TYPE_IDS + 1.  The cells
 *              are used to record the maximum number of times that any single
 *              entry with type id equal to the array index that has been
 *              marked as pinned in the cache in the current epoch.
 *
 *
 * Fields supporting testing:
 *
 * For test purposes, it is useful to turn off some asserts and sanity
 * checks.  The following flags support this.
 *
 * skip_file_checks:  Boolean flag used to skip sanity checks on file
 *		parameters passed to the cache.  In the test bed, there
 *		is no reason to have a file open, as the cache proper
 *		just passes these parameters through without using them.
 *
 *		When this flag is set, all sanity checks on the file
 *		parameters are skipped.  The field defaults to FALSE.
 *
 * skip_dxpl_id_checks:  Boolean flag used to skip sanity checks on the
 *		dxpl_id parameters passed to the cache.  These are not
 *		used directly by the cache, so skipping the checks
 *		simplifies the test bed.
 *
 *		When this flag is set, all sanity checks on the dxpl_id
 *		parameters are skipped.  The field defaults to FALSE.
 *
 * prefix	Array of char used to prefix debugging output.  The
 *		field is intended to allow marking of output of with
 *		the processes mpi rank.
 *
 ****************************************************************************/

#define H5C__H5C_T_MAGIC	0x005CAC0E
#define H5C__MAX_NUM_TYPE_IDS	14
#define H5C__PREFIX_LEN		32

struct H5C_t
{
    uint32_t			magic;

    FILE *			trace_file_ptr;

    void *			aux_ptr;

    int32_t			max_type_id;
    const char *                (* type_name_table_ptr);

    size_t                      max_cache_size;
    size_t                      min_clean_size;

    H5C_write_permitted_func_t	check_write_permitted;
    hbool_t			write_permitted;

    H5C_log_flush_func_t	log_flush;

    int32_t                     index_len;
    size_t                      index_size;
    H5C_cache_entry_t *		(index[H5C__HASH_TABLE_LEN]);


    int32_t                     slist_len;
    size_t                      slist_size;
    H5SL_t *                    slist_ptr;


    int32_t                     pl_len;
    size_t                      pl_size;
    H5C_cache_entry_t *	        pl_head_ptr;
    H5C_cache_entry_t *  	pl_tail_ptr;

    int32_t                     pel_len;
    size_t                      pel_size;
    H5C_cache_entry_t *	        pel_head_ptr;
    H5C_cache_entry_t *  	pel_tail_ptr;

    int32_t                     LRU_list_len;
    size_t                      LRU_list_size;
    H5C_cache_entry_t *		LRU_head_ptr;
    H5C_cache_entry_t *		LRU_tail_ptr;

    int32_t                     cLRU_list_len;
    size_t                      cLRU_list_size;
    H5C_cache_entry_t *		cLRU_head_ptr;
    H5C_cache_entry_t *		cLRU_tail_ptr;

    int32_t                     dLRU_list_len;
    size_t                      dLRU_list_size;
    H5C_cache_entry_t *		dLRU_head_ptr;
    H5C_cache_entry_t *	        dLRU_tail_ptr;

    hbool_t			size_increase_possible;
    hbool_t			size_decrease_possible;
    hbool_t			resize_enabled;
    hbool_t			cache_full;
    hbool_t			size_decreased;
    H5C_auto_size_ctl_t		resize_ctl;

    int32_t			epoch_markers_active;
    hbool_t			epoch_marker_active[H5C__MAX_EPOCH_MARKERS];
    int32_t			epoch_marker_ringbuf[H5C__MAX_EPOCH_MARKERS+1];
    int32_t			epoch_marker_ringbuf_first;
    int32_t			epoch_marker_ringbuf_last;
    int32_t			epoch_marker_ringbuf_size;
    H5C_cache_entry_t		epoch_markers[H5C__MAX_EPOCH_MARKERS];

    int64_t			cache_hits;
    int64_t			cache_accesses;

#if H5C_COLLECT_CACHE_STATS

    /* stats fields */
    int64_t                     hits[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     misses[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     insertions[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     pinned_insertions[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     clears[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     flushes[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     evictions[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     renames[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     pins[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     unpins[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     dirty_pins[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     pinned_flushes[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     pinned_clears[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     size_increases[H5C__MAX_NUM_TYPE_IDS + 1];
    int64_t                     size_decreases[H5C__MAX_NUM_TYPE_IDS + 1];

    int64_t			total_ht_insertions;
    int64_t			total_ht_deletions;
    int64_t			successful_ht_searches;
    int64_t			total_successful_ht_search_depth;
    int64_t			failed_ht_searches;
    int64_t			total_failed_ht_search_depth;

    int32_t                     max_index_len;
    size_t                      max_index_size;

    int32_t                     max_slist_len;
    size_t                      max_slist_size;

    int32_t                     max_pl_len;
    size_t                      max_pl_size;

    int32_t                     max_pel_len;
    size_t                      max_pel_size;

#if H5C_COLLECT_CACHE_ENTRY_STATS

    int32_t                     max_accesses[H5C__MAX_NUM_TYPE_IDS + 1];
    int32_t                     min_accesses[H5C__MAX_NUM_TYPE_IDS + 1];
    int32_t                     max_clears[H5C__MAX_NUM_TYPE_IDS + 1];
    int32_t                     max_flushes[H5C__MAX_NUM_TYPE_IDS + 1];
    size_t                      max_size[H5C__MAX_NUM_TYPE_IDS + 1];
    int32_t                     max_pins[H5C__MAX_NUM_TYPE_IDS + 1];

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

#endif /* H5C_COLLECT_CACHE_STATS */

    hbool_t			skip_file_checks;
    hbool_t			skip_dxpl_id_checks;
    char			prefix[H5C__PREFIX_LEN];
};

#endif /* _H5Cpkg_H */

