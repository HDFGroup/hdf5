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
 * Created:     H5Cdbg.c
 *              July 8 2016
 *              Quincey Koziol
 *
 * Purpose:     Debugging Routines for the generic cache structure or entries.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Cmodule.h"          /* This source code file is part of the H5C module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Cpkg.h"		/* Cache				*/
#include "H5Eprivate.h"		/* Error handling		  	*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

#if 0 /* debugging routines */
herr_t H5C_dump_cache_skip_list(H5C_t *cache_ptr, char *calling_fcn);
#endif /* debugging routines */


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
 * Function:    H5C_dump_cache
 *
 * Purpose:     Print a summary of the contents of the metadata cache for
 *              debugging purposes.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              10/10/10
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_dump_cache(H5C_t * cache_ptr, const char *  cache_name)
{
    H5C_cache_entry_t * entry_ptr;
    H5SL_t *            slist_ptr = NULL;
    int                 i;                      /* Local index variable */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_name != NULL );

    /* First, create a skip list */
    if(NULL == (slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "can't create skip list.")

    /* Next, scan the index, and insert all entries in the skip list.
     * Do this, as we want to display cache entries in increasing address
     * order.
     */
    for(i = 0; i < H5C__HASH_TABLE_LEN; i++) {
        entry_ptr = cache_ptr->index[i];

        while(entry_ptr != NULL) {
            HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
            if(H5SL_insert(slist_ptr, entry_ptr, &(entry_ptr->addr)) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "can't insert entry in skip list")

            entry_ptr = entry_ptr->ht_next;
        } /* end while */
    } /* end for */

    /* If we get this far, all entries in the cache are listed in the
     * skip list -- scan the skip list generating the desired output.
     */

    HDfprintf(stdout, "\n\nDump of metadata cache \"%s\".\n", cache_name);
    HDfprintf(stdout,
        "Num:    Addr:                             Len:    Type:   Prot:   Pinned: Dirty:\n");

    i = 0;
    entry_ptr = (H5C_cache_entry_t *)H5SL_remove_first(slist_ptr);
    while(entry_ptr != NULL) {
        HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);

        HDfprintf(stdout,
            "%s%d       0x%16llx                0x%3llx      %2d     %d      %d      %d\n",
             cache_ptr->prefix, i,
             (long long)(entry_ptr->addr),
             (long long)(entry_ptr->size),
             (int)(entry_ptr->type->id),
             (int)(entry_ptr->is_protected),
             (int)(entry_ptr->is_pinned),
             (int)(entry_ptr->is_dirty));

        /* remove the next (first) item in the skip list */
        entry_ptr = (H5C_cache_entry_t *)H5SL_remove_first(slist_ptr);

        i++;
    } /* end while */

    HDfprintf(stdout, "\n\n");

    /* Verify that all the entries were removed from the skip list */
    HDassert(H5SL_count(slist_ptr) == 0);

done:
    /* Discard the skip list */
    if(slist_ptr)
        H5SL_close(slist_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_dump_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5C_dump_cache_skip_list
 *
 * Purpose:     Debugging routine that prints a summary of the contents of 
 *		the skip list used by the metadata cache metadata cache to 
 *		maintain an address sorted list of dirty entries.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              11/15/14
 *
 *-------------------------------------------------------------------------
 */
#if 0 /* debugging routine */
herr_t
H5C_dump_cache_skip_list(H5C_t * cache_ptr, char * calling_fcn)
{
    herr_t              ret_value = SUCCEED;   /* Return value */
    int                 i;
    H5C_cache_entry_t * entry_ptr = NULL;
    H5SL_node_t *       node_ptr = NULL;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(calling_fcn != NULL);

    HDfprintf(stdout, "\n\nDumping metadata cache skip list from %s.\n", calling_fcn);
    HDfprintf(stdout, "	slist len = %d.\n", cache_ptr->slist_len);
    HDfprintf(stdout, "	slist size = %lld.\n", (long long)(cache_ptr->slist_size));

    if(cache_ptr->slist_len > 0) {
        /* If we get this far, all entries in the cache are listed in the
         * skip list -- scan the skip list generating the desired output.
         */
        HDfprintf(stdout,
                  "Num:    Addr:               Len: Prot/Pind: Dirty: Type:\n");

        i = 0;
        node_ptr = H5SL_first(cache_ptr->slist_ptr);
        if(node_ptr != NULL)
            entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);
        else
            entry_ptr = NULL;

        while(entry_ptr != NULL) {
            HDassert( entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC );

            HDfprintf(stdout,
               "%s%d       0x%016llx  %4lld    %d/%d       %d    %s\n",
               cache_ptr->prefix, i,
               (long long)(entry_ptr->addr),
               (long long)(entry_ptr->size),
               (int)(entry_ptr->is_protected),
               (int)(entry_ptr->is_pinned),
               (int)(entry_ptr->is_dirty),
               entry_ptr->type->name);

            HDfprintf(stdout, "		node_ptr = 0x%llx, item = 0x%llx\n",
                      (unsigned long long)node_ptr,
                      (unsigned long long)H5SL_item(node_ptr));

            /* increment node_ptr before we delete its target */
            node_ptr = H5SL_next(node_ptr);
            if(node_ptr != NULL)
                entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);
            else
                entry_ptr = NULL;

            i++;
        } /* end while */
    } /* end if */

    HDfprintf(stdout, "\n\n");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_dump_cache_skip_list() */
#endif /* debugging routine */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_prefix
 *
 * Purpose:     Set the values of the prefix field of H5C_t.  This
 *		filed is used to label some debugging output.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              1/20/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_set_prefix(H5C_t * cache_ptr, char * prefix)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if((cache_ptr == NULL) || (cache_ptr->magic != H5C__H5C_T_MAGIC) ||
            (prefix == NULL) || (HDstrlen(prefix) >= H5C__PREFIX_LEN))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad param(s) on entry.")

    HDstrncpy(&(cache_ptr->prefix[0]), prefix, (size_t)(H5C__PREFIX_LEN));

    cache_ptr->prefix[H5C__PREFIX_LEN - 1] = '\0';

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_set_prefix() */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_trace_file_ptr
 *
 * Purpose:     Set the trace_file_ptr field for the cache.
 *
 *              This field must either be NULL (which turns of trace
 *              file logging), or be a pointer to an open file to which
 *              trace file data is to be written.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              1/20/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_set_trace_file_ptr(H5C_t * cache_ptr, FILE * trace_file_ptr)
{
    herr_t		ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if((NULL == cache_ptr) || (cache_ptr->magic != H5C__H5C_T_MAGIC))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr")

    cache_ptr->trace_file_ptr = trace_file_ptr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_set_trace_file_ptr() */


/*-------------------------------------------------------------------------
 * Function:    H5C_stats
 *
 * Purpose:     Prints statistics about the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 *		JRM -- 11/13/08
 *		Added code displaying the max_clean_index_size and
 *		max_dirty_index_size.
 *
 *              MAM -- 01/06/09
 *              Added code displaying the calls_to_msic,
 *              total_entries_skipped_in_msic, total_entries_scanned_in_msic,
 *              and max_entries_skipped_in_msic fields.
 *
 *		JRM -- 4/11/15
 *		Added code displaying the new slist_scan_restarts,
 *		LRU_scan_restarts, and hash_bucket_scan_restarts fields;
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_stats(H5C_t * cache_ptr,
          const char *  cache_name,
          hbool_t
#if !H5C_COLLECT_CACHE_STATS
          H5_ATTR_UNUSED
#endif /* H5C_COLLECT_CACHE_STATS */
          display_detailed_stats)
{
#if H5C_COLLECT_CACHE_STATS
    int		i;
    int64_t     total_hits = 0;
    int64_t     total_misses = 0;
    int64_t	total_write_protects = 0;
    int64_t	total_read_protects = 0;
    int64_t	max_read_protects = 0;
    int64_t     total_insertions = 0;
    int64_t     total_pinned_insertions = 0;
    int64_t     total_clears = 0;
    int64_t     total_flushes = 0;
    int64_t     total_evictions = 0;
    int64_t     total_take_ownerships = 0;
    int64_t     total_moves = 0;
    int64_t     total_entry_flush_moves = 0;
    int64_t     total_cache_flush_moves = 0;
    int64_t	total_size_increases = 0;
    int64_t	total_size_decreases = 0;
    int64_t	total_entry_flush_size_changes = 0;
    int64_t	total_cache_flush_size_changes = 0;
    int64_t	total_pins = 0;
    int64_t	total_unpins = 0;
    int64_t	total_dirty_pins = 0;
    int64_t	total_pinned_flushes = 0;
    int64_t	total_pinned_clears = 0;
    int32_t     aggregate_max_accesses = 0;
    int32_t     aggregate_min_accesses = 1000000;
    int32_t     aggregate_max_clears = 0;
    int32_t     aggregate_max_flushes = 0;
    size_t      aggregate_max_size = 0;
    int32_t	aggregate_max_pins = 0;
    double      hit_rate;
    double	average_successful_search_depth = 0.0f;
    double	average_failed_search_depth = 0.0f;
    double      average_entries_skipped_per_calls_to_msic = 0.0f;
    double      average_entries_scanned_per_calls_to_msic = 0.0f;
#endif /* H5C_COLLECT_CACHE_STATS */
    herr_t	ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if((NULL == cache_ptr) || (cache_ptr->magic != H5C__H5C_T_MAGIC) ||
            (NULL == cache_name))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr or cache_name")

#if H5C_COLLECT_CACHE_STATS
    for(i = 0; i <= cache_ptr->max_type_id; i++ ) {
        total_hits              += cache_ptr->hits[i];
        total_misses            += cache_ptr->misses[i];
	total_write_protects	+= cache_ptr->write_protects[i];
	total_read_protects	+= cache_ptr->read_protects[i];
	if(max_read_protects < cache_ptr->max_read_protects[i])
	    max_read_protects = cache_ptr->max_read_protects[i];
        total_insertions        += cache_ptr->insertions[i];
        total_pinned_insertions += cache_ptr->pinned_insertions[i];
        total_clears            += cache_ptr->clears[i];
        total_flushes           += cache_ptr->flushes[i];
        total_evictions         += cache_ptr->evictions[i];
        total_take_ownerships   += cache_ptr->take_ownerships[i];
        total_moves             += cache_ptr->moves[i];
	total_entry_flush_moves += cache_ptr->entry_flush_moves[i];
	total_cache_flush_moves += cache_ptr->cache_flush_moves[i];
        total_size_increases    += cache_ptr->size_increases[i];
        total_size_decreases    += cache_ptr->size_decreases[i];
    	total_entry_flush_size_changes
				+= cache_ptr->entry_flush_size_changes[i];
    	total_cache_flush_size_changes
				+= cache_ptr->cache_flush_size_changes[i];
	total_pins              += cache_ptr->pins[i];
	total_unpins            += cache_ptr->unpins[i];
	total_dirty_pins        += cache_ptr->dirty_pins[i];
	total_pinned_flushes    += cache_ptr->pinned_flushes[i];
	total_pinned_clears     += cache_ptr->pinned_clears[i];
#if H5C_COLLECT_CACHE_ENTRY_STATS
        if(aggregate_max_accesses < cache_ptr->max_accesses[i])
            aggregate_max_accesses = cache_ptr->max_accesses[i];
        if(aggregate_min_accesses > aggregate_max_accesses)
            aggregate_min_accesses = aggregate_max_accesses;
        if(aggregate_min_accesses > cache_ptr->min_accesses[i])
            aggregate_min_accesses = cache_ptr->min_accesses[i];
        if(aggregate_max_clears < cache_ptr->max_clears[i])
            aggregate_max_clears = cache_ptr->max_clears[i];
        if(aggregate_max_flushes < cache_ptr->max_flushes[i])
            aggregate_max_flushes = cache_ptr->max_flushes[i];
        if(aggregate_max_size < cache_ptr->max_size[i])
            aggregate_max_size = cache_ptr->max_size[i];
        if(aggregate_max_pins < cache_ptr->max_pins[i])
            aggregate_max_pins = cache_ptr->max_pins[i];
#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
    } /* end for */

    if((total_hits > 0) || (total_misses > 0))
        hit_rate = (double)100.0f * ((double)(total_hits)) /
                   ((double)(total_hits + total_misses));
    else
        hit_rate = 0.0f;

    if(cache_ptr->successful_ht_searches > 0)
        average_successful_search_depth =
            ((double)(cache_ptr->total_successful_ht_search_depth)) /
            ((double)(cache_ptr->successful_ht_searches));

    if(cache_ptr->failed_ht_searches > 0)
        average_failed_search_depth =
            ((double)(cache_ptr->total_failed_ht_search_depth)) /
            ((double)(cache_ptr->failed_ht_searches));


    HDfprintf(stdout, "\n%sH5C: cache statistics for %s\n",
              cache_ptr->prefix, cache_name);

    HDfprintf(stdout, "\n");

    HDfprintf(stdout,
              "%s  hash table insertion / deletions   = %ld / %ld\n",
              cache_ptr->prefix,
              (long)(cache_ptr->total_ht_insertions),
              (long)(cache_ptr->total_ht_deletions));

    HDfprintf(stdout,
              "%s  HT successful / failed searches    = %ld / %ld\n",
              cache_ptr->prefix,
              (long)(cache_ptr->successful_ht_searches),
              (long)(cache_ptr->failed_ht_searches));

    HDfprintf(stdout,
              "%s  Av. HT suc / failed search depth   = %f / %f\n",
              cache_ptr->prefix,
              average_successful_search_depth,
              average_failed_search_depth);

    HDfprintf(stdout,
             "%s  current (max) index size / length  = %ld (%ld) / %ld (%ld)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->index_size),
              (long)(cache_ptr->max_index_size),
              (long)(cache_ptr->index_len),
              (long)(cache_ptr->max_index_len));

    HDfprintf(stdout,
             "%s  current (max) clean/dirty idx size = %ld (%ld) / %ld (%ld)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->clean_index_size),
              (long)(cache_ptr->max_clean_index_size),
              (long)(cache_ptr->dirty_index_size),
              (long)(cache_ptr->max_dirty_index_size));

    HDfprintf(stdout,
             "%s  current (max) slist size / length  = %ld (%ld) / %ld (%ld)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->slist_size),
              (long)(cache_ptr->max_slist_size),
              (long)(cache_ptr->slist_len),
              (long)(cache_ptr->max_slist_len));

    HDfprintf(stdout,
             "%s  current (max) PL size / length     = %ld (%ld) / %ld (%ld)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->pl_size),
              (long)(cache_ptr->max_pl_size),
              (long)(cache_ptr->pl_len),
              (long)(cache_ptr->max_pl_len));

    HDfprintf(stdout,
             "%s  current (max) PEL size / length    = %ld (%ld) / %ld (%ld)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->pel_size),
              (long)(cache_ptr->max_pel_size),
              (long)(cache_ptr->pel_len),
              (long)(cache_ptr->max_pel_len));

    HDfprintf(stdout,
              "%s  current LRU list size / length     = %ld / %ld\n",
              cache_ptr->prefix,
              (long)(cache_ptr->LRU_list_size),
              (long)(cache_ptr->LRU_list_len));

    HDfprintf(stdout,
              "%s  current clean LRU size / length    = %ld / %ld\n",
              cache_ptr->prefix,
              (long)(cache_ptr->cLRU_list_size),
              (long)(cache_ptr->cLRU_list_len));

    HDfprintf(stdout,
              "%s  current dirty LRU size / length    = %ld / %ld\n",
              cache_ptr->prefix,
              (long)(cache_ptr->dLRU_list_size),
              (long)(cache_ptr->dLRU_list_len));

    HDfprintf(stdout,
              "%s  Total hits / misses / hit_rate     = %ld / %ld / %f\n",
              cache_ptr->prefix,
              (long)total_hits,
              (long)total_misses,
              hit_rate);

    HDfprintf(stdout,
              "%s  Total write / read (max) protects  = %ld / %ld (%ld)\n",
              cache_ptr->prefix,
              (long)total_write_protects,
              (long)total_read_protects,
              (long)max_read_protects);

    HDfprintf(stdout,
              "%s  Total clears / flushes             = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_clears,
              (long)total_flushes);

    HDfprintf(stdout,
              "%s  Total evictions / take ownerships  = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_evictions,
              (long)total_take_ownerships);

    HDfprintf(stdout,
	      "%s  Total insertions(pinned) / moves   = %ld(%ld) / %ld\n",
              cache_ptr->prefix,
              (long)total_insertions,
              (long)total_pinned_insertions,
              (long)total_moves);

    HDfprintf(stdout,
	      "%s  Total entry / cache flush moves    = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_entry_flush_moves,
              (long)total_cache_flush_moves);

    HDfprintf(stdout, "%s  Total entry size incrs / decrs     = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_size_increases,
              (long)total_size_decreases);

    HDfprintf(stdout, "%s  Ttl entry/cache flush size changes = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_entry_flush_size_changes,
              (long)total_cache_flush_size_changes);

    HDfprintf(stdout,
	      "%s  Total entry pins (dirty) / unpins  = %ld (%ld) / %ld\n",
              cache_ptr->prefix,
              (long)total_pins,
	      (long)total_dirty_pins,
              (long)total_unpins);

    HDfprintf(stdout, "%s  Total pinned flushes / clears      = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_pinned_flushes,
              (long)total_pinned_clears);

    HDfprintf(stdout, "%s  MSIC: (make space in cache) calls  = %lld\n",
              cache_ptr->prefix,
              (long long)(cache_ptr->calls_to_msic));

    if (cache_ptr->calls_to_msic > 0) {
        average_entries_skipped_per_calls_to_msic =
            (((double)(cache_ptr->total_entries_skipped_in_msic)) /
            ((double)(cache_ptr->calls_to_msic)));
    }

    HDfprintf(stdout, "%s  MSIC: Average/max entries skipped  = %lf / %ld\n",
              cache_ptr->prefix,
              (double)average_entries_skipped_per_calls_to_msic,
              (long)(cache_ptr->max_entries_skipped_in_msic));

    if(cache_ptr->calls_to_msic > 0)
        average_entries_scanned_per_calls_to_msic =
            (((double)(cache_ptr->total_entries_scanned_in_msic)) /
            ((double)(cache_ptr->calls_to_msic)));

    HDfprintf(stdout, "%s  MSIC: Average/max entries scanned  = %lf / %ld\n",
              cache_ptr->prefix,
              (double)average_entries_scanned_per_calls_to_msic,
              (long)(cache_ptr->max_entries_scanned_in_msic));

    HDfprintf(stdout, "%s  MSIC: Scanned to make space(evict) = %lld\n",
              cache_ptr->prefix,
              (long long)(cache_ptr->entries_scanned_to_make_space));

    HDfprintf(stdout, "%s  MSIC: Scanned to satisfy min_clean = %lld\n",
              cache_ptr->prefix,
              (long long)(cache_ptr->total_entries_scanned_in_msic -
                            cache_ptr->entries_scanned_to_make_space));

    HDfprintf(stdout, 
              "%s  slist/LRU/hash bkt scan restarts   = %lld / %lld / %lld.\n",
              cache_ptr->prefix, 
              (long long)(cache_ptr->slist_scan_restarts),
              (long long)(cache_ptr->LRU_scan_restarts),
              (long long)(cache_ptr->hash_bucket_scan_restarts));

#if H5C_COLLECT_CACHE_ENTRY_STATS

    HDfprintf(stdout, "%s  aggregate max / min accesses       = %d / %d\n",
              cache_ptr->prefix,
              (int)aggregate_max_accesses,
              (int)aggregate_min_accesses);

    HDfprintf(stdout, "%s  aggregate max_clears / max_flushes = %d / %d\n",
              cache_ptr->prefix,
              (int)aggregate_max_clears,
              (int)aggregate_max_flushes);

    HDfprintf(stdout, "%s  aggregate max_size / max_pins      = %d / %d\n",
              cache_ptr->prefix,
              (int)aggregate_max_size,
	      (int)aggregate_max_pins);

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

    if(display_detailed_stats) {
        for(i = 0; i <= cache_ptr->max_type_id; i++) {
            HDfprintf(stdout, "\n");

            HDfprintf(stdout, "%s  Stats on %s:\n",
                      cache_ptr->prefix,
                      ((cache_ptr->type_name_table_ptr))[i]);

            if((cache_ptr->hits[i] > 0) || (cache_ptr->misses[i] > 0))
                hit_rate = (double)100.0f * ((double)(cache_ptr->hits[i])) /
                          ((double)(cache_ptr->hits[i] + cache_ptr->misses[i]));
            else
                hit_rate = 0.0f;

            HDfprintf(stdout,
                      "%s    hits / misses / hit_rate       = %ld / %ld / %f\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->hits[i]),
                      (long)(cache_ptr->misses[i]),
                      hit_rate);

            HDfprintf(stdout,
                      "%s    write / read (max) protects    = %ld / %ld (%d)\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->write_protects[i]),
                      (long)(cache_ptr->read_protects[i]),
                      (int)(cache_ptr->max_read_protects[i]));

            HDfprintf(stdout,
                      "%s    clears / flushes               = %ld / %ld\n", 
                      cache_ptr->prefix,
                      (long)(cache_ptr->clears[i]),
                      (long)(cache_ptr->flushes[i]));

            HDfprintf(stdout,
                      "%s    evictions / take ownerships    = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->evictions[i]),
                      (long)(cache_ptr->take_ownerships[i]));

            HDfprintf(stdout,
                      "%s    insertions(pinned) / moves     = %ld(%ld) / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->insertions[i]),
                      (long)(cache_ptr->pinned_insertions[i]),
                      (long)(cache_ptr->moves[i]));

            HDfprintf(stdout,
                      "%s    entry / cache flush moves      = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->entry_flush_moves[i]),
                      (long)(cache_ptr->cache_flush_moves[i]));

            HDfprintf(stdout,
                      "%s    size increases / decreases     = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->size_increases[i]),
                      (long)(cache_ptr->size_decreases[i]));

            HDfprintf(stdout,
                      "%s    entry/cache flush size changes = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->entry_flush_size_changes[i]),
                      (long)(cache_ptr->cache_flush_size_changes[i]));


            HDfprintf(stdout,
                      "%s    entry pins / unpins            = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->pins[i]),
                      (long)(cache_ptr->unpins[i]));

            HDfprintf(stdout,
                      "%s    entry dirty pins/pin'd flushes = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->dirty_pins[i]),
                      (long)(cache_ptr->pinned_flushes[i]));

#if H5C_COLLECT_CACHE_ENTRY_STATS

            HDfprintf(stdout,
                      "%s    entry max / min accesses       = %d / %d\n",
                      cache_ptr->prefix,
                      cache_ptr->max_accesses[i],
                      cache_ptr->min_accesses[i]);

            HDfprintf(stdout,
                      "%s    entry max_clears / max_flushes = %d / %d\n",
                      cache_ptr->prefix,
                      cache_ptr->max_clears[i],
                      cache_ptr->max_flushes[i]);

            HDfprintf(stdout,
                      "%s    entry max_size / max_pins      = %d / %d\n",
                      cache_ptr->prefix,
                      (int)(cache_ptr->max_size[i]),
		      (int)(cache_ptr->max_pins[i]));


#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

        } /* end for */
    } /* end if */

    HDfprintf(stdout, "\n");

#endif /* H5C_COLLECT_CACHE_STATS */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_stats() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_stats__reset
 *
 * Purpose:     Reset the stats fields to their initial values.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer, 4/28/04
 *
 *		JRM 11/13/08
 *		Added initialization for the new max_clean_index_size and
 *		max_dirty_index_size fields.
 *
 *              MAM -- 01/06/09
 *              Added code to initalize the calls_to_msic,
 *              total_entries_skipped_in_msic, total_entries_scanned_in_msic,
 *              and max_entries_skipped_in_msic fields.
 *
 *		JRM 4/11/15
 *		Added code to initialize the new slist_scan_restarts,
 *		LRU_scan_restarts, hash_bucket_scan_restarts, and 
 *		take_ownerships fields.
 *
 *-------------------------------------------------------------------------
 */
void
#ifndef NDEBUG
H5C_stats__reset(H5C_t * cache_ptr)
#else /* NDEBUG */
#if H5C_COLLECT_CACHE_STATS
H5C_stats__reset(H5C_t * cache_ptr)
#else /* H5C_COLLECT_CACHE_STATS */
H5C_stats__reset(H5C_t H5_ATTR_UNUSED * cache_ptr)
#endif /* H5C_COLLECT_CACHE_STATS */
#endif /* NDEBUG */
{
#if H5C_COLLECT_CACHE_STATS
    int i;
#endif /* H5C_COLLECT_CACHE_STATS */

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

#if H5C_COLLECT_CACHE_STATS
    for(i = 0; i <= cache_ptr->max_type_id; i++) {
        cache_ptr->hits[i]			= 0;
        cache_ptr->misses[i]			= 0;
        cache_ptr->write_protects[i]		= 0;
        cache_ptr->read_protects[i]		= 0;
        cache_ptr->max_read_protects[i]		= 0;
        cache_ptr->insertions[i]		= 0;
        cache_ptr->pinned_insertions[i]		= 0;
        cache_ptr->clears[i]			= 0;
        cache_ptr->flushes[i]			= 0;
        cache_ptr->evictions[i]	 		= 0;
        cache_ptr->take_ownerships[i] 		= 0;
        cache_ptr->moves[i]	 		= 0;
        cache_ptr->entry_flush_moves[i]		= 0;
        cache_ptr->cache_flush_moves[i]		= 0;
        cache_ptr->pins[i]	 		= 0;
        cache_ptr->unpins[i]	 		= 0;
        cache_ptr->dirty_pins[i]	 	= 0;
        cache_ptr->pinned_flushes[i]	 	= 0;
        cache_ptr->pinned_clears[i]	 	= 0;
        cache_ptr->size_increases[i] 		= 0;
        cache_ptr->size_decreases[i] 		= 0;
	cache_ptr->entry_flush_size_changes[i]	= 0;
	cache_ptr->cache_flush_size_changes[i]	= 0;
    } /* end for */

    cache_ptr->total_ht_insertions		= 0;
    cache_ptr->total_ht_deletions		= 0;
    cache_ptr->successful_ht_searches		= 0;
    cache_ptr->total_successful_ht_search_depth	= 0;
    cache_ptr->failed_ht_searches		= 0;
    cache_ptr->total_failed_ht_search_depth	= 0;

    cache_ptr->max_index_len			= 0;
    cache_ptr->max_index_size			= (size_t)0;
    cache_ptr->max_clean_index_size		= (size_t)0;
    cache_ptr->max_dirty_index_size		= (size_t)0;

    cache_ptr->max_slist_len			= 0;
    cache_ptr->max_slist_size			= (size_t)0;

    cache_ptr->max_pl_len			= 0;
    cache_ptr->max_pl_size			= (size_t)0;

    cache_ptr->max_pel_len			= 0;
    cache_ptr->max_pel_size			= (size_t)0;

    cache_ptr->calls_to_msic                    = 0;
    cache_ptr->total_entries_skipped_in_msic    = 0;
    cache_ptr->total_entries_scanned_in_msic    = 0;
    cache_ptr->max_entries_skipped_in_msic      = 0;
    cache_ptr->max_entries_scanned_in_msic      = 0;
    cache_ptr->entries_scanned_to_make_space    = 0;

    cache_ptr->slist_scan_restarts		= 0;
    cache_ptr->LRU_scan_restarts		= 0;
    cache_ptr->hash_bucket_scan_restarts	= 0;

#if H5C_COLLECT_CACHE_ENTRY_STATS
    for(i = 0; i <= cache_ptr->max_type_id; i++) {
        cache_ptr->max_accesses[i]		= 0;
        cache_ptr->min_accesses[i]		= 1000000;
        cache_ptr->max_clears[i]		= 0;
        cache_ptr->max_flushes[i]		= 0;
        cache_ptr->max_size[i]			= (size_t)0;
        cache_ptr->max_pins[i]			= 0;
    } /* end for */

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
#endif /* H5C_COLLECT_CACHE_STATS */

} /* H5C_stats__reset() */

extern void
H5C__dump_entry(H5C_t *cache_ptr, const H5C_cache_entry_t *entry_ptr,
    hbool_t dump_parents, const char *prefix, int indent);

static void
H5C__dump_parents(H5C_t *cache_ptr, const H5C_cache_entry_t *entry_ptr, const char *prefix, int indent)
{
    unsigned u;

    for(u = 0; u < entry_ptr->flush_dep_nparents; u++)
        H5C__dump_entry(cache_ptr, entry_ptr->flush_dep_parent[u], TRUE, prefix, indent + 2);
}

typedef struct H5C__dump_child_ctx_t {
    H5C_t *cache_ptr;
    const H5C_cache_entry_t *parent;
    hbool_t dump_parents;
    const char *prefix;
    int indent;
} H5C__dump_child_ctx_t;

static int
H5C__dump_children_cb(H5C_cache_entry_t *entry_ptr, void *_ctx)
{
    H5C__dump_child_ctx_t *ctx = (H5C__dump_child_ctx_t *)_ctx;

    if(entry_ptr->tag != entry_ptr->addr) {
        unsigned u;

        HDassert(entry_ptr->flush_dep_nparents);
        for(u = 0; u < entry_ptr->flush_dep_nparents; u++)
            if(ctx->parent == entry_ptr->flush_dep_parent[u])
                H5C__dump_entry(ctx->cache_ptr, entry_ptr, ctx->dump_parents, ctx->prefix, ctx->indent + 2);
    } /* end if */

    return(H5_ITER_CONT);
}

static void
H5C__dump_children(H5C_t *cache_ptr, const H5C_cache_entry_t *entry_ptr,
    hbool_t dump_parents, const char *prefix, int indent)
{
    H5C__dump_child_ctx_t ctx;

    ctx.cache_ptr = cache_ptr;
    ctx.parent = entry_ptr;
    ctx.dump_parents = dump_parents;
    ctx.prefix = prefix;
    ctx.indent = indent;
    H5C__iter_tagged_entries(cache_ptr, entry_ptr->tag, FALSE, H5C__dump_children_cb, &ctx);
}

void
H5C__dump_entry(H5C_t *cache_ptr, const H5C_cache_entry_t *entry_ptr,
    hbool_t dump_parents, const char *prefix, int indent)
{
    HDassert(cache_ptr);
    HDassert(entry_ptr);

    HDfprintf(stderr, "%*s%s: entry_ptr = (%a, '%s', %a, %t, %u, %u/%u)\n", indent, "", prefix, entry_ptr->addr, entry_ptr->type->name, entry_ptr->tag, entry_ptr->is_dirty, entry_ptr->flush_dep_nparents, entry_ptr->flush_dep_nchildren, entry_ptr->flush_dep_ndirty_children);
    if(dump_parents && entry_ptr->flush_dep_nparents)
        H5C__dump_parents(cache_ptr, entry_ptr, "Parent", indent);
    if(entry_ptr->flush_dep_nchildren)
        H5C__dump_children(cache_ptr, entry_ptr, FALSE, "Child", indent);
}
