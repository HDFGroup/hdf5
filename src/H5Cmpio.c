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
 * Created:     H5Cmpio.c
 *              June 20 2015
 *              Quincey Koziol
 *
 * Purpose:     Functions in this file implement support for parallel I/O for
 *		generic cache code.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Cmodule.h"          /* This source code file is part of the H5C module */
#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"        /* Metadata cache                       */
#include "H5Cpkg.h"		/* Cache				*/
#include "H5Dprivate.h"		/* Datasets     		  	*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property lists                       */


#ifdef H5_HAVE_PARALLEL
/****************/
/* Local Macros */
/****************/
#define H5C_APPLY_CANDIDATE_LIST__DEBUG 0


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/
static herr_t H5C__collective_write(H5F_t *f, hid_t dxpl_id);


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
 * Function:    H5C_apply_candidate_list
 *
 * Purpose:     Apply the supplied candidate list.
 *
 *		We used to do this by simply having each process write 
 *		every mpi_size-th entry in the candidate list, starting 
 *		at index mpi_rank, and mark all the others clean.  
 *
 *		However, this can cause unnecessary contention in a file 
 *		system by increasing the number of processes writing to 
 *		adjacent locations in the HDF5 file.
 *
 *		To attempt to minimize this, we now arange matters such 
 *		that each process writes n adjacent entries in the 
 *		candidate list, and marks all others clean.  We must do
 *		this in such a fashion as to guarantee that each entry 
 *		on the candidate list is written by exactly one process, 
 *		and marked clean by all others.  
 *
 *		To do this, first construct a table mapping mpi_rank
 *		to the index of the first entry in the candidate list to
 *		be written by the process of that mpi_rank, and then use
 *		the table to control which entries are written and which
 *		are marked as clean as a function of the mpi_rank.
 *
 *		Note that the table must be identical on all processes, as
 *		all see the same candidate list, mpi_size, and mpi_rank --
 *		the inputs used to construct the table.  
 *
 *		We construct the table as follows.  Let:
 *
 *			n = num_candidates / mpi_size;
 *
 *			m = num_candidates % mpi_size;
 *
 *		Now allocate an array of integers of length mpi_size + 1, 
 *		and call this array candidate_assignment_table. 
 *
 *		Conceptually, if the number of candidates is a multiple
 *		of the mpi_size, we simply pass through the candidate list
 *		and assign n entries to each process to flush, with the 
 *		index of the first entry to flush in the location in 
 *		the candidate_assignment_table indicated by the mpi_rank
 *		of the process.  
 *
 *		In the more common case in which the candidate list isn't 
 *		isn't a multiple of the mpi_size, we pretend it is, and 
 *		give num_candidates % mpi_size processes one extra entry
 *		each to make things work out.
 *
 *		Once the table is constructed, we determine the first and
 *		last entry this process is to flush as follows:
 *
 *	 	first_entry_to_flush = candidate_assignment_table[mpi_rank]
 *
 *		last_entry_to_flush = 
 *			candidate_assignment_table[mpi_rank + 1] - 1;
 *		
 *		With these values determined, we simply scan through the 
 *		candidate list, marking all entries in the range 
 *		[first_entry_to_flush, last_entry_to_flush] for flush,
 *		and all others to be cleaned.
 *
 *		Finally, we scan the LRU from tail to head, flushing 
 *		or marking clean the candidate entries as indicated.
 *		If necessary, we scan the pinned list as well.
 *
 *		Note that this function will fail if any protected or 
 *		clean entries appear on the candidate list.
 *
 *		This function is used in managing sync points, and 
 *		shouldn't be used elsewhere.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/17/10
 *
 * Changes:     Ported code to detect next entry status changes as the 
 *              the result of a flush from the serial code in the scan of 
 *              the LRU.  Also added code to detect and adapt to the 
 *              removal from the cache of the next entry in the scan of 
 *		the LRU.
 *
 *		Note that at present, all of these changes should not 
 *		be required as the operations on entries as they are 
 *		flushed that can cause these condiditions are not premitted
 *		in the parallel case.  However, Quincey indicates that 
 *		this may change, and thus has requested the modification.
 *
 *		Note the assert(FALSE) in the if statement whose body 
 *		restarts the scan of the LRU.  As the body of the if 
 *		statement should be unreachable, it should never be 
 *		triggered until the constraints on the parallel case 
 *		are relaxed.  Please remove the assertion at that time.
 *
 *		Also added warning on the Pinned Entry List scan, as it
 *		is potentially subject to the same issue.  As there is 
 *		no cognate of this scan in the serial code, I don't have
 *		a fix to port to it.
 *
 *						JRM -- 4/10/19
 *		
 *-------------------------------------------------------------------------
 */
herr_t
H5C_apply_candidate_list(H5F_t * f,
                         hid_t dxpl_id,
                         H5C_t * cache_ptr,
                         int num_candidates,
                         haddr_t * candidates_list_ptr,
                         int mpi_rank,
                         int mpi_size)
{
    hbool_t		restart_scan;
    hbool_t		prev_is_dirty;
    int                 i;
    int			m;
    int			n;
    int			first_entry_to_flush;
    int			last_entry_to_flush;
    int			entries_to_clear = 0;
    int			entries_to_flush = 0;
    int			entries_to_flush_or_clear_last = 0;
    int			entries_to_flush_collectively = 0;
    int			entries_cleared = 0;
    int			entries_flushed = 0;
    int			entries_delayed = 0;
    int			entries_flushed_or_cleared_last = 0;
    int			entries_flushed_collectively = 0;
    int			entries_examined = 0;
    int			initial_list_len;
    int               * candidate_assignment_table = NULL;
    haddr_t		addr;
    H5C_cache_entry_t *	clear_ptr = NULL;
    H5C_cache_entry_t *	next_ptr = NULL;
    H5C_cache_entry_t *	entry_ptr = NULL;
    H5C_cache_entry_t *	flush_ptr = NULL;
    H5C_cache_entry_t * delayed_ptr = NULL;
#if H5C_DO_SANITY_CHECKS
    haddr_t		last_addr;
#endif /* H5C_DO_SANITY_CHECKS */
#if H5C_APPLY_CANDIDATE_LIST__DEBUG
    char		tbl_buf[1024];
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( num_candidates > 0 );
    HDassert( num_candidates <= cache_ptr->slist_len );
    HDassert( candidates_list_ptr != NULL );
    HDassert( 0 <= mpi_rank );
    HDassert( mpi_rank < mpi_size );

#if H5C_APPLY_CANDIDATE_LIST__DEBUG
    HDfprintf(stdout, "%s:%d: setting up candidate assignment table.\n", 
              FUNC, mpi_rank);
    for ( i = 0; i < 1024; i++ ) tbl_buf[i] = '\0';
    sprintf(&(tbl_buf[0]), "candidate list = ");
    for ( i = 0; i < num_candidates; i++ )
    {
        sprintf(&(tbl_buf[HDstrlen(tbl_buf)]), " 0x%llx", 
                (long long)(*(candidates_list_ptr + i)));
    }
    sprintf(&(tbl_buf[HDstrlen(tbl_buf)]), "\n");
    HDfprintf(stdout, "%s", tbl_buf);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

    if(f->coll_md_write) {
        /* Sanity check */
        HDassert(NULL == cache_ptr->coll_write_list);

        /* Create skip list of entries for collective write */
        if(NULL == (cache_ptr->coll_write_list = H5SL_create(H5SL_TYPE_HADDR, NULL)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create skip list for entries")
    } /* end if */

    n = num_candidates / mpi_size;
    m = num_candidates % mpi_size;
    HDassert(n >= 0);

    if(NULL == (candidate_assignment_table = (int *)H5MM_malloc(sizeof(int) * (size_t)(mpi_size + 1))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for candidate assignment table")

    candidate_assignment_table[0] = 0;
    candidate_assignment_table[mpi_size] = num_candidates;

    if(m == 0) { /* mpi_size is an even divisor of num_candidates */
        HDassert(n > 0);
        for(i = 1; i < mpi_size; i++)
            candidate_assignment_table[i] = candidate_assignment_table[i - 1] + n;
    } /* end if */
    else { 
        for(i = 1; i <= m; i++)
            candidate_assignment_table[i] = candidate_assignment_table[i - 1] + n + 1;

        if(num_candidates < mpi_size) {
            for(i = m + 1; i < mpi_size; i++)
                candidate_assignment_table[i] = num_candidates;
        } /* end if */
        else {
            for(i = m + 1; i < mpi_size; i++)
                candidate_assignment_table[i] = candidate_assignment_table[i - 1] + n;
        } /* end else */
    } /* end else */
    HDassert((candidate_assignment_table[mpi_size - 1] + n) == num_candidates);

#if H5C_DO_SANITY_CHECKS
    /* verify that the candidate assignment table has the expected form */
    for ( i = 1; i < mpi_size - 1; i++ ) 
    {
        int a, b;

        a = candidate_assignment_table[i] - candidate_assignment_table[i - 1];
        b = candidate_assignment_table[i + 1] - candidate_assignment_table[i];

        HDassert( n + 1 >= a );
        HDassert( a >= b );
        HDassert( b >= n );
    }
#endif /* H5C_DO_SANITY_CHECKS */

    first_entry_to_flush = candidate_assignment_table[mpi_rank];
    last_entry_to_flush = candidate_assignment_table[mpi_rank + 1] - 1;

#if H5C_APPLY_CANDIDATE_LIST__DEBUG
    for ( i = 0; i < 1024; i++ )
        tbl_buf[i] = '\0';
    sprintf(&(tbl_buf[0]), "candidate assignment table = ");
    for(i = 0; i <= mpi_size; i++)
        sprintf(&(tbl_buf[HDstrlen(tbl_buf)]), " %d", candidate_assignment_table[i]);
    sprintf(&(tbl_buf[HDstrlen(tbl_buf)]), "\n");
    HDfprintf(stdout, "%s", tbl_buf);

    HDfprintf(stdout, "%s:%d: flush entries [%d, %d].\n", 
              FUNC, mpi_rank, first_entry_to_flush, last_entry_to_flush);

    HDfprintf(stdout, "%s:%d: marking entries.\n", FUNC, mpi_rank);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

    for(i = 0; i < num_candidates; i++) {
        addr = candidates_list_ptr[i];
        HDassert( H5F_addr_defined(addr) );

#if H5C_DO_SANITY_CHECKS
        if ( i > 0 ) {
            if ( last_addr == addr ) {
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Duplicate entry in cleaned list.\n")
            } else if ( last_addr > addr ) {
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "candidate list not sorted.\n")
            }
        }

        last_addr = addr;
#endif /* H5C_DO_SANITY_CHECKS */

        H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)
        if(entry_ptr == NULL) {
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Listed candidate entry not in cache?!?!?.")
        } else if(!entry_ptr->is_dirty) {
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Listed entry not dirty?!?!?.")
        } else if ( entry_ptr->is_protected ) {
            /* For now at least, we can't deal with protected entries.
             * If we encounter one, scream and die.  If it becomes an
             * issue, we should be able to work around this. 
             */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Listed entry is protected?!?!?.")
        } else {
            /* determine whether the entry is to be cleared or flushed,
             * and mark it accordingly.  We will scan the protected and 
             * pinned list shortly, and clear or flush according to these
             * markings.  
             */
            if((i >= first_entry_to_flush) && (i <= last_entry_to_flush)) {
                entries_to_flush++;
                entry_ptr->flush_immediately = TRUE;
            } /* end if */
            else {
                entries_to_clear++;
                entry_ptr->clear_on_unprotect = TRUE;
            } /* end else */

            /* Entries marked as collectively accessed and are in the
               candidate list to clear from the cache have to be
               removed from the coll list. This is OK since the
               candidate list is collective and uniform across all
               ranks. */
            if(TRUE == entry_ptr->coll_access) {
                entry_ptr->coll_access = FALSE;
                H5C__REMOVE_FROM_COLL_LIST(cache_ptr, entry_ptr, FAIL)
            } /* end if */
        } /* end else */
    } /* end for */

#if H5C_APPLY_CANDIDATE_LIST__DEBUG
    HDfprintf(stdout, "%s:%d: num candidates/to clear/to flush = %d/%d/%d.\n", 
              FUNC, mpi_rank, (int)num_candidates, (int)entries_to_clear,
              (int)entries_to_flush);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */


    /* We have now marked all the entries on the candidate list for 
     * either flush or clear -- now scan the LRU and the pinned list
     * for these entries and do the deed.
     *
     * Note that we are doing things in this round about manner so as
     * to preserve the order of the LRU list to the best of our ability.
     * If we don't do this, my experiments indicate that we will have a
     * noticably poorer hit ratio as a result.
     */

#if H5C_APPLY_CANDIDATE_LIST__DEBUG
    HDfprintf(stdout, "%s:%d: scanning LRU list. len = %d.\n", FUNC, mpi_rank,
              (int)(cache_ptr->LRU_list_len));
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

    /* ===================================================================== *
     * Now scan the LRU and PEL lists, flushing or clearing entries as
     * needed.
     *
     * The flush_me_last flag may dictate how or
     * when some entries can be flushed, and should be addressed here.
     * However, in their initial implementation, these flags only apply to the
     * superblock, so there's only a relatively small change to this function
     * to account for this one case where they come into play. If these flags
     * are ever expanded upon, this function and the following flushing steps
     * should be reworked to account for additional cases.
     * ===================================================================== */

    HDassert(entries_to_flush >= 0);

    restart_scan = FALSE;
    entries_examined = 0;
    initial_list_len = cache_ptr->LRU_list_len;
    entry_ptr = cache_ptr->LRU_tail_ptr;

    /* Examine each entry in the LRU list */
    while ( ( entry_ptr != NULL ) 
            && 
            ( entries_examined <= (entries_to_flush + 1) * initial_list_len ) 
            &&
            ( (entries_cleared + entries_flushed) < num_candidates ) ) {

        if ( entry_ptr->prev != NULL )
            prev_is_dirty = entry_ptr->prev->is_dirty;

        /* If this process needs to clear this entry. */
        if(entry_ptr->clear_on_unprotect) {

            HDassert(entry_ptr->is_dirty);

            next_ptr = entry_ptr->next; 
            entry_ptr->clear_on_unprotect = FALSE;
            clear_ptr = entry_ptr;
            entry_ptr = entry_ptr->prev;
            entries_cleared++;

#if ( H5C_APPLY_CANDIDATE_LIST__DEBUG > 1 )
    HDfprintf(stdout, "%s:%d: clearing 0x%llx.\n", FUNC, mpi_rank,
              (long long)clear_ptr->addr);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

            /* reset entries_removed_counter and
             * last_entry_removed_ptr prior to the call to
             * H5C__flush_single_entry() so that we can spot
             * unexpected removals of entries from the cache,
             * and set the restart_scan flag if proceeding
             * would be likely to cause us to scan an entry
             * that is no longer in the cache.
             *
             * Note that as of this writing (April 2015) this 
             * case cannot occur in the parallel case.  However 
             * Quincey is making noises about changing this, hence 
             * the insertion of this test.
             *
             * Note also that there is no test code to verify 
             * that this code actually works (although similar code
             * in the serial version exists and is tested).  
             * 
             * Implementing a test will likely require implementing
             * flush op like facilities in the parallel tests.  At
             * a guess this will not be terribly painful, but it 
             * will take a bit of time.
             */
            cache_ptr->entries_removed_counter = 0;
            cache_ptr->last_entry_removed_ptr  = NULL;

            if(H5C__flush_single_entry(f, dxpl_id, clear_ptr, H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__GENERATE_IMAGE_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't clear entry.")

            if((cache_ptr->entries_removed_counter > 1) ||
                    (cache_ptr->last_entry_removed_ptr == entry_ptr))
                restart_scan = TRUE;
        } /* end if */

        /* Else, if this process needs to flush this entry. */
        else if (entry_ptr->flush_immediately) {

	    HDassert(entry_ptr->is_dirty);

            next_ptr = entry_ptr->next; 
            entry_ptr->flush_immediately = FALSE;
            flush_ptr = entry_ptr;
            entry_ptr = entry_ptr->prev;
            entries_flushed++;

#if ( H5C_APPLY_CANDIDATE_LIST__DEBUG > 1 )
    HDfprintf(stdout, "%s:%d: flushing 0x%llx.\n", FUNC, mpi_rank,
              (long long)flush_ptr->addr);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

            /* reset entries_removed_counter and
             * last_entry_removed_ptr prior to the call to
             * H5C__flush_single_entry() so that we can spot
             * unexpected removals of entries from the cache,
             * and set the restart_scan flag if proceeding
             * would be likely to cause us to scan an entry
             * that is no longer in the cache.
             *
             * Note that as of this writing (April 2015) this 
             * case cannot occur in the parallel case.  However 
             * Quincey is making noises about changing this, hence 
             * the insertion of this test.
             *
             * Note also that there is no test code to verify 
             * that this code actually works (although similar code
             * in the serial version exists and is tested).  
             * 
             * Implementing a test will likely require implementing
             * flush op like facilities in the parallel tests.  At
             * a guess this will not be terribly painful, but it 
             * will take a bit of time.
             */
            cache_ptr->entries_removed_counter = 0;
            cache_ptr->last_entry_removed_ptr  = NULL;

            /* Add this entry to the list of entries to collectively write */
            if(H5C__flush_single_entry(f, dxpl_id, flush_ptr, H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush entry.")

            if((cache_ptr->entries_removed_counter > 1) ||
                    (cache_ptr->last_entry_removed_ptr == entry_ptr))
                restart_scan = TRUE;
        } /* end else-if */

        /* Otherwise, no action to be taken on this entry. Grab the next. */
        else {
            entry_ptr = entry_ptr->prev;

            if ( entry_ptr != NULL )
                next_ptr = entry_ptr->next; 

        } /* end else */

        if ( ( entry_ptr != NULL ) 
             &&
             ( ( restart_scan )
               ||
               ( entry_ptr->is_dirty != prev_is_dirty )
               ||
               ( entry_ptr->next != next_ptr )
               ||
               ( entry_ptr->is_protected )
               ||
               ( entry_ptr->is_pinned ) 
             ) 
           ) {

            /* something has happened to the LRU -- start over
             * from the tail.
             *
             * Recall that this code should be un-reachable at present,
             * as all the operations by entries on flush that could cause
             * it to be reachable are disallowed in the parallel case at
             * present.  Hence the following assertion which should be 
             * removed if the above changes.
             */

	    HDassert( ! restart_scan );
            HDassert( entry_ptr->is_dirty == prev_is_dirty );
            HDassert( entry_ptr->next == next_ptr );
            HDassert( ! entry_ptr->is_protected );
            HDassert( ! entry_ptr->is_pinned );

            HDassert(FALSE); /* see comment above */

            restart_scan = FALSE;
            entry_ptr = cache_ptr->LRU_tail_ptr;
/* 
	    H5C__UPDATE_STATS_FOR_LRU_SCAN_RESTART(cache_ptr)
*/
        }

        entries_examined++;
    } /* end while */

#if H5C_APPLY_CANDIDATE_LIST__DEBUG
    HDfprintf(stdout, "%s:%d: entries examined/cleared/flushed = %d/%d/%d.\n",
              FUNC, mpi_rank, entries_examined,
              entries_cleared, entries_flushed);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

    /* It is also possible that some of the cleared entries are on the
     * pinned list.  Must scan that also.
     *
     * WARNING:
     *
     *	As we now allow unpinning, and removal of other entries as a side 
     *  effect of flushing an entry, it is possible that the next entry
     *  in a PEL scan could either be no longer pinned, or no longer in
     *  the cache by the time we get to it.
     *
     *  At present, this is not possible in this case, as we disallow such
     *  operations in the parallel version of the library.  However, Quincey
     *  has been making noises about relaxing this.  If and when he does,
     *  we have a potential problem here.
     *
     *  The same issue exists in the serial cache, and there are tests 
     *  to detect this problem when it occurs, and adjust to it.  As seen
     *  above in the LRU scan, I have ported such tests to the parallel 
     *  code where a close cognate exists in the serial code.  
     *
     *  I haven't done so here, as there are no PEL scans where the problem
     *  can occur in the serial code.  Needless to say, this will have to 
     *  be repaired if the constraints on pre_serialize and serialize 
     *  callbacks are relaxed in the parallel version of the metadata cache.
     *
     *						JRM -- 4/1/15
     */

#if H5C_APPLY_CANDIDATE_LIST__DEBUG
    HDfprintf(stdout, "%s:%d: scanning pinned entry list. len = %d\n",
             FUNC, mpi_rank, (int)(cache_ptr->pel_len));
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

    entry_ptr = cache_ptr->pel_head_ptr;
    while((entry_ptr != NULL) &&
          ((entries_cleared + entries_flushed + entries_delayed)
            < num_candidates)) {

        /* If entry is marked for flush or for clear */
        if((entry_ptr->clear_on_unprotect||entry_ptr->flush_immediately)) {

            /* If this entry needs to be flushed last */
            if (entry_ptr->flush_me_last) {

                /* At this time, only the superblock supports being
                   flushed last. Conveniently, it also happens to be the only
                   entry that supports being flushed collectively, as well. Also
                   conveniently, it's always pinned, so we only need to check
                   for it while scanning the PEL here. Finally, it's never
                   included in a candidate list that excludes other dirty
                   entries in a cache, so we can handle this relatively simple
                   case here.
                   
                   For now, this function asserts this and saves the entry
                   to flush it after scanning the rest of the PEL list.

                   If there are ever more entries that either need to be
                   flushed last and/or flushed collectively, this whole routine
                   will need to be reworked to handle all additional cases. As
                   it is the simple case of a single pinned entry needing
                   flushed last and collectively is just a minor addition to
                   this routine, but signficantly buffing up the usage of
                   flush_me_last will require a more
                   intense rework of this function and potentially the function
                   of candidate lists as a whole. */

                entries_to_flush_or_clear_last++;
                entries_to_flush_collectively++;
                HDassert(entries_to_flush_or_clear_last == 1);
                HDassert(entries_to_flush_collectively == 1);

                /* Delay the entry. It will be flushed later. */
                delayed_ptr = entry_ptr;
                entries_delayed++;
                HDassert(entries_delayed == 1);

            } /* end if */

            /* Else, this process needs to clear this entry. */
            else if (entry_ptr->clear_on_unprotect) {
                HDassert(!entry_ptr->flush_immediately);
                entry_ptr->clear_on_unprotect = FALSE;
                clear_ptr = entry_ptr;
                entry_ptr = entry_ptr->next;
                entries_cleared++;

#if ( H5C_APPLY_CANDIDATE_LIST__DEBUG > 1 )
                HDfprintf(stdout, "%s:%d: clearing 0x%llx.\n", FUNC, mpi_rank,
                          (long long)clear_ptr->addr);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

                if(H5C__flush_single_entry(f, dxpl_id, clear_ptr, H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__GENERATE_IMAGE_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't clear entry.")
            } /* end else-if */

            /* Else, if this process needs to independently flush this entry. */
            else if (entry_ptr->flush_immediately) {
                entry_ptr->flush_immediately = FALSE;
                flush_ptr = entry_ptr;
                entry_ptr = entry_ptr->next;
                entries_flushed++;

#if ( H5C_APPLY_CANDIDATE_LIST__DEBUG > 1 )
                HDfprintf(stdout, "%s:%d: flushing 0x%llx.\n", FUNC, mpi_rank,
                      (long long)flush_ptr->addr);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

                /* Add this entry to the list of entries to collectively write */
                if(H5C__flush_single_entry(f, dxpl_id, flush_ptr, H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't clear entry.")
            } /* end else-if */
        } /* end if */

        /* Otherwise, this entry is not marked for flush or clear. Grab the next. */
        else {
            entry_ptr = entry_ptr->next;
        } /* end else */

    } /* end while */

#if H5C_APPLY_CANDIDATE_LIST__DEBUG
    HDfprintf(stdout,
              "%s:%d: pel entries examined/cleared/flushed = %d/%d/%d.\n",
              FUNC, mpi_rank, entries_examined,
              entries_cleared, entries_flushed);
    HDfprintf(stdout, "%s:%d: done.\n", FUNC, mpi_rank);

    HDfsync(stdout);
#endif /* H5C_APPLY_CANDIDATE_LIST__DEBUG */

    /* ====================================================================== *
     * Now, handle all delayed entries.                                       *
     *                                                                        *
     * This can *only* be the superblock at this time, so it's relatively     *
     * easy to deal with. We're collectively flushing the entry saved from    *
     * above. This will need to be handled differently if there are ever more *
     * than one entry needing this special treatment.)                        *
     * ====================================================================== */

    if (delayed_ptr) {

        if (delayed_ptr->clear_on_unprotect) {
            if(H5C__flush_single_entry(f, dxpl_id, delayed_ptr, H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__GENERATE_IMAGE_FLAG) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush entry.")

            entry_ptr->clear_on_unprotect = FALSE;
            entries_cleared++;
        } else if (delayed_ptr->flush_immediately) {
            /* Add this entry to the list of entries to collectively write */
            if(H5C__flush_single_entry(f, dxpl_id, delayed_ptr, H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush entry collectively.")

            entry_ptr->flush_immediately = FALSE;
            entries_flushed++;
        } /* end if */

        entries_flushed_collectively++;
        entries_flushed_or_cleared_last++;
    } /* end if */

    /* If we've deferred writing to do it collectively, take care of that now */
    if(f->coll_md_write) {
        /* Sanity check */
        HDassert(cache_ptr->coll_write_list);

        /* Write collective list */
        if(H5C__collective_write(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_WRITEERROR, FAIL, "Can't write metadata collectively")
    } /* end if */

    /* ====================================================================== *
     * Finished flushing everything.                                          *
     * ====================================================================== */

    HDassert((entries_flushed == entries_to_flush));
    HDassert((entries_cleared == entries_to_clear));
    HDassert((entries_flushed_or_cleared_last == entries_to_flush_or_clear_last));
    HDassert((entries_flushed_collectively == entries_to_flush_collectively));
    
    if((entries_flushed != entries_to_flush) || 
           (entries_cleared != entries_to_clear) ||
           (entries_flushed_or_cleared_last != entries_to_flush_or_clear_last) ||
           (entries_flushed_collectively != entries_to_flush_collectively))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "entry count mismatch.")

done:
    if(candidate_assignment_table != NULL)
        candidate_assignment_table = (int *)H5MM_xfree((void *)candidate_assignment_table);

    if(cache_ptr->coll_write_list) {
        if(H5SL_close(cache_ptr->coll_write_list) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "failed to destroy skip list")
        cache_ptr->coll_write_list = NULL;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_apply_candidate_list() */


/*-------------------------------------------------------------------------
 * Function:    H5C_construct_candidate_list__clean_cache
 *
 * Purpose:     Construct the list of entries that should be flushed to 
 *		clean all entries in the cache.
 *
 *		This function is used in managing sync points, and 
 *		shouldn't be used elsewhere.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/17/10
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_construct_candidate_list__clean_cache(H5C_t * cache_ptr)
{
    size_t              space_needed;
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    /* As a sanity check, set space needed to the size of the skip list.
     * This should be the sum total of the sizes of all the dirty entries
     * in the metadata cache.
     */
    space_needed = cache_ptr->slist_size;

    /* Recall that while we shouldn't have any protected entries at this
     * point, it is possible that some dirty entries may reside on the
     * pinned list at this point.
     */
    HDassert( cache_ptr->slist_size <= 
              (cache_ptr->dLRU_list_size + cache_ptr->pel_size) );
    HDassert( cache_ptr->slist_len  <= 
              (cache_ptr->dLRU_list_len + cache_ptr->pel_len) );

    if(space_needed > 0) { /* we have work to do */
        H5C_cache_entry_t *entry_ptr;
        int     nominated_entries_count = 0;
        size_t  nominated_entries_size = 0;
        haddr_t	nominated_addr;

        HDassert( cache_ptr->slist_len > 0 );

        /* Scan the dirty LRU list from tail forward and nominate sufficient
         * entries to free up the necessary space. 
         */
        entry_ptr = cache_ptr->dLRU_tail_ptr;
        while((nominated_entries_size < space_needed) &&
                (nominated_entries_count < cache_ptr->slist_len) &&
                (entry_ptr != NULL)) {
            HDassert( ! (entry_ptr->is_protected) );
            HDassert( ! (entry_ptr->is_read_only) );
            HDassert( entry_ptr->ro_ref_count == 0 );
            HDassert( entry_ptr->is_dirty );
            HDassert( entry_ptr->in_slist );

            nominated_addr = entry_ptr->addr;
            if(H5AC_add_candidate((H5AC_t *)cache_ptr, nominated_addr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_add_candidate() failed(1).")

            nominated_entries_size += entry_ptr->size;
            nominated_entries_count++;
            entry_ptr = entry_ptr->aux_prev;
        } /* end while */
        HDassert( entry_ptr == NULL );

        /* it is possible that there are some dirty entries on the 
         * protected entry list as well -- scan it too if necessary
         */
        entry_ptr = cache_ptr->pel_head_ptr;
        while((nominated_entries_size < space_needed) &&
                (nominated_entries_count < cache_ptr->slist_len) &&
                (entry_ptr != NULL)) {
            if(entry_ptr->is_dirty) {
                HDassert( ! (entry_ptr->is_protected) );
                HDassert( ! (entry_ptr->is_read_only) );
                HDassert( entry_ptr->ro_ref_count == 0 );
                HDassert( entry_ptr->is_dirty );
                HDassert( entry_ptr->in_slist );

                nominated_addr = entry_ptr->addr;
                if(H5AC_add_candidate((H5AC_t *)cache_ptr, nominated_addr) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_add_candidate() failed(2).")

                nominated_entries_size += entry_ptr->size;
                nominated_entries_count++;
            } /* end if */

            entry_ptr = entry_ptr->next;
        } /* end while */

        HDassert( nominated_entries_count == cache_ptr->slist_len );
        HDassert( nominated_entries_size == space_needed );
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_construct_candidate_list__clean_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5C_construct_candidate_list__min_clean
 *
 * Purpose:     Construct the list of entries that should be flushed to 
 *		get the cache back within its min clean constraints.
 *
 *		This function is used in managing sync points, and 
 *		shouldn't be used elsewhere.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/17/10
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_construct_candidate_list__min_clean(H5C_t * cache_ptr)
{
    size_t              space_needed = 0;
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    /* compute the number of bytes (if any) that must be flushed to get the 
     * cache back within its min clean constraints.
     */
    if(cache_ptr->max_cache_size > cache_ptr->index_size) {
        if(((cache_ptr->max_cache_size - cache_ptr->index_size) +
               cache_ptr->cLRU_list_size) >= cache_ptr->min_clean_size)
            space_needed = 0;
        else
            space_needed = cache_ptr->min_clean_size -
                ((cache_ptr->max_cache_size - cache_ptr->index_size) +
                 cache_ptr->cLRU_list_size);
    } /* end if */
    else {
        if(cache_ptr->min_clean_size <= cache_ptr->cLRU_list_size)
           space_needed = 0;
        else
            space_needed = cache_ptr->min_clean_size -
                           cache_ptr->cLRU_list_size;
    } /* end else */

    if(space_needed > 0) { /* we have work to do */
        H5C_cache_entry_t *entry_ptr;
        int    nominated_entries_count = 0;
        size_t nominated_entries_size = 0;

        HDassert( cache_ptr->slist_len > 0 );

        /* Scan the dirty LRU list from tail forward and nominate sufficient
         * entries to free up the necessary space. 
         */
        entry_ptr = cache_ptr->dLRU_tail_ptr;
        while((nominated_entries_size < space_needed) &&
                (nominated_entries_count < cache_ptr->slist_len) &&
                (entry_ptr != NULL) &&
                (!entry_ptr->flush_me_last)) {
            haddr_t		nominated_addr;

            HDassert( ! (entry_ptr->is_protected) );
            HDassert( ! (entry_ptr->is_read_only) );
            HDassert( entry_ptr->ro_ref_count == 0 );
            HDassert( entry_ptr->is_dirty );
            HDassert( entry_ptr->in_slist );

            nominated_addr = entry_ptr->addr;
            if(H5AC_add_candidate((H5AC_t *)cache_ptr, nominated_addr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_add_candidate() failed.")

            nominated_entries_size += entry_ptr->size;
            nominated_entries_count++;
            entry_ptr = entry_ptr->aux_prev;
        } /* end while */
        HDassert( nominated_entries_count <= cache_ptr->slist_len );
        HDassert( nominated_entries_size >= space_needed );
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_construct_candidate_list__min_clean() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_mark_entries_as_clean
 *
 * Purpose:     When the H5C code is used to implement the metadata caches
 *		in PHDF5, only the cache with MPI_rank 0 is allowed to
 *		actually write entries to disk -- all other caches must
 *		retain dirty entries until they are advised that the
 *		entries are clean.
 *
 *		This function exists to allow the H5C code to receive these
 *		notifications.
 *
 *		The function receives a list of entry base addresses
 *		which must refer to dirty entries in the cache.  If any
 *		of the entries are either clean or don't exist, the
 *		function flags an error.
 *
 *		The function scans the list of entries and flushes all
 *		those that are currently unprotected with the
 *		H5C__FLUSH_CLEAR_ONLY_FLAG.  Those that are currently
 *		protected are flagged for clearing when they are
 *		unprotected.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              7/5/05
 *
 * Changes:     Tidied up code, removeing some old commented out 
 *		code that had been left in pending success of the 
 *		new version.
 *
 *		Note that unlike H5C_apply_candidate_list(), 
 *		H5C_mark_entries_as_clean() makes all its calls to 
 *		H5C__flush_single_entry() with the 
 *		H5C__FLUSH_CLEAR_ONLY_FLAG set.  As a result, 
 *		the pre_serialize() and serialize calls are not made.
 *
 *		This then implies that (assuming such actions were 
 *		permitted in the parallel case) no loads, dirties, 
 *		resizes, or removals of other entries can occur as 
 *		a side effect of the flush.  Hence, there is no need
 *		for the checks for entry removal / status change 
 *		that I ported to H5C_apply_candidate_list().
 *
 *		However, if (in addition to allowing such operations
 *		in the parallel case), we allow such operations outside
 *		of the pre_serialize / serialize routines, this may 
 *		cease to be the case -- requiring a review of this 
 *		function.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_mark_entries_as_clean(H5F_t *  f,
                          hid_t     dxpl_id,
                          int32_t   ce_array_len,
                          haddr_t * ce_array_ptr)
{
    H5C_t *             cache_ptr;
    int			entries_cleared;
    int			entries_examined;
    int                 i;
    int			initial_list_len;
    haddr_t		addr;
#if H5C_DO_SANITY_CHECKS
    int			pinned_entries_marked = 0;
    int			protected_entries_marked = 0;
    int			other_entries_marked = 0;
    haddr_t		last_addr;
#endif /* H5C_DO_SANITY_CHECKS */
    H5C_cache_entry_t *	clear_ptr = NULL;
    H5C_cache_entry_t *	entry_ptr = NULL;
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( f );
    HDassert( f->shared );
    cache_ptr = f->shared->cache;
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    HDassert( ce_array_len > 0 );
    HDassert( ce_array_ptr != NULL );

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on entry.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    for ( i = 0; i < ce_array_len; i++ )
    {
        addr = ce_array_ptr[i];

#if H5C_DO_SANITY_CHECKS
        if ( i == 0 ) {

            last_addr = addr;

        } else {

            if ( last_addr == addr ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "Duplicate entry in cleaned list.\n");

            } else if ( last_addr > addr ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "cleaned list not sorted.\n");
            }
        }

#if H5C_DO_EXTREME_SANITY_CHECKS
        if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
             ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
             ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "an extreme sanity check failed in for loop.\n");
        }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */
#endif /* H5C_DO_SANITY_CHECKS */

        HDassert( H5F_addr_defined(addr) );

        H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)

        if ( entry_ptr == NULL ) {
#if H5C_DO_SANITY_CHECKS
	    HDfprintf(stdout,
                  "H5C_mark_entries_as_clean: entry[%d] = %ld not in cache.\n",
                      (int)i,
                      (long)addr);
#endif /* H5C_DO_SANITY_CHECKS */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "Listed entry not in cache?!?!?.")

        } else if ( ! entry_ptr->is_dirty ) {

#if H5C_DO_SANITY_CHECKS
	    HDfprintf(stdout,
                      "H5C_mark_entries_as_clean: entry %ld is not dirty!?!\n",
                      (long)addr);
#endif /* H5C_DO_SANITY_CHECKS */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "Listed entry not dirty?!?!?.")

        } else {

            /* Mark the entry to be cleared on unprotect.  We will
             * scan the LRU list shortly, and clear all those entries
             * not currently protected.
             */

            /* Make sure first that we clear the collective flag from
               it so it can be cleared */
            if(TRUE == entry_ptr->coll_access) {
                entry_ptr->coll_access = FALSE;
                H5C__REMOVE_FROM_COLL_LIST(cache_ptr, entry_ptr, FAIL)
            } /* end if */

            entry_ptr->clear_on_unprotect = TRUE;
#if H5C_DO_SANITY_CHECKS
	    if ( entry_ptr->is_protected ) {

		protected_entries_marked++;

	    } else if ( entry_ptr->is_pinned ) {

		pinned_entries_marked++;

	    } else {

		other_entries_marked++;
	    }
#endif /* H5C_DO_SANITY_CHECKS */
        }
    }

    /* Scan through the LRU list from back to front, and flush the
     * entries whose clear_on_unprotect flags are set.  Observe that
     * any protected entries will not be on the LRU, and therefore
     * will not be flushed at this time.
     *
     * Note that unlike H5C_apply_candidate_list(), 
     * H5C_mark_entries_as_clean() makes all its calls to 
     * H5C__flush_single_entry() with the H5C__FLUSH_CLEAR_ONLY_FLAG 
     * set.  As a result, the pre_serialize() and serialize calls are 
     * not made.
     *
     * This then implies that (assuming such actions were 
     * permitted in the parallel case) no loads, dirties, 
     * resizes, or removals of other entries can occur as 
     * a side effect of the flush.  Hence, there is no need
     * for the checks for entry removal / status change 
     * that I ported to H5C_apply_candidate_list().
     *
     * However, if (in addition to allowing such operations
     * in the parallel case), we allow such operations outside
     * of the pre_serialize / serialize routines, this may 
     * cease to be the case -- requiring a review of this 
     * point.
     *					JRM -- 4/7/15
     */

    entries_cleared = 0;
    entries_examined = 0;
    initial_list_len = cache_ptr->LRU_list_len;
    entry_ptr = cache_ptr->LRU_tail_ptr;

    while ( ( entry_ptr != NULL ) &&
            ( entries_examined <= initial_list_len ) &&
            ( entries_cleared < ce_array_len ) )
    {
        if ( entry_ptr->clear_on_unprotect ) {

            entry_ptr->clear_on_unprotect = FALSE;
            clear_ptr = entry_ptr;
            entry_ptr = entry_ptr->prev;
            entries_cleared++;

            if(H5C__flush_single_entry(f, dxpl_id, clear_ptr, H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't clear entry.")
        } else {

            entry_ptr = entry_ptr->prev;
        }
        entries_examined++;
    }

#if H5C_DO_SANITY_CHECKS
    HDassert( entries_cleared == other_entries_marked );
#endif /* H5C_DO_SANITY_CHECKS */

    /* It is also possible that some of the cleared entries are on the
     * pinned list.  Must scan that also.
     */

    entry_ptr = cache_ptr->pel_head_ptr;

    while ( entry_ptr != NULL )
    {
        if ( entry_ptr->clear_on_unprotect ) {

            entry_ptr->clear_on_unprotect = FALSE;
            clear_ptr = entry_ptr;
            entry_ptr = entry_ptr->next;
            entries_cleared++;

            if(H5C__flush_single_entry(f, dxpl_id, clear_ptr, H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) < 0 )
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't clear entry.")
        } else {

            entry_ptr = entry_ptr->next;
        }
    }

#if H5C_DO_SANITY_CHECKS
    HDassert( entries_cleared == pinned_entries_marked + other_entries_marked );
    HDassert( entries_cleared + protected_entries_marked == ce_array_len );
#endif /* H5C_DO_SANITY_CHECKS */

    HDassert( ( entries_cleared == ce_array_len ) ||
              ( (ce_array_len - entries_cleared) <= cache_ptr->pl_len ) );

#if H5C_DO_SANITY_CHECKS
    i = 0;
    entry_ptr = cache_ptr->pl_head_ptr;
    while ( entry_ptr != NULL )
    {
        if ( entry_ptr->clear_on_unprotect ) {

            i++;
        }
        entry_ptr = entry_ptr->next;
    }
    HDassert( (entries_cleared + i) == ce_array_len );
#endif /* H5C_DO_SANITY_CHECKS */

done:

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on exit.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_mark_entries_as_clean() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_clear_coll_entries
 *
 * Purpose:     Clear half or the entire list of collective entries and 
 *              mark them as independent.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_clear_coll_entries(H5C_t *cache_ptr, hbool_t partial)
{ 
    int32_t		clear_cnt;
    H5C_cache_entry_t *	entry_ptr = NULL;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    entry_ptr = cache_ptr->coll_tail_ptr;
    clear_cnt = (partial ? cache_ptr->coll_list_len / 2 : cache_ptr->coll_list_len);
    while(entry_ptr && clear_cnt > 0) {
        H5C_cache_entry_t *prev_ptr = entry_ptr->coll_prev;

        /* Sanity check */
        HDassert(entry_ptr->coll_access);

        /* Mark entry as independent */
        entry_ptr->coll_access = FALSE;
        H5C__REMOVE_FROM_COLL_LIST(cache_ptr, entry_ptr, FAIL)

        /* Decrement entry count */
        clear_cnt--;

        /* Advance to next entry */
        entry_ptr = prev_ptr;
    } /* end while */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_clear_coll_entries */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__collective_write
 *
 * Purpose:     Perform a collective write of a list of metadata entries.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__collective_write(H5F_t *f, hid_t dxpl_id)
{
    H5AC_t              *cache_ptr;
    H5P_genplist_t      *plist = NULL;
    H5FD_mpio_xfer_t    orig_xfer_mode = H5FD_MPIO_COLLECTIVE;
    int                 count;
    int                 *length_array = NULL;
    MPI_Aint            *buf_array = NULL;
    MPI_Aint            *offset_array = NULL;
    MPI_Datatype        btype;
    hbool_t             btype_created = FALSE;
    MPI_Datatype        ftype;
    hbool_t             ftype_created = FALSE;
    int                 mpi_code;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->coll_write_list != NULL);

    /* Get original transfer mode */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list")
    if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, &orig_xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI-I/O property")

    /* Get number of entries in collective write list */
    count = (int)H5SL_count(cache_ptr->coll_write_list);

    if(count > 0) {
        H5FD_mpio_xfer_t    xfer_mode = H5FD_MPIO_COLLECTIVE;
        H5SL_node_t         *node;
        H5C_cache_entry_t   *entry_ptr;
        void                *base_buf;
        int                 i;

        if(H5P_set(plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI-I/O property")

        /* Allocate arrays */
        if(NULL == (length_array = (int *)H5MM_malloc((size_t)count * sizeof(int))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for collective write table length array")
        if(NULL == (buf_array = (MPI_Aint *)H5MM_malloc((size_t)count * sizeof(MPI_Aint))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for collective buf table length array")
        if(NULL == (offset_array = (MPI_Aint *)H5MM_malloc((size_t)count * sizeof(MPI_Aint))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for collective offset table length array")

        /* Fill arrays */
        node = H5SL_first(cache_ptr->coll_write_list);
        HDassert(node);
        if(NULL == (entry_ptr = (H5C_cache_entry_t *)H5SL_item(node)))
            HGOTO_ERROR(H5E_CACHE, H5E_NOTFOUND, FAIL, "can't retrieve skip list item")

        /* Set up initial array position & buffer base address */
        length_array[0] = (int)entry_ptr->size;
        base_buf = entry_ptr->image_ptr;
        buf_array[0] = (MPI_Aint)0;
        offset_array[0] = (MPI_Aint)entry_ptr->addr;

        node = H5SL_next(node);
        i = 1;
        while(node) {
            if(NULL == (entry_ptr = (H5C_cache_entry_t *)H5SL_item(node)))
                HGOTO_ERROR(H5E_CACHE, H5E_NOTFOUND, FAIL, "can't retrieve skip list item")

            /* Set up array position */
            length_array[i] = (int)entry_ptr->size;
            buf_array[i] = (MPI_Aint)entry_ptr->image_ptr - (MPI_Aint)base_buf;
            offset_array[i] = (MPI_Aint)entry_ptr->addr;

            /* Advance to next node & array location */
            node = H5SL_next(node);
            i++;
        } /* end while */

        /* Create memory MPI type */
        if(MPI_SUCCESS != (mpi_code = MPI_Type_create_hindexed(count, length_array, buf_array, MPI_BYTE, &btype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hindexed failed", mpi_code)
        btype_created = TRUE;
        if(MPI_SUCCESS != (mpi_code = MPI_Type_commit(&btype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

        /* Create file MPI type */
        if(MPI_SUCCESS != (mpi_code = MPI_Type_create_hindexed(count, length_array, offset_array, MPI_BYTE, &ftype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hindexed failed", mpi_code)
        ftype_created = TRUE;
        if(MPI_SUCCESS != (mpi_code = MPI_Type_commit(&ftype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

        /* Pass buf type, file type to the file driver */
        if(H5FD_mpi_setup_collective(dxpl_id, &btype, &ftype) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI-I/O properties")

        /* Write data */
        if(H5F_block_write(f, H5FD_MEM_DEFAULT, (haddr_t)0, (size_t)1, dxpl_id, base_buf) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to write entries collectively")
    } /* end if */
    else {
        MPI_Status mpi_stat;
        MPI_File mpi_fh_p;
        MPI_File mpi_fh;

        if(H5F_get_mpi_handle(f, (MPI_File **)&mpi_fh_p) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get mpi file handle")
        mpi_fh = *(MPI_File*)mpi_fh_p;

        /* just to match up with the 1st MPI_File_set_view from H5FD_mpio_write() */
        if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(mpi_fh, (MPI_Offset)0, MPI_BYTE, MPI_BYTE, "native", MPI_INFO_NULL)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)

        /* just to match up with MPI_File_write_at_all from H5FD_mpio_write() */
        HDmemset(&mpi_stat, 0, sizeof(MPI_Status));
        if(MPI_SUCCESS != (mpi_code = MPI_File_write_at_all(mpi_fh, (MPI_Offset)0, NULL, 0, MPI_BYTE, &mpi_stat)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at_all failed", mpi_code)

        /* just to match up with the 2nd MPI_File_set_view (reset) in H5FD_mpio_write() */
        if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(mpi_fh, (MPI_Offset)0, MPI_BYTE, MPI_BYTE, "native", MPI_INFO_NULL)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)
    } /* end else */

done:
    /* Free arrays */
    length_array = (int *)H5MM_xfree(length_array);
    buf_array = (MPI_Aint *)H5MM_xfree(buf_array);
    offset_array = (MPI_Aint *)H5MM_xfree(offset_array);

    /* Free MPI Types */
    if(btype_created && MPI_SUCCESS != (mpi_code = MPI_Type_free(&btype)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if(ftype_created && MPI_SUCCESS != (mpi_code = MPI_Type_free(&ftype)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    /* Reset dxpl */
    if(orig_xfer_mode != H5FD_MPIO_COLLECTIVE) {
        HDassert(plist);
        if(H5P_set(plist, H5D_XFER_IO_XFER_MODE_NAME, &orig_xfer_mode) < 0)
            HDONE_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI-I/O property")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5C__collective_write() */
#endif /* H5_HAVE_PARALLEL */

