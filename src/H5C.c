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

/*-------------------------------------------------------------------------
 *
 * Created:     H5C.c
 *              June 1 2004
 *              John Mainzer
 *
 * Purpose:     Functions in this file implement a generic cache for
 *              things which exist on disk, and which may be 
 *	 	unambiguously referenced by their disk addresses.
 *
 *              The code in this module was initially written in 
 *		support of a complete re-write of the metadata cache
 *		in H5AC.c  However, other uses for the cache code
 *		suggested themselves, and thus this file was created 
 *		in an attempt to support re-use.
 *
 *		For a detailed overview of the cache, please see the
 *		header comment for H5C_t in this file.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/**************************************************************************
 *
 *				To Do:
 *
 *	Code Changes:
 *
 *	 - Remove extra functionality in H5C_flush_single_entry()?
 *
 *	 - Change protect/unprotect to lock/unlock.
 *
 *	 - Change the way the dirty flag is set.  Probably pass it in
 *	   as a parameter in unprotect & insert.
 *
 *	 - Size should also be passed in as a parameter in insert and
 *	   unprotect -- or some other way should be found to advise the
 *	   cache of changes in entry size.
 *
 *	 - Flush entries in increasing address order in 
 *	   H5C_make_space_in_cache().
 *
 *	 - Also in H5C_make_space_in_cache(), use high and low water marks
 *	   to reduce the number of I/O calls.
 *
 *	 - When flushing, attempt to combine contiguous entries to reduce
 *	   I/O overhead.  Can't do this just yet as some entries are not
 *	   contiguous.  Do this in parallel only or in serial as well?
 *
 *	 - Create MPI type for dirty objects when flushing in parallel.
 *
 *	 - Fix TBBT routines so deletions don't move nodes in memory and
 *         point directly to the TBBT node from the LRU list, eliminating
 *         binary tree lookups when evicting objects from the cache.
 *
 *	Tests:
 *
 *	 - Trim execution time.
 *
 *	 - Add random tests.
 *
 **************************************************************************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5C_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Cprivate.h"		/* Cache				*/
#include "H5Dprivate.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5TBprivate.h"        /* Threaded, Balanced, Binary Trees     */



/****************************************************************************
 *
 * We maintain doubly linked lists of instances of H5C_cache_entry_t for a 
 * variety of reasons -- protected list, LRU list, and the clean and dirty 
 * LRU lists at present.  The following macros support linking and unlinking 
 * of instances of H5C_cache_entry_t by both their regular and auxilary next 
 * and previous pointers.  
 *
 * The size and length fields are also maintained.
 *
 * Note that the relevant pair of prev and next pointers are presumed to be
 * NULL on entry in the insertion macros.
 *
 * Finally, observe that the sanity checking macros evaluate to the empty
 * string when H5C_DO_SANITY_CHECKS is FALSE.  They also contain calls
 * to the HGOTO_ERROR macro, which may not be appropriate in all cases.
 * If so, we will need versions of the insertion and deletion macros which
 * do not reference the sanity checking macros.
 *							JRM - 5/5/04
 *
 ****************************************************************************/

#if H5C_DO_SANITY_CHECKS

#define H5C__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size)   \
if ( ( (head_ptr) == NULL ) ||                                             \
     ( (tail_ptr) == NULL ) ||                                             \
     ( (entry_ptr) == NULL ) ||                                            \
     ( (len) <= 0 ) ||                                                     \
     ( (Size) < (entry_ptr)->size ) ||                                     \
     ( ( (Size) == (entry_ptr)->size ) && ( (len) != 1 ) ) ||              \
     ( ( (entry_ptr)->prev == NULL ) && ( (head_ptr) != (entry_ptr) ) ) || \
     ( ( (entry_ptr)->next == NULL ) && ( (tail_ptr) != (entry_ptr) ) ) || \
     ( ( (len) == 1 ) &&                                                   \
       ( ! ( ( (head_ptr) == (entry_ptr) ) &&                              \
             ( (tail_ptr) == (entry_ptr) ) &&                              \
             ( (entry_ptr)->next == NULL ) &&                              \
             ( (entry_ptr)->prev == NULL ) &&                              \
             ( (Size) == (entry_ptr)->size )                               \
           )                                                               \
       )                                                                   \
     )                                                                     \
   ) {                                                                     \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "DLL pre remove SC failed")   \
}

#define H5C__DLL_SC(head_ptr, tail_ptr, len, Size)                       \
if ( ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&           \
       ( (head_ptr) != (tail_ptr) )                                      \
     ) ||                                                                \
     ( (len) < 0 ) ||                                                    \
     ( (Size) < 0 ) ||                                                   \
     ( ( (len) == 1 ) &&                                                 \
       ( ( (head_ptr) != (tail_ptr) ) || ( (cache_ptr)->size <= 0 ) ||   \
         ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )        \
       )                                                                 \
     ) ||                                                                \
     ( ( (len) >= 1 ) &&                                                 \
       ( ( (head_ptr) == NULL ) || ( (head_ptr)->prev != NULL ) ||       \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->next != NULL )          \
       )                                                                 \
     )                                                                   \
   ) {                                                                   \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "DLL sanity check failed")  \
}

#define H5C__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size) \
if ( ( (entry_ptr) == NULL ) ||                                          \
     ( (entry_ptr)->next != NULL ) ||                                    \
     ( (entry_ptr)->prev != NULL ) ||                                    \
     ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&           \
       ( (head_ptr) != (tail_ptr) )                                      \
     ) ||                                                                \
     ( (len) < 0 ) ||                                                    \
     ( ( (len) == 1 ) &&                                                 \
       ( ( (head_ptr) != (tail_ptr) ) || ( (Size) <= 0 ) ||              \
         ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )        \
       )                                                                 \
     ) ||                                                                \
     ( ( (len) >= 1 ) &&                                                 \
       ( ( (head_ptr) == NULL ) || ( (head_ptr)->prev != NULL ) ||       \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->next != NULL )          \
       )                                                                 \
     )                                                                   \
   ) {                                                                   \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "DLL pre insert SC failed") \
}

#else /* H5C_DO_SANITY_CHECKS */

#define H5C__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size)
#define H5C__DLL_SC(head_ptr, tail_ptr, len, Size)
#define H5C__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size)

#endif /* H5C_DO_SANITY_CHECKS */


#define H5C__DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size)        \
        H5C__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size) \
        if ( (head_ptr) == NULL )                                        \
        {                                                                \
           (head_ptr) = (entry_ptr);                                     \
           (tail_ptr) = (entry_ptr);                                     \
        }                                                                \
        else                                                             \
        {                                                                \
           (tail_ptr)->next = (entry_ptr);                               \
           (entry_ptr)->prev = (tail_ptr);                               \
           (tail_ptr) = (entry_ptr);                                     \
        }                                                                \
        (len)++;                                                         \
        (Size) += (entry_ptr)->size;

#define H5C__DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size)       \
        H5C__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size) \
        if ( (head_ptr) == NULL )                                        \
        {                                                                \
           (head_ptr) = (entry_ptr);                                     \
           (tail_ptr) = (entry_ptr);                                     \
        }                                                                \
        else                                                             \
        {                                                                \
           (head_ptr)->prev = (entry_ptr);                               \
           (entry_ptr)->next = (head_ptr);                               \
           (head_ptr) = (entry_ptr);                                     \
        }                                                                \
        (len)++;                                                         \
        (Size) += entry_ptr->size;

#define H5C__DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size)        \
        H5C__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size) \
        {                                                                \
           if ( (head_ptr) == (entry_ptr) )                              \
           {                                                             \
              (head_ptr) = (entry_ptr)->next;                            \
              if ( (head_ptr) != NULL )                                  \
              {                                                          \
                 (head_ptr)->prev = NULL;                                \
              }                                                          \
           }                                                             \
           else                                                          \
           {                                                             \
              (entry_ptr)->prev->next = (entry_ptr)->next;               \
           }                                                             \
           if ( (tail_ptr) == (entry_ptr) )                              \
           {                                                             \
              (tail_ptr) = (entry_ptr)->prev;                            \
              if ( (tail_ptr) != NULL )                                  \
              {                                                          \
                 (tail_ptr)->next = NULL;                                \
              }                                                          \
           }                                                             \
           else                                                          \
           {                                                             \
              (entry_ptr)->next->prev = (entry_ptr)->prev;               \
           }                                                             \
           entry_ptr->next = NULL;                                       \
           entry_ptr->prev = NULL;                                       \
           (len)--;                                                      \
           (Size) -= entry_ptr->size;                                    \
        }


#if H5C_DO_SANITY_CHECKS

#define H5C__AUX_DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size)   \
if ( ( (head_ptr) == NULL ) ||                                                 \
     ( (tail_ptr) == NULL ) ||                                                 \
     ( (entry_ptr) == NULL ) ||                                                \
     ( (len) <= 0 ) ||                                                         \
     ( (Size) < (entry_ptr)->size ) ||                                         \
     ( ( (Size) == (entry_ptr)->size ) && ( ! ( (len) == 1 ) ) ) ||            \
     ( ( (entry_ptr)->aux_prev == NULL ) && ( (head_ptr) != (entry_ptr) ) ) || \
     ( ( (entry_ptr)->aux_next == NULL ) && ( (tail_ptr) != (entry_ptr) ) ) || \
     ( ( (len) == 1 ) &&                                                       \
       ( ! ( ( (head_ptr) == (entry_ptr) ) && ( (tail_ptr) == (entry_ptr) ) && \
             ( (entry_ptr)->aux_next == NULL ) &&                              \
             ( (entry_ptr)->aux_prev == NULL ) &&                              \
             ( (Size) == (entry_ptr)->size )                                   \
           )                                                                   \
       )                                                                       \
     )                                                                         \
   ) {                                                                         \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "aux DLL pre remove SC failed")   \
}

#define H5C__AUX_DLL_SC(head_ptr, tail_ptr, len, Size)                      \
if ( ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&              \
       ( (head_ptr) != (tail_ptr) )                                         \
     ) ||                                                                   \
     ( (len) < 0 ) ||                                                       \
     ( (Size) < 0 ) ||                                                      \
     ( ( (len) == 1 ) &&                                                    \
       ( ( (head_ptr) != (tail_ptr) ) || ( (Size) <= 0 ) ||                 \
         ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )           \
       )                                                                    \
     ) ||                                                                   \
     ( ( (len) >= 1 ) &&                                                    \
       ( ( (head_ptr) == NULL ) || ( (head_ptr)->aux_prev != NULL ) ||      \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->aux_next != NULL )         \
       )                                                                    \
     )                                                                      \
   ) {                                                                      \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "AUX DLL sanity check failed") \
}

#define H5C__AUX_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size) \
if ( ( (entry_ptr) == NULL ) ||                                              \
     ( (entry_ptr)->aux_next != NULL ) ||                                    \
     ( (entry_ptr)->aux_prev != NULL ) ||                                    \
     ( ( ( (head_ptr) == NULL ) || ( (tail_ptr) == NULL ) ) &&               \
       ( (head_ptr) != (tail_ptr) )                                          \
     ) ||                                                                    \
     ( (len) < 0 ) ||                                                        \
     ( ( (len) == 1 ) &&                                                     \
       ( ( (head_ptr) != (tail_ptr) ) || ( (Size) <= 0 ) ||                  \
         ( (head_ptr) == NULL ) || ( (head_ptr)->size != (Size) )            \
       )                                                                     \
     ) ||                                                                    \
     ( ( (len) >= 1 ) &&                                                     \
       ( ( (head_ptr) == NULL ) || ( (head_ptr)->aux_prev != NULL ) ||       \
         ( (tail_ptr) == NULL ) || ( (tail_ptr)->aux_next != NULL )          \
       )                                                                     \
     )                                                                       \
   ) {                                                                       \
    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "AUX DLL pre insert SC failed") \
}

#else /* H5C_DO_SANITY_CHECKS */

#define H5C__AUX_DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size)
#define H5C__AUX_DLL_SC(head_ptr, tail_ptr, len, Size)
#define H5C__AUX_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size)

#endif /* H5C_DO_SANITY_CHECKS */


#define H5C__AUX_DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size)        \
        H5C__AUX_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size) \
        if ( (head_ptr) == NULL )                                            \
        {                                                                    \
           (head_ptr) = (entry_ptr);                                         \
           (tail_ptr) = (entry_ptr);                                         \
        }                                                                    \
        else                                                                 \
        {                                                                    \
           (tail_ptr)->aux_next = (entry_ptr);                               \
           (entry_ptr)->aux_prev = (tail_ptr);                               \
           (tail_ptr) = (entry_ptr);                                         \
        }                                                                    \
        (len)++;                                                             \
        (Size) += entry_ptr->size;

#define H5C__AUX_DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size)       \
        H5C__AUX_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size) \
        if ( (head_ptr) == NULL )                                            \
        {                                                                    \
           (head_ptr) = (entry_ptr);                                         \
           (tail_ptr) = (entry_ptr);                                         \
        }                                                                    \
        else                                                                 \
        {                                                                    \
           (head_ptr)->aux_prev = (entry_ptr);                               \
           (entry_ptr)->aux_next = (head_ptr);                               \
           (head_ptr) = (entry_ptr);                                         \
        }                                                                    \
        (len)++;                                                             \
        (Size) += entry_ptr->size;

#define H5C__AUX_DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size)        \
        H5C__AUX_DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size) \
        {                                                                    \
           if ( (head_ptr) == (entry_ptr) )                                  \
           {                                                                 \
              (head_ptr) = (entry_ptr)->aux_next;                            \
              if ( (head_ptr) != NULL )                                      \
              {                                                              \
                 (head_ptr)->aux_prev = NULL;                                \
              }                                                              \
           }                                                                 \
           else                                                              \
           {                                                                 \
              (entry_ptr)->aux_prev->aux_next = (entry_ptr)->aux_next;       \
           }                                                                 \
           if ( (tail_ptr) == (entry_ptr) )                                  \
           {                                                                 \
              (tail_ptr) = (entry_ptr)->aux_prev;                            \
              if ( (tail_ptr) != NULL )                                      \
              {                                                              \
                 (tail_ptr)->aux_next = NULL;                                \
              }                                                              \
           }                                                                 \
           else                                                              \
           {                                                                 \
              (entry_ptr)->aux_next->aux_prev = (entry_ptr)->aux_prev;       \
           }                                                                 \
           entry_ptr->aux_next = NULL;                                       \
           entry_ptr->aux_prev = NULL;                                       \
           (len)--;                                                          \
           (Size) -= entry_ptr->size;                                        \
        }


/***********************************************************************
 *
 * Stats collection macros
 *
 * The following macros must handle stats collection when this collection
 * is encabled, and evaluate to the empty string when it is not.
 *
 ***********************************************************************/

#if H5C_COLLECT_CACHE_STATS

#define H5C__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)        \
	(((cache_ptr)->insertions)[(entry_ptr)->type->id])++;        \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )   \
	    (cache_ptr)->max_index_len = (cache_ptr)->index_len;     \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size ) \
	    (cache_ptr)->max_index_size = (cache_ptr)->index_size;

#define H5C__UPDATE_STATS_FOR_RENAME(cache_ptr, entry_ptr) \
	(((cache_ptr)->renames)[(entry_ptr)->type->id])++;

#if H5C_COLLECT_CACHE_ENTRY_STATS

#define H5C__RESET_CACHE_ENTRY_STATS(entry_ptr) \
        (entry_ptr)->accesses = 0;              \
        (entry_ptr)->clears   = 0;              \
        (entry_ptr)->flushes  = 0;

#define H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr) \
	(((cache_ptr)->clears)[(entry_ptr)->type->id])++; \
        ((entry_ptr)->clears)++;

#define H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)  \
	(((cache_ptr)->flushes)[(entry_ptr)->type->id])++; \
        ((entry_ptr)->flushes)++;

#define H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr)        \
	(((cache_ptr)->evictions)[(entry_ptr)->type->id])++;        \
        if ( (entry_ptr)->accesses >                                \
             ((cache_ptr)->max_accesses)[(entry_ptr)->type->id] ) { \
            ((cache_ptr)->max_accesses)[(entry_ptr)->type->id]      \
                = (entry_ptr)->accesses;                            \
        }                                                           \
        if ( (entry_ptr)->accesses <                                \
             ((cache_ptr)->min_accesses)[(entry_ptr)->type->id] ) { \
            ((cache_ptr)->min_accesses)[(entry_ptr)->type->id]      \
                = (entry_ptr)->accesses;                            \
        }                                                           \
        if ( (entry_ptr)->clears >                                  \
             ((cache_ptr)->max_clears)[(entry_ptr)->type->id] ) {   \
            ((cache_ptr)->max_clears)[(entry_ptr)->type->id]        \
                 = (entry_ptr)->clears;                             \
        }                                                           \
        if ( (entry_ptr)->flushes >                                 \
             ((cache_ptr)->max_flushes)[(entry_ptr)->type->id] ) {  \
            ((cache_ptr)->max_flushes)[(entry_ptr)->type->id]       \
                 = (entry_ptr)->flushes;                            \
        }                                                           \

#define H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)     \
	if ( hit )                                                   \
            ((cache_ptr)->hits)[(entry_ptr)->type->id]++;            \
	else                                                         \
            ((cache_ptr)->misses)[(entry_ptr)->type->id]++;          \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )   \
            (cache_ptr)->max_index_len = (cache_ptr)->index_len;     \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size ) \
            (cache_ptr)->max_index_size = (cache_ptr)->index_size;   \
        if ( (cache_ptr)->pl_len > (cache_ptr)->max_pl_len )         \
            (cache_ptr)->max_pl_len = (cache_ptr)->pl_len;           \
        if ( (cache_ptr)->pl_size > (cache_ptr)->max_pl_size )       \
            (cache_ptr)->max_pl_size = (cache_ptr)->pl_size;         \
        ((entry_ptr)->accesses)++;

#else /* H5C_COLLECT_CACHE_ENTRY_STATS */

#define H5C__RESET_CACHE_ENTRY_STATS(entry_ptr) 

#define H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr) \
	(((cache_ptr)->clears)[(entry_ptr)->type->id])++; 

#define H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr) \
	(((cache_ptr)->flushes)[(entry_ptr)->type->id])++; 

#define H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr) \
	(((cache_ptr)->evictions)[(entry_ptr)->type->id])++; 

#define H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)     \
	if ( hit )                                                   \
            ((cache_ptr)->hits)[(entry_ptr)->type->id]++;            \
	else                                                         \
            ((cache_ptr)->misses)[(entry_ptr)->type->id]++;          \
        if ( (cache_ptr)->index_len > (cache_ptr)->max_index_len )   \
            (cache_ptr)->max_index_len = (cache_ptr)->index_len;     \
        if ( (cache_ptr)->index_size > (cache_ptr)->max_index_size ) \
            (cache_ptr)->max_index_size = (cache_ptr)->index_size;   \
        if ( (cache_ptr)->pl_len > (cache_ptr)->max_pl_len )         \
            (cache_ptr)->max_pl_len = (cache_ptr)->pl_len;           \
        if ( (cache_ptr)->pl_size > (cache_ptr)->max_pl_size )       \
            (cache_ptr)->max_pl_size = (cache_ptr)->pl_size;

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

#else /* H5C_COLLECT_CACHE_STATS */

#define H5C__RESET_CACHE_ENTRY_STATS(entry_ptr) 
#define H5C__UPDATE_STATS_FOR_RENAME(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr)
#define H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)

#endif /* H5C_COLLECT_CACHE_STATS */


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
 * magic:	Unsigned 32 bit integer always set to H5C__H5C_T_MAGIC.  This
 *		field is used to validate pointers to instances of H5C_t.
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
 *		the cache will presume that writes are always permissable.
 *
 *
 * The cache requires an index to facilitate searching for entries.  The
 * following fields support that index.
 *
 * index_len:   Number of entries currently in the threaded binary B-tree
 *              used to index the cache.
 *
 * index_size:  Number of bytes of cache entries currently stored in the
 *              threaded binary B-tree used to index the cache.
 *
 *              This value should not be mistaken for footprint of the
 *              cache in memory.  The average cache entry is small, and
 *              the cache has a considerable overhead.  Multiplying the
 *              index_size by two should yield a conservative estimate
 *              of the cache's memory footprint.
 *
 * index_tree_ptr: pointer to the instance of H5TB_TREE used to index
 *              the cache.  I use an instance of H5TB_TREE instead of
 *              a more conventional hash table based design for two
 *              reasons:
 *
 *              a) the code is already present and tested.
 *
 *              b) the H5TB_TREE makes it easy to check for adjacent
 *                 cache entries so that writes can be combined and
 *                 thus optimized.
 *
 *              If time permitted, a more efficient index could be
 *              constructed.  However, this should do for now.  If the
 *              additional lookup overhead proves excessive, I will
 *              write specialized code.
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
 * Statistics collection fields:
 *
 * When enabled, these fields are used to collect statistics as described
 * below.  The first set are collected only when H5C_COLLECT_CACHE_STATS
 * is true.
 *
 * hits:        Array of int64 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the number of times an entry with type id 
 *		equal to the array index has been in cache when requested in 
 *		the current epoch.
 *
 * misses:      Array of int64 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the number of times an entry with type id 
 *		equal to the array index has not been in cache when 
 *		requested in the current epoch.
 *
 * insertions:  Array of int64 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the number of times an entry with type 
 *		id equal to the array index has been inserted into the 
 *		cache in the current epoch.
 *
 * clears:      Array of int64 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the number of times an entry with type 
 *		id equal to the array index has been cleared in the current 
 *		epoch.
 *
 * flushes:     Array of int64 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the number of times an entry with type id 
 *		equal to the array index has been written to disk in the
 *              current epoch.
 *
 * evictions:   Array of int64 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the number of times an entry with type id 
 *		equal to the array index has been evicted from the cache in 
 *		the current epoch.
 *
 * renames:     Array of int64 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the number of times an entry with type 
 *		id equal to the array index has been renamed in the current 
 *		epoch.
 *
 * max_index_len:  Largest value attained by the index_len field in the
 *              current epoch.
 *
 * max_index_size:  Largest value attained by the index_size field in the
 *              current epoch.
 *
 * max_pl_len:  Largest value attained by the pl_len field in the
 *              current epoch.
 *
 * max_pl_size: Largest value attained by the pl_size field in the
 *              current epoch.
 *
 * The remaining stats are collected only when both H5C_COLLECT_CACHE_STATS
 * and H5C_COLLECT_CACHE_ENTRY_STATS are true.
 *
 * max_accesses: Array of int32 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the maximum number of times any single 
 *		entry with type id equal to the array index has been 
 *		accessed in the current epoch.
 *
 * min_accesses: Array of int32 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the minimum number of times any single 
 *		entry with type id equal to the array index has been 
 *		accessed in the current epoch.
 *
 * max_clears:  Array of int32 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the maximum number of times any single 
 *		entry with type id equal to the array index has been cleared 
 *		in the current epoch.
 *
 * max_flushes: Array of int32 of length H5C__MAX_NUM_TYPE_IDS.  The cells 
 *		are used to record the maximum number of times any single 
 *		entry with type id equal to the array index has been 
 *		flushed in the current epoch.
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
 ****************************************************************************/

#define H5C__H5C_T_MAGIC		0x005CAC0E
#define H5C__MAX_NUM_TYPE_IDS		9

struct H5C_t
{
    uint32_t			magic;

    int32_t			max_type_id;
    const char *                (* type_name_table_ptr);

    size_t                      max_cache_size;
    size_t                      min_clean_size;

    H5C_write_permitted_func_t	check_write_permitted;

    int32_t                     index_len;
    size_t                      index_size;
    H5TB_TREE *                 index_tree_ptr;

    int32_t                     pl_len;
    size_t                      pl_size;
    H5C_cache_entry_t *	        pl_head_ptr;
    H5C_cache_entry_t *  	pl_tail_ptr;

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

#if H5C_COLLECT_CACHE_STATS

    /* stats fields */
    int64_t                     hits[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     misses[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     insertions[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     clears[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     flushes[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     evictions[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     renames[H5C__MAX_NUM_TYPE_IDS];

    int32_t                     max_index_len;
    size_t                      max_index_size;

    int32_t                     max_pl_len;
    size_t                      max_pl_size;

#if H5C_COLLECT_CACHE_ENTRY_STATS

    int32_t                     max_accesses[H5C__MAX_NUM_TYPE_IDS];
    int32_t                     min_accesses[H5C__MAX_NUM_TYPE_IDS];
    int32_t                     max_clears[H5C__MAX_NUM_TYPE_IDS];
    int32_t                     max_flushes[H5C__MAX_NUM_TYPE_IDS];

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

#endif /* H5C_COLLECT_CACHE_STATS */

    hbool_t			skip_file_checks;
    hbool_t			skip_dxpl_id_checks;

};


/*
 * Private file-scope variables.
 */

/* Declare a free list to manage the H5C_t struct */
H5FL_DEFINE_STATIC(H5C_t);

/*
 * Private file-scope function declarations:
 */

static herr_t H5C_flush_single_entry(H5F_t *             f,
                                     hid_t               primary_dxpl_id,
                                     hid_t               secondary_dxpl_id,
                                     H5C_t *             cache_ptr,
                                     const H5C_class_t * type_ptr,
                                     haddr_t             addr,
                                     unsigned            flags,
                                     H5TB_NODE *         tgt_node_ptr,
                                     hbool_t *           first_flush_ptr,
                                     hbool_t remove_entry_from_tree_on_destroy);

static herr_t H5C_insert_entry_in_tree(H5C_t *             cache_ptr,
                                       H5C_cache_entry_t * entry_ptr);

static void * H5C_load_entry(H5F_t *             f,
                             hid_t               dxpl_id,
                             const H5C_class_t * type,
                             haddr_t             addr,
                             const void *        udata1,
                             void *              udata2,
                             hbool_t             skip_file_checks);

static herr_t H5C_make_space_in_cache(H5F_t * f,
                                      hid_t   primary_dxpl_id,
                                      hid_t   secondary_dxpl_id,
                                      H5C_t * cache_ptr,
                                      size_t  space_needed,
                                      hbool_t write_permitted);

static herr_t H5C_remove_entry_from_tree(H5C_t *             cache_ptr,
                                         H5C_cache_entry_t * entry_ptr,
                                         H5TB_NODE *         node_ptr);

static herr_t H5C_update_rp_for_eviction(H5C_t *              cache_ptr,
                                         H5C_cache_entry_t * entry_ptr);

static herr_t H5C_update_rp_for_flush(H5C_t *             cache_ptr,
                                      H5C_cache_entry_t * entry_ptr);

static herr_t H5C_update_rp_for_insertion(H5C_t *             cache_ptr,
                                          H5C_cache_entry_t * entry_ptr);

static herr_t H5C_update_rp_for_protect(H5C_t *             cache_ptr,
                                        H5C_cache_entry_t * entry_ptr);

static herr_t H5C_update_rp_for_rename(H5C_t *             cache_ptr,
                                       H5C_cache_entry_t * entry_ptr);

static herr_t H5C_update_rp_for_unprotect(H5C_t *             cache_ptr,
                                          H5C_cache_entry_t * entry_ptr);


/*-------------------------------------------------------------------------
 * Function:    H5C_create
 *
 * Purpose:     Allocate, initialize, and return the address of a new 
 *		instance of H5C_t.
 *
 *		In general, the max_cache_size parameter must be positive, 
 *		and the min_clean_size parameter must lie in the closed 
 *		interval [0, max_cache_size]. 
 *
 *		The check_write_permitted parameter must either be NULL,
 *		or point to a function of type H5C_write_permitted_func_t.
 *		If it is NULL, the cache will presume that writes are 
 *		always permitted.
 *
 * Return:      Success:        Pointer to the new instance.
 *
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

H5C_t *
H5C_create(size_t		      max_cache_size,
           size_t		      min_clean_size,
           int			      max_type_id,
           const char *		      (* type_name_table_ptr),
           H5C_write_permitted_func_t check_write_permitted)
{
    int i;
    H5C_t * cache_ptr = NULL;
    H5C_t * ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_create, NULL)

    HDassert( max_cache_size > 0 );
    HDassert( min_clean_size <= max_cache_size );

    HDassert( max_type_id >= 0 );
    HDassert( max_type_id < H5C__MAX_NUM_TYPE_IDS );
    HDassert( type_name_table_ptr );

    for ( i = 0; i <= max_type_id; i++ ) {

        HDassert( (type_name_table_ptr)[i] );
        HDassert( HDstrlen(( type_name_table_ptr)[i]) > 0 );
    }


    if ( NULL == (cache_ptr = H5FL_CALLOC(H5C_t)) ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
                    "memory allocation failed")
    }

    if ( (cache_ptr->index_tree_ptr = H5TB_fast_dmake(H5TB_FAST_HADDR_COMPARE)) 
         == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, NULL, "can't create TBBT.")
    }

    /* If we get this far, we should succeed.  Go ahead and initialize all
     * the fields.
     */

    cache_ptr->magic 			= H5C__H5C_T_MAGIC;

    cache_ptr->max_type_id		= max_type_id;
    cache_ptr->type_name_table_ptr	= type_name_table_ptr;

    cache_ptr->max_cache_size		= max_cache_size;
    cache_ptr->min_clean_size		= min_clean_size;

    cache_ptr->check_write_permitted	= check_write_permitted;

    cache_ptr->index_len		= 0;
    cache_ptr->index_size		= (size_t)0;

    cache_ptr->pl_len			= 0;
    cache_ptr->pl_size			= (size_t)0;
    cache_ptr->pl_head_ptr		= NULL;
    cache_ptr->pl_tail_ptr		= NULL;

    cache_ptr->LRU_list_len		= 0;
    cache_ptr->LRU_list_size		= (size_t)0;
    cache_ptr->LRU_head_ptr		= NULL;
    cache_ptr->LRU_tail_ptr		= NULL;

    cache_ptr->cLRU_list_len		= 0;
    cache_ptr->cLRU_list_size		= (size_t)0;
    cache_ptr->cLRU_head_ptr		= NULL;
    cache_ptr->cLRU_tail_ptr		= NULL;

    cache_ptr->dLRU_list_len		= 0;
    cache_ptr->dLRU_list_size		= (size_t)0;
    cache_ptr->dLRU_head_ptr		= NULL;
    cache_ptr->dLRU_tail_ptr		= NULL;

    H5C_stats__reset(cache_ptr);

    cache_ptr->skip_file_checks		= FALSE;
    cache_ptr->skip_dxpl_id_checks	= FALSE;

    /* Set return value */
    ret_value = cache_ptr;

done:

    if ( ret_value == 0 ) {

        if ( cache_ptr != NULL ) {

            if ( cache_ptr->index_tree_ptr != NULL ) {

                /* the index tree should be empty, so we can pass in
                 * NULL for the fd & fk parameters.
                 */
                H5TB_dfree(cache_ptr->index_tree_ptr, NULL, NULL);
            }

            cache_ptr->magic = 0;
            H5FL_FREE(H5C_t, cache_ptr);
            cache_ptr = NULL;

        } /* end if */

    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_create() */


/*-------------------------------------------------------------------------
 * Function:    H5C_dest
 *
 * Purpose:     Flush all data to disk and destroy the cache.
 *
 *              This function fails if any object are protected since the
 *              resulting file might not be consistent.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the destroy (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the metadata 
 *		cache, but may not be needed elsewhere.  If so, just use the 
 *		same dxpl_id for both parameters.
 *
 *		Note that *cache_ptr has been freed upon successful return.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_dest(H5F_t * f, 
         hid_t	 primary_dxpl_id, 
         hid_t	 secondary_dxpl_id, 
         H5C_t * cache_ptr)
{
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_dest, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );

    if ( H5C_flush_cache(f, primary_dxpl_id, secondary_dxpl_id, 
                         cache_ptr, H5F_FLUSH_INVALIDATE) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    }

    if ( cache_ptr->index_tree_ptr != NULL ) {

        /* the index tree should be empty, so we can pass in
         * NULL for the fd & fk parameters.
         */
        H5TB_dfree(cache_ptr->index_tree_ptr, NULL, NULL);
        cache_ptr->index_tree_ptr = NULL;
    }

    cache_ptr->magic = 0;

    H5FL_FREE(H5C_t, cache_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5C_dest_empty
 *
 * Purpose:     Destroy an empty cache.
 *
 *              This function fails if the cache is not empty on entry.
 *
 *		Note that *cache_ptr has been freed upon successful return.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_dest_empty(H5C_t * cache_ptr)
{
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_dest_empty, FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( ! cache_ptr ) || 
         ( cache_ptr->magic != H5C__H5C_T_MAGIC ) || 
         ( cache_ptr->index_len != 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Bad cache_ptr or non-empty cache on entry.")
    }


    if ( cache_ptr->index_tree_ptr != NULL ) {

        /* the index tree should be empty, so we can pass in
         * NULL for the fd & fk parameters.
         */
        H5TB_dfree(cache_ptr->index_tree_ptr, NULL, NULL);
        cache_ptr->index_tree_ptr = NULL;
    }

    cache_ptr->magic = 0;

    H5FL_FREE(H5C_t, cache_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_dest_empty() */


/*-------------------------------------------------------------------------
 * Function:    H5C_flush_cache
 *
 * Purpose:	Flush (and possibly destroy) the entries contained in the
 *		specified cache.
 *
 *		If the cache contains protected entries, the function will
 *		fail, as protected entries cannot be flushed.  However 
 *		all unprotected entries should be flushed before the 
 *		function returns failure.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the flush (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the metadata 
 *		cache, but may not be needed elsewhere.  If so, just use the 
 *		same dxpl_id for both parameters.
 *		
 * Return:      Non-negative on success/Negative on failure or if there was 
 *		a request to flush all items and something was protected.
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_flush_cache(H5F_t *  f, 
                hid_t    primary_dxpl_id, 
                hid_t    secondary_dxpl_id, 
                H5C_t *  cache_ptr,
                unsigned flags)
{
    herr_t              status;
    herr_t		ret_value = SUCCEED;
    hbool_t             destroy = ( (flags & H5F_FLUSH_INVALIDATE) != 0 );
    hbool_t		first_flush = TRUE;
    int32_t		protected_entries = 0;
    H5TB_NODE * 	node_ptr;
    H5C_cache_entry_t *	entry_ptr;
#if H5C_DO_SANITY_CHECKS
    int32_t		actual_index_len = 0;
    size_t              actual_index_size = 0;
#endif /* H5C_DO_SANITY_CHECKS */

    FUNC_ENTER_NOAPI(H5C_flush_cache, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );


    if ( cache_ptr->index_tree_ptr->root == NULL ) {

        node_ptr = NULL;
        HDassert( cache_ptr->index_len == 0 );
        HDassert( cache_ptr->index_size == 0 );

    } else {

        node_ptr = H5TB_first(cache_ptr->index_tree_ptr->root);
    }

    while ( node_ptr != NULL )
    {
        entry_ptr = (H5C_cache_entry_t *)(node_ptr->data);
        HDassert( entry_ptr != NULL );

#if H5C_DO_SANITY_CHECKS
        actual_index_len++;
        actual_index_size += entry_ptr->size;
#endif /* H5C_DO_SANITY_CHECKS */

        if ( entry_ptr->is_protected ) {

            /* we have major problems -- but lets flush everything 
             * we can before we flag an error.
             */
	    protected_entries++;

        } else { 

            status = H5C_flush_single_entry(f, 
                                            primary_dxpl_id, 
                                            secondary_dxpl_id, 
                                            cache_ptr,
                                            NULL, 
                                            entry_ptr->addr, 
                                            flags, 
                                            node_ptr, 
                                            &first_flush, 
                                            FALSE);
            if ( status < 0 ) {
                
                /* This shouldn't happen -- if it does, we are toast so
                 * just scream and die.
                 */
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "Can't flush entry.")
            }
        }

        node_ptr = H5TB_next(node_ptr);

    } /* while */

    HDassert( protected_entries == cache_ptr->pl_len );

#if H5C_DO_SANITY_CHECKS
    HDassert( actual_index_len == cache_ptr->index_len );
    HDassert( actual_index_size == cache_ptr->index_size );
#endif /* H5C_DO_SANITY_CHECKS */

    if ( destroy ) {

        /* don't pass in any key or data free functions, as all
         * unprotected entries should have already been destroyed.
         */
        H5TB_free(&(cache_ptr->index_tree_ptr->root), NULL, NULL);
        cache_ptr->index_len = 0;
        cache_ptr->index_size = 0;

        if ( protected_entries > 0 )
        {
            /* the caller asked us to flush and destroy a cache that 
             * contains one or more protected entries.  Since we can't
             * flush protected entries, we haven't destroyed them either.
             * Since they are all on the protected list, just re-insert 
             * them into the tree before we flag an error.
             */
            entry_ptr = cache_ptr->pl_head_ptr;

            while ( entry_ptr != NULL )
            {
                if ( H5C_insert_entry_in_tree(cache_ptr, entry_ptr) < 0 ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                                "Can't re-insert protected entry.")
                }
                entry_ptr = entry_ptr->next;
            }
        }
    }

    if ( protected_entries > 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_PROTECT, FAIL, "cache has protected items")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_flush_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5C_insert_entry
 *
 * Purpose:     Adds the specified thing to the cache.  The thing need not
 *              exist on disk yet, but it must have an address and disk
 *              space reserved.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the insertion (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the 
 *		metadata cache, but may not be needed elsewhere.  If so, 
 *		just use the same dxpl_id for both parameters.
 *
 *		The primary_dxpl_id is the dxpl_id passed to the 
 *		check_write_permitted function if such a function has been 
 *		provided.
 *
 *		Observe that this function cannot occasion a read.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_insert_entry(H5F_t * 	     f, 
                 hid_t		     primary_dxpl_id, 
                 hid_t		     secondary_dxpl_id, 
                 H5C_t *	     cache_ptr,
                 const H5C_class_t * type, 
                 haddr_t 	     addr, 
                 void *		     thing)
{
    herr_t		result;
    herr_t		ret_value = SUCCEED;    /* Return value */
    hbool_t		write_permitted = TRUE;
    H5C_cache_entry_t *	entry_ptr;
    H5TB_NODE *		node_ptr = NULL;

    FUNC_ENTER_NOAPI(H5C_insert_entry, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( type );
    HDassert( type->flush );
    HDassert( type->size );
    HDassert( H5F_addr_defined(addr) );
    HDassert( thing );

    entry_ptr = (H5C_cache_entry_t *)thing;

    entry_ptr->addr = addr;
    entry_ptr->type = type;

    if ( (type->size)(f, thing, &(entry_ptr->size)) < 0 ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGETSIZE, FAIL, \
                    "Can't get size of thing")
    }

    HDassert( entry_ptr->size < H5C_MAX_ENTRY_SIZE ); 

    entry_ptr->next = NULL;
    entry_ptr->prev = NULL; 
    entry_ptr->aux_next = NULL;
    entry_ptr->aux_prev = NULL;

    H5C__RESET_CACHE_ENTRY_STATS(entry_ptr) 

    if ( (cache_ptr->index_size + entry_ptr->size) > cache_ptr->max_cache_size ){

        size_t space_needed;

        if ( cache_ptr->check_write_permitted != NULL ) {

            result = (cache_ptr->check_write_permitted)(f, 
                                                        primary_dxpl_id, 
                                                        &write_permitted);

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                            "Can't get write_permitted")
            }
        }

        HDassert( entry_ptr->size <= H5C_MAX_ENTRY_SIZE );

        space_needed = (cache_ptr->index_size + entry_ptr->size) -
                       cache_ptr->max_cache_size;

        /* It would be nice to be able to do a tight sanity check on
         * space_needed here, but it is hard to assign an upper bound on
         * its value other than then value assigned to it.
         *
         * This fact springs from several features of the cache:
         *
         * First, it is possible for the cache to grow without
         * bound as long as entries are protected and not unprotected.
         *
         * Second, when writes are not permitted it is also possible
         * for the cache to grow without bound.
         *
         * Finally, we don't check to see if the cache is oversized
         * at the end of an unprotect.  As a result, it is possible
         * to have a vastly oversized cache with no protected entries
         * as long as all the protects preceed the unprotects.
         *
         * Since items 1 and 2 are not changing any time soon, I see
         * no point in worrying about the third.
         *
         * In any case, I hope this explains why there is no sanity
         * check on space_needed here.
         */

        result = H5C_make_space_in_cache(f, 
                                         primary_dxpl_id, 
                                         secondary_dxpl_id,
                                         cache_ptr, 
                                         space_needed, 
                                         write_permitted);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                        "H5C_make_space_in_cache failed.")
        }
    }

    /* verify the the new entry isn't already in the tree -- scream
     * and die if it is.
     */
    node_ptr = H5TB_dfind(cache_ptr->index_tree_ptr, entry_ptr, NULL);

    if ( node_ptr != NULL ) {

        if ( node_ptr->key == ((void *)(entry_ptr)) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                        "entry already in cache.")

        } else {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                        "duplicate entry in cache.")
       
        }
    }

    /* we don't initialize the protected field until here as it is
     * possible that the entry is already in the cache, and already 
     * protected.  If it is, we don't want to make things worse by
     * marking it unprotected.
     */

    entry_ptr->is_protected = FALSE;

    if ( H5C_insert_entry_in_tree(cache_ptr, entry_ptr) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                    "Can't insert entry in tree.")
       
    }

    if ( H5C_update_rp_for_insertion(cache_ptr, entry_ptr) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, \
                    "Can't update replacement policy for insertion.")
       
    }

    H5C__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_insert_entry() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_rename_entry
 *
 * Purpose:     Use this function to notify the cache that an entry's
 *              file address changed.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_rename_entry(H5F_t *	     f, 
                 H5C_t *	     cache_ptr,
                 const H5C_class_t * type, 
                 haddr_t 	     old_addr,
	         haddr_t 	     new_addr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */
    H5TB_NODE *		new_node_ptr = NULL;
    H5TB_NODE *		old_node_ptr = NULL;
    H5C_cache_entry_t *	entry_ptr;
    H5C_cache_entry_t	search_target;

    FUNC_ENTER_NOAPI(H5C_rename_entry, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( type );
    HDassert( H5F_addr_defined(old_addr) );
    HDassert( H5F_addr_defined(new_addr) );
    HDassert( H5F_addr_ne(old_addr, new_addr) );

    search_target.addr = old_addr;
    old_node_ptr = H5TB_dfind(cache_ptr->index_tree_ptr, 
                              (void *)(&search_target), 
                              NULL);

    if ( ( old_node_ptr == NULL ) || 
         ( ((H5C_cache_entry_t *)(old_node_ptr->key))->type != type ) ) {

        /* the old item doesn't exist in the cache, so we are done. */
        HGOTO_DONE(SUCCEED)

    } else {

        entry_ptr = old_node_ptr->key;
        HDassert( entry_ptr->addr == old_addr );
        HDassert( entry_ptr->type == type );
        HDassert( !(entry_ptr->is_protected) );
    }

    search_target.addr = new_addr;
    new_node_ptr = H5TB_dfind(cache_ptr->index_tree_ptr, 
                              (void *)&search_target, 
                              NULL);

    if ( new_node_ptr != NULL ) { /* we are hosed */
        
        if ( ((H5C_cache_entry_t *)(new_node_ptr->key))->type == type ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTRENAME, FAIL, \
                        "Target already renamed & reinserted???.")

        } else {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTRENAME, FAIL, \
                        "New address already in use?.")

        }
    }

    /* If we get this far, we have work to do.  Remove *entry_ptr from
     * the tree, change its address to the new address, and then re-insert.
     * Update the replacement policy for a hit to avoid an eviction before
     * the renamed entry is touched.  Update stats for a rename.
     *
     * Note that we do not check the size of the cache, or evict anything.
     * Since this is a simple re-name, cache size should be unaffected.
     */

    if ( H5C_remove_entry_from_tree(cache_ptr, entry_ptr, old_node_ptr) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTRENAME, FAIL, \
                    "Can't remove entry from tree.")
    }

    entry_ptr->addr = new_addr;

    if ( H5C_insert_entry_in_tree(cache_ptr, entry_ptr) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTRENAME, FAIL, \
                    "Can't re-insert entry from tree.")
    }

    if ( H5C_update_rp_for_rename(cache_ptr, entry_ptr) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTRENAME, FAIL, \
                    "Can't can't update replacement policy for a hit.")
    }

    H5C__UPDATE_STATS_FOR_RENAME(cache_ptr, entry_ptr)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_rename_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5C_protect
 *
 * Purpose:     If the target entry is not in the cache, load it.  If 
 *		necessary, attempt to evict one or more entries to keep
 *		the cache within its maximum size.
 *
 *		Mark the target entry as protected, and return its address
 *		to the caller.  The caller must call H5C_unprotect() when
 *		finished with the entry.
 *
 *		While it is protected, the entry may not be either evicted 
 *		or flushed -- nor may it be accessed by another call to 
 *		H5C_protect.  Any attempt to do so will result in a failure.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the insertion (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the 
 *		metadata cache, but may not be needed elsewhere.  If so, 
 *		just use the same dxpl_id for both parameters.
 *
 *		All reads are performed with the primary_dxpl_id.
 *
 *		Similarly, the primary_dxpl_id is passed to the 
 *		check_write_permitted function if it is called.
 *
 * Return:      Success:        Ptr to the desired entry
 *
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer -  6/2/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void *
H5C_protect(H5F_t *	       f, 
           hid_t	       primary_dxpl_id, 
           hid_t	       secondary_dxpl_id, 
           H5C_t *	       cache_ptr,
           const H5C_class_t * type, 
           haddr_t 	       addr,
           const void *	       udata1, 
           void *	       udata2)
{
    hbool_t		hit = FALSE;
    void *		thing = NULL;
    H5C_cache_entry_t *	entry_ptr;
    H5TB_NODE *         node_ptr = NULL;
    void *		ret_value;      /* Return value */
    H5C_cache_entry_t	search_target;

    FUNC_ENTER_NOAPI(H5C_protect, NULL)

    /* check args */
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( type );
    HDassert( type->flush );
    HDassert( type->load );
    HDassert( H5F_addr_defined(addr) );

    /* first check to see if the target is in cache */
    search_target.addr = addr;
    node_ptr = H5TB_dfind(cache_ptr->index_tree_ptr,
                          (void *)(&search_target),
                          NULL);

    if ( node_ptr != NULL ) {

        hit = TRUE;
        thing = node_ptr->key;
        entry_ptr = (H5C_cache_entry_t *)thing;

    } else { /* must try to load the entry from disk. */

        hit = FALSE;
        thing = H5C_load_entry(f, primary_dxpl_id, type, addr, udata1, udata2,
                               cache_ptr->skip_file_checks);

        if ( thing == NULL ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTLOAD, NULL, "can't load entry")
        }

        entry_ptr = (H5C_cache_entry_t *)thing;

        /* try to free up some space if necessay */
        if ( (cache_ptr->index_size + entry_ptr->size) > 
              cache_ptr->max_cache_size ) {

            hbool_t write_permitted = TRUE;
            herr_t result;
            size_t space_needed;

            if ( cache_ptr->check_write_permitted != NULL ) {

                result = (cache_ptr->check_write_permitted)(f, 
                                                            primary_dxpl_id, 
                                                            &write_permitted);

                if ( result < 0 ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                               "Can't get write_permitted")
                }
            }

            HDassert( entry_ptr->size <= H5C_MAX_ENTRY_SIZE );

            space_needed = (cache_ptr->index_size + entry_ptr->size) -
                           cache_ptr->max_cache_size;

            /* It would be nice to be able to do a tight sanity check on 
             * space_needed here, but it is hard to assign an upper bound on 
             * its value other than then value assigned to it.
             *
             * This fact springs from several features of the cache:
             *
             * First, it is possible for the cache to grow without
             * bound as long as entries are protected and not unprotected.
             *
             * Second, when writes are not permitted it is also possible
             * for the cache to grow without bound.
             *
             * Finally, we don't check to see if the cache is oversized 
             * at the end of an unprotect.  As a result, it is possible 
             * to have a vastly oversized cache with no protected entries
             * as long as all the protects preceed the unprotects.
             *
             * Since items 1 and 2 are not changing any time soon, I see
             * no point in worrying about the third.
             *
             * In any case, I hope this explains why there is no sanity
             * check on space_needed here.
             */

            result = H5C_make_space_in_cache(f, primary_dxpl_id, 
                                             secondary_dxpl_id, cache_ptr, 
                                             space_needed, write_permitted);

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                            "H5C_make_space_in_cache failed.")
            }
        }

        /* insert the entry in the tree and in the protected list. */
        if ( H5C_insert_entry_in_tree(cache_ptr, entry_ptr) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                        "Can't insert newly loaded entry in tree.")
        }

        /* insert the entry in the data structures used by the replacement
         * policy.  We are just going to take it out again when we update
         * the replacement policy for a protect, but this simplifies the
         * code.  If we do this often enough, we may want to optimize this.
         */

        if ( H5C_update_rp_for_insertion(cache_ptr, entry_ptr) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                     "Can't update replacement policy for newly loaded entry.")

        }
    }

    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->type == type );

    if ( entry_ptr->is_protected ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                    "Target already protected?!?.")
    }

    if ( H5C_update_rp_for_protect(cache_ptr, entry_ptr) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                    "Can't update replacement policy for protect")
    }

    entry_ptr->is_protected = TRUE;

    ret_value = thing;

    H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_protect() */


/*-------------------------------------------------------------------------
 * Function:    H5C_unprotect
 *
 * Purpose:	Undo an H5C_protect() call -- specifically, mark the
 *		entry as unprotected, remove it from the protected list,
 *		and give it back to the replacement policy.
 *
 *		The TYPE and ADDR arguments must be the same as those in
 *		the corresponding call to H5C_protect() and the THING 
 *		argument must be the value returned by that call to 
 *		H5C_protect().
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the unprotect (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  Since an uprotect cannot 
 *		occasion a write at present, all this is moot for now.  
 *		However, things change, and in any case, 
 *		H5C_flush_single_entry() needs primary_dxpl_id and 
 *		secondary_dxpl_id in its parameter list.
 *
 *		The function can't cause a read either, so the dxpl_id
 *		parameters are moot in this case as well.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *		If the deleted flag is TRUE, simply remove the target entry
 *		from the cache, clear it, and free it without writing it to 
 *		disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_unprotect(H5F_t *		  f, 
              hid_t		  primary_dxpl_id, 
              hid_t		  secondary_dxpl_id, 
              H5C_t *		  cache_ptr,
              const H5C_class_t * type, 
              haddr_t		  addr,
              void *		  thing, 
              hbool_t		  deleted)
{
    herr_t              ret_value = SUCCEED;    /* Return value */
    H5C_cache_entry_t *	entry_ptr;

    FUNC_ENTER_NOAPI(H5C_unprotect, FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( type );
    HDassert( type->clear );
    HDassert( type->flush );
    HDassert( H5F_addr_defined(addr) );
    HDassert( thing );

    entry_ptr = (H5C_cache_entry_t *)thing;

    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->type == type );

    if ( ! (entry_ptr->is_protected) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, \
                        "Entry already unprotected??")
    }

    if ( H5C_update_rp_for_unprotect(cache_ptr, entry_ptr) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, \
                        "Can't update replacement policy for unprotect.")
    }

    entry_ptr->is_protected = FALSE;    

    /* this implementation of the "deleted" option is a bit inefficient, as
     * we re-insert the entry to be deleted into the replacement policy
     * data structures, only to remove them again.  Depending on how often
     * we do this, we may want to optimize a bit.  
     *
     * On the other hand, this implementation is reasonably clean, and 
     * makes good use of existing code.
     *                                             JRM - 5/19/04
     */
    if ( deleted ) {

        /* the following first flush flag will never be used as we are
         * calling H5C_flush_single_entry with both the H5F_FLUSH_CLEAR_ONLY
         * and H5F_FLUSH_INVALIDATE flags.  However, it is needed for the
         * function call.
         */
        hbool_t		dummy_first_flush = TRUE;
        H5TB_NODE *	node_ptr;

        /* verify that the target entry is in the tree. */

        node_ptr = H5TB_dfind(cache_ptr->index_tree_ptr, entry_ptr, NULL);

        if ( ( node_ptr == NULL ) || ( node_ptr->key != thing ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, \
                        "thing not in tree?!?.")
        }

        if ( H5C_flush_single_entry(f, 
                                    primary_dxpl_id, 
                                    secondary_dxpl_id, 
                                    cache_ptr,
                                    type, 
                                    addr, 
                                    (H5F_FLUSH_CLEAR_ONLY|H5F_FLUSH_INVALIDATE),
                                    node_ptr, 
                                    &dummy_first_flush, 
                                    TRUE) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, \
                        "thing not in tree?!?.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_unprotect() */


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_stats(H5C_t * cache_ptr,
          const char *  cache_name,
          hbool_t
#ifndef H5C_COLLECT_CACHE_STATS
          UNUSED
#endif /* H5C_COLLECT_CACHE_STATS */
          display_detailed_stats)
{
    herr_t	ret_value = SUCCEED;   /* Return value */
#if H5C_COLLECT_CACHE_STATS
    int		i;
    int64_t     total_hits = 0;
    int64_t     total_misses = 0;
    int64_t     total_insertions = 0;
    int64_t     total_clears = 0;
    int64_t     total_flushes = 0;
    int64_t     total_evictions = 0;
    int64_t     total_renames = 0;
    int32_t     aggregate_max_accesses = 0;
    int32_t     aggregate_min_accesses = 1000000;
    int32_t     aggregate_max_clears = 0;
    int32_t     aggregate_max_flushes = 0;
    double      hit_rate;
#endif /* H5C_COLLECT_CACHE_STATS */

    FUNC_ENTER_NOAPI(H5C_stats, FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( ! cache_ptr ) || 
         ( cache_ptr->magic != H5C__H5C_T_MAGIC ) || 
         ( !cache_name ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr or cache_name")
    }

#if H5C_COLLECT_CACHE_STATS

    for ( i = 0; i <= cache_ptr->max_type_id; i++ ) {

        total_hits       += cache_ptr->hits[i];
        total_misses     += cache_ptr->misses[i];
        total_insertions += cache_ptr->insertions[i];
        total_clears     += cache_ptr->clears[i];
        total_flushes    += cache_ptr->flushes[i];
        total_evictions  += cache_ptr->evictions[i];
        total_renames    += cache_ptr->renames[i];
#if H5C_COLLECT_CACHE_ENTRY_STATS
    if ( aggregate_max_accesses < cache_ptr->max_accesses[i] )
        aggregate_max_accesses = cache_ptr->max_accesses[i];
    if ( aggregate_min_accesses > aggregate_max_accesses )
        aggregate_min_accesses = aggregate_max_accesses;
    if ( aggregate_min_accesses > cache_ptr->min_accesses[i] )
        aggregate_min_accesses = cache_ptr->min_accesses[i]; 
    if ( aggregate_max_clears < cache_ptr->max_clears[i] )
        aggregate_max_clears = cache_ptr->max_clears[i];
    if ( aggregate_max_flushes < cache_ptr->max_flushes[i] )
        aggregate_max_flushes = cache_ptr->max_flushes[i];
#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
    }

    if ( ( total_hits > 0 ) || ( total_misses > 0 ) ) {

        hit_rate = 100.0 * ((double)(total_hits)) /
                   ((double)(total_hits + total_misses));
    } else {
        hit_rate = 0.0;
    }

    HDfprintf(stdout, "\nH5C: cache statistics for %s\n",
              cache_name);

    HDfprintf(stdout, "\n");

    HDfprintf(stdout, 
              "  current (max) index size / length  = %ld (%ld) / %ld (%ld)\n",
              (long)(cache_ptr->index_size),
              (long)(cache_ptr->max_index_size),
              (long)(cache_ptr->index_len),
              (long)(cache_ptr->max_index_len));

    HDfprintf(stdout, 
              "  current (max) PL size / length     = %ld (%ld) / %ld (%ld)\n",
              (long)(cache_ptr->pl_size),
              (long)(cache_ptr->max_pl_size),
              (long)(cache_ptr->pl_len),
              (long)(cache_ptr->max_pl_len));

    HDfprintf(stdout,
              "  current LRU list size / length     = %ld / %ld\n",
              (long)(cache_ptr->LRU_list_size),
              (long)(cache_ptr->LRU_list_len));

    HDfprintf(stdout,
              "  current clean LRU size / length    = %ld / %ld\n",
              (long)(cache_ptr->cLRU_list_size),
              (long)(cache_ptr->cLRU_list_len));

    HDfprintf(stdout,
              "  current dirty LRU size / length    = %ld / %ld\n",
              (long)(cache_ptr->dLRU_list_size),
              (long)(cache_ptr->dLRU_list_len));

    HDfprintf(stdout, 
              "  Total hits / misses / hit_rate     = %ld / %ld / %f\n",
              (long)total_hits, 
              (long)total_misses, 
              hit_rate);

    HDfprintf(stdout, 
              "  Total clears / flushes / evictions = %ld / %ld / %ld\n",
              (long)total_clears,
              (long)total_flushes,
              (long)total_evictions);

    HDfprintf(stdout, "  Total insertions / renames         = %ld / %ld\n",
              (long)total_insertions,
              (long)total_renames);

#if H5C_COLLECT_CACHE_ENTRY_STATS

    HDfprintf(stdout, "  aggregate max / min accesses       = %d / %d\n",
              (int)aggregate_max_accesses,
              (int)aggregate_min_accesses);

    HDfprintf(stdout, "  aggregate max_clears / max_flushes = %d / %d\n",
              (int)aggregate_max_clears,
              (int)aggregate_max_flushes);

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

    if ( display_detailed_stats )
    {

        for ( i = 0; i <= cache_ptr->max_type_id; i++ ) {

            HDfprintf(stdout, "\n");

            HDfprintf(stdout, "  Stats on %s:\n", 
                      ((cache_ptr->type_name_table_ptr))[i]);

            if ( ( cache_ptr->hits[i] > 0 ) || ( cache_ptr->misses[i] > 0 ) ) {

                hit_rate = 100.0 * ((double)(cache_ptr->hits[i])) /
                          ((double)(cache_ptr->hits[i] + cache_ptr->misses[i]));
            } else {
                hit_rate = 0.0;
            }

            HDfprintf(stdout, 
                      "    hits / misses / hit_rate       = %ld / %ld / %f\n", 
                      (long)(cache_ptr->hits[i]),
                      (long)(cache_ptr->misses[i]),
                      hit_rate);

            HDfprintf(stdout, 
                      "    clears / flushes / evictions   = %ld / %ld / %ld\n",
                      (long)(cache_ptr->clears[i]),
                      (long)(cache_ptr->flushes[i]),
                      (long)(cache_ptr->evictions[i]));

            HDfprintf(stdout, 
                      "    insertions / renames           = %ld / %ld\n",
                      (long)(cache_ptr->insertions[i]),
                      (long)(cache_ptr->renames[i]));

#if H5C_COLLECT_CACHE_ENTRY_STATS

            HDfprintf(stdout, 
                      "    entry max / min accesses       = %d / %d\n",
                      cache_ptr->max_accesses[i],
                      cache_ptr->min_accesses[i]);

            HDfprintf(stdout, 
                      "    entry max_clears / max_flushes = %d / %d\n",
                      cache_ptr->max_clears[i],
                      cache_ptr->max_flushes[i]);

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

        }
    }

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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
H5C_stats__reset(H5C_t * cache_ptr)
{
#if H5C_COLLECT_CACHE_STATS
    int i;
#endif /* H5C_COLLECT_CACHE_STATS */

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

#if H5C_COLLECT_CACHE_STATS
    for ( i = 0; i <= cache_ptr->max_type_id; i++ )
    {
        cache_ptr->hits[i]		= 0;
        cache_ptr->misses[i]		= 0;
        cache_ptr->insertions[i]	= 0;
        cache_ptr->clears[i]		= 0;
        cache_ptr->flushes[i]		= 0;
        cache_ptr->evictions[i]	 	= 0;
        cache_ptr->renames[i]	 	= 0;
    }

    cache_ptr->max_index_len		= 0;
    cache_ptr->max_index_size		= (size_t)0;

    cache_ptr->max_pl_len		= 0;
    cache_ptr->max_pl_size		= (size_t)0;

#if H5C_COLLECT_CACHE_ENTRY_STATS

    for ( i = 0; i <= cache_ptr->max_type_id; i++ )
    {
        cache_ptr->max_accesses[i]		= 0;
        cache_ptr->min_accesses[i]		= 1000000; 
        cache_ptr->max_clears[i]		= 0;
        cache_ptr->max_flushes[i]		= 0;
    }

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
#endif /* H5C_COLLECT_CACHE_STATS */

    return;

} /* H5C_stats__reset() */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_skip_flags
 *
 * Purpose:     Set the values of the skip sanity check flags.
 *		
 *		This function and the skip sanity check flags were created
 *		for the convenience of the test bed.  However it is 
 *		possible that there may be other uses for the flags.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/11/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_set_skip_flags(H5C_t * cache_ptr,
                   hbool_t skip_file_checks,
                   hbool_t skip_dxpl_id_checks)
{
    herr_t		ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5C_set_skip_flags, FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( ! cache_ptr ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr")
    }

    cache_ptr->skip_file_checks    = skip_file_checks;
    cache_ptr->skip_dxpl_id_checks = skip_dxpl_id_checks;

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_set_skip_flags() */


/*************************************************************************/
/**************************** Private Functions: *************************/
/*************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Function:    H5C_flush_single_entry
 *
 * Purpose:     Flush or clear (and evict if requested) the cache entry 
 *		with the specified address and type.  If the type is NULL,
 *		any unprotected entry at the specified address will be 
 *		flushed (and possibly evicted).
 *
 *		Attempts to flush a protected entry will result in an 
 *		error.
 *
 *		*first_flush_ptr should be true if only one 
 *		flush is contemplated before the next load, or if this
 *		is the first of a sequence of flushes that will be 
 *		completed before the next load.  *first_flush_ptr is set
 *		to false if a flush actually takes place, and should be 
 *		left false until the end of the sequence.
 *
 *		The primary_dxpl_id is used if *first_flush_ptr is TRUE
 *		on entry, and a flush actually takes place.  The 
 *		secondary_dxpl_id is used in any subsequent flush where 
 *		*first_flush_ptr is FALSE on entry.
 *
 *		If the H5F_FLUSH_CLEAR_ONLY flag is set, the entry will
 *		be cleared and not flushed -- in the case *first_flush_ptr,
 *		primary_dxpl_id, and secondary_dxpl_id are all irrelevent,
 *		and the call can't be part of a sequence of flushes.
 *
 *		If the caller knows the address of the TBBT node at 
 *		which the target entry resides, it can avoid a lookup 
 *		by supplying that address in the tgt_node_ptr parameter.
 *		If this parameter is NULL, the function will do a TBBT
 *		search for the entry instead.
 *
 *		The function does nothing silently if there is no entry
 *		at the supplied address, or if the entry found has the 
 *		wrong type.
 *
 * Return:      Non-negative on success/Negative on failure or if there was 
 *		an attempt to flush a protected item.
 *
 * Programmer:  John Mainzer, 5/5/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_flush_single_entry(H5F_t *		   f, 
                       hid_t 		   primary_dxpl_id, 
                       hid_t 		   secondary_dxpl_id, 
                       H5C_t *		   cache_ptr,
                       const H5C_class_t * type_ptr, 
                       haddr_t		   addr, 
                       unsigned		   flags,
                       H5TB_NODE *	   tgt_node_ptr,
                       hbool_t *	   first_flush_ptr,
                       hbool_t		   remove_entry_from_tree_on_destroy)
{
    hbool_t		destroy = ( (flags & H5F_FLUSH_INVALIDATE) != 0 );
    hbool_t		clear_only = ( (flags & H5F_FLUSH_CLEAR_ONLY) != 0);  
    herr_t		ret_value = SUCCEED;      /* Return value */
    herr_t		status;
    H5TB_NODE *		node_ptr;
    H5C_cache_entry_t *	entry_ptr = NULL;
    H5C_cache_entry_t	search_target;

    FUNC_ENTER_NOAPI_NOINIT(H5C_flush_single_entry)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->skip_file_checks || f );
    HDassert( H5F_addr_defined(addr) );
    HDassert( first_flush_ptr );

    /* If tgt_node_ptr is NULL, look up the target entry in the tree.  
     * If it doesn't exist, we are done.
     */

    if ( tgt_node_ptr == NULL ) {

        search_target.addr = addr;
        node_ptr = H5TB_dfind(cache_ptr->index_tree_ptr, 
                              (void *)&search_target, 
                              NULL);

    } else {

        node_ptr = tgt_node_ptr;
    }

    if ( node_ptr != NULL ) {

        entry_ptr = (H5C_cache_entry_t *)(node_ptr->data);
        HDassert( entry_ptr != NULL );
        HDassert( entry_ptr->addr == addr );
        HDassert( node_ptr->data == node_ptr->key );
    }

    if ( ( entry_ptr != NULL ) && ( entry_ptr->is_protected ) )
    {
        /* Attempt to flush a protected entry -- scream and die. */
        HGOTO_ERROR(H5E_CACHE, H5E_PROTECT, FAIL, \
                    "Attempt to flush a protected entry.")
    }

    if ( ( entry_ptr != NULL ) &&
         ( ( type_ptr == NULL ) || ( type_ptr->id == entry_ptr->type->id ) ) )
    {
        /* we have work to do */

#ifdef H5_HAVE_PARALLEL  
#ifndef NDEBUG

        /* If MPI based VFD is used, do special parallel I/O sanity checks.
         * Note that we only do these sanity checks when the clear_only flag
         * is not set, and the entry to be flushed is dirty.  Don't bother 
         * otherwise as no file I/O can result.
         *
         * There are also cases (testing for instance) where it is convenient
         * to pass in dummy dxpl_ids.  Since we don't use the dxpl_ids directly,
         * this isn't a problem -- but we do have to turn off sanity checks
         * involving them.  We use cache_ptr->skip_dxpl_id_checks to do this.
         */
        if ( ( ! cache_ptr->skip_dxpl_id_checks ) && 
             ( ! clear_only ) && 
             ( entry_ptr->is_dirty ) && 
             ( IS_H5FD_MPI(f) ) ) {

            H5P_genplist_t *dxpl;           /* Dataset transfer property list */
            H5FD_mpio_xfer_t xfer_mode;     /* I/O xfer mode property value */

            /* Get the dataset transfer property list */
            if ( NULL == (dxpl = H5I_object(primary_dxpl_id)) ) {

                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, \
                            "not a dataset creation property list")
            }

            /* Get the transfer mode property */
            if( H5P_get(dxpl, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0 ) {

                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, \
                            "can't retrieve xfer mode")
            }

            /* Sanity check transfer mode */
            HDassert( xfer_mode == H5FD_MPIO_COLLECTIVE || IS_H5FD_FPHDF5(f) );
        }

#endif /* NDEBUG */
#endif /* H5_HAVE_PARALLEL */
        if ( clear_only ) {
            H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr)
        } else {
            H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)
        }

        if ( destroy ) {
            H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr)
        }

        /* remove entry from tree if asked  -- must do this now as the
         * callback routines will free the entry if destroy is true.
         */
        if ( ( destroy ) && ( remove_entry_from_tree_on_destroy ) ) {
            if ( H5C_remove_entry_from_tree(cache_ptr, entry_ptr, node_ptr)
                 < 0 ) {
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "Can't delete entry from tree.")
            }
        }

        /* Update the replacement policy for the flush or eviction.
         * Again, do this now so we don't have to reference freed 
         * memory in the destroy case.
         */

        if ( destroy ) { /* AKA eviction */
            status = H5C_update_rp_for_eviction(cache_ptr, entry_ptr); 
        } else { 
            status = H5C_update_rp_for_flush(cache_ptr, entry_ptr);
        }

        if ( status < 0 ) {
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                        "Can't update replacement policy.")
        }

        /* Clear the dirty flag only, if requested */
        if ( clear_only ) {
            /* Call the callback routine to clear all dirty flags for object */
            if ( (entry_ptr->type->clear)(f, entry_ptr, destroy) < 0 ) {
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "can't clear entry")
            }
        } else {

            /* Only block for all the processes on the first piece of metadata 
             */

            if ( *first_flush_ptr && entry_ptr->is_dirty ) {
                status = (entry_ptr->type->flush)(f, primary_dxpl_id, destroy, 
                                                 entry_ptr->addr, entry_ptr);
                *first_flush_ptr = FALSE;
            } else {
                status = (entry_ptr->type->flush)(f, secondary_dxpl_id, 
                                                 destroy, entry_ptr->addr, 
                                                 entry_ptr);
            }

            if ( status < 0 ) {
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }
        }

        if ( ! destroy ) {
        
            HDassert( !(entry_ptr->is_dirty) );
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_flush_single_entry() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_insert_entry_in_tree
 *
 * Purpose:     Insert the specified instance of H5C_cache_entry_t from the 
 *		index tree in the specified instance of H5C_t.  Update
 *		the associated length and size fields.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_insert_entry_in_tree(H5C_t *	     cache_ptr,
			 H5C_cache_entry_t * entry_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */
    H5TB_NODE *		node_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5C_insert_entry_in_tree)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( entry_ptr );
    HDassert( entry_ptr->size > 0 );
    HDassert( H5F_addr_defined(entry_ptr->addr) );

    /* Don't bother to check if the entry is already in the tree -- if it
     * is, H5TB_dins() will fail.
     */
    node_ptr = H5TB_dins(cache_ptr->index_tree_ptr, (void *)entry_ptr, NULL);

    if ( node_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't insert entry in tree")

    }

    cache_ptr->index_len++;
    cache_ptr->index_size += entry_ptr->size;
    HDassert( cache_ptr->index_len > 0 );
    HDassert( cache_ptr->index_size > 0 );

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_insert_entry_in_tree */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_load_entry
 *
 * Purpose:     Attempt to load the entry at the specified disk address
 *		and with the specified type into memory.  If successful.
 *		return the in memory address of the entry.  Return NULL
 *		on failure.
 *
 *		Note that this function simply loads the entry into 
 *		core.  It does not insert it into the cache.
 *
 * Return:      Non-NULL on success / NULL on failure.
 *
 * Programmer:  John Mainzer, 5/18/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void *
H5C_load_entry(H5F_t *             f, 
               hid_t               dxpl_id, 
               const H5C_class_t * type, 
               haddr_t             addr,
               const void *        udata1, 
               void *              udata2,
               hbool_t		   skip_file_checks)
{
    void *		thing = NULL;
    void *		ret_value = NULL;
    H5C_cache_entry_t *	entry_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5C_load_entry)

    HDassert( skip_file_checks || f );
    HDassert( type );
    HDassert( type->load );
    HDassert( type->size );
    HDassert( H5F_addr_defined(addr) );

    if ( NULL == (thing = (type->load)(f, dxpl_id, addr, udata1, udata2)) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTLOAD, NULL, "unable to load entry")

    }

    entry_ptr = (H5C_cache_entry_t *)thing;

    HDassert( entry_ptr->is_dirty == FALSE );

    entry_ptr->addr = addr;
    entry_ptr->type = type;
    entry_ptr->is_protected = FALSE;

    if ( (type->size)(f, thing, &(entry_ptr->size)) < 0 ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGETSIZE, NULL, \
                    "Can't get size of thing")
    }

    HDassert( entry_ptr->size < H5C_MAX_ENTRY_SIZE ); 

    entry_ptr->next = NULL;
    entry_ptr->prev = NULL;
    entry_ptr->aux_next = NULL;
    entry_ptr->aux_prev = NULL;

    H5C__RESET_CACHE_ENTRY_STATS(entry_ptr);

    ret_value = thing;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_load_entry() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_make_space_in_cache
 *
 * Purpose:     Attempt to evict cache entries until the index_size
 *		is at least needed_space below max_cache_size.
 *
 *		In passing, also attempt to bring cLRU_list_size to a 
 *		value greater than min_clean_size.
 *
 *		Depending on circumstances, both of these goals may
 *		be impossible, as in parallel mode, we must avoid generating
 *		a write as part of a read (to avoid deadlock in collective
 *		I/O), and in all cases, it is possible (though hopefully
 *		highly unlikely) that the protected list may exceed the 
 *		maximum size of the cache.
 *		
 *		Thus the function simply does its best, returning success
 *		unless an error is encountered.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters 
 *		specify the dxpl_ids used on the first write occasioned
 *		by the call (primary_dxpl_id), and on all subsequent 
 *		writes (secondary_dxpl_id).  This is useful in the metadata 
 *		cache, but may not be needed elsewhere.  If so, just use the 
 *		same dxpl_id for both parameters.
 *
 *		Observe that this function cannot occasion a read.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/14/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_make_space_in_cache(H5F_t *	f,
                        hid_t	primary_dxpl_id,
                        hid_t	secondary_dxpl_id,
                        H5C_t *	cache_ptr,
		        size_t	space_needed,
                        hbool_t	write_permitted)
{
    hbool_t		first_flush = TRUE;
    herr_t		ret_value = SUCCEED;      /* Return value */
    herr_t		result;
    int32_t		entries_examined = 0;
    int32_t		initial_list_len;
    H5C_cache_entry_t *	entry_ptr;
    H5C_cache_entry_t *	prev_ptr;

    FUNC_ENTER_NOAPI_NOINIT(H5C_make_space_in_cache)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( write_permitted ) {

        initial_list_len = cache_ptr->LRU_list_len;
        entry_ptr = cache_ptr->LRU_tail_ptr;

        while ( ( (cache_ptr->index_size + space_needed) 
                  > 
                  cache_ptr->max_cache_size
                )
                &&
                ( entries_examined <= (2 * initial_list_len) )
                &&
                ( entry_ptr != NULL )
              )
        {
            HDassert( ! (entry_ptr->is_protected) );

            prev_ptr = entry_ptr->prev;

            if ( entry_ptr->is_dirty ) {

                result = H5C_flush_single_entry(f, 
                                                primary_dxpl_id, 
                                                secondary_dxpl_id,
                                                cache_ptr,
                                                entry_ptr->type, 
                                                entry_ptr->addr, 
                                                (unsigned)0, 
                                                NULL, 
                                                &first_flush, 
                                                FALSE);
            } else {

                result = H5C_flush_single_entry(f, 
                                                primary_dxpl_id, 
                                                secondary_dxpl_id,
                                                cache_ptr,
                                                entry_ptr->type, 
                                                entry_ptr->addr, 
                                                H5F_FLUSH_INVALIDATE,
                                                NULL, 
                                                &first_flush, 
                                                TRUE);
            }

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }

            entry_ptr = prev_ptr;
        }

        initial_list_len = cache_ptr->dLRU_list_len;
        entry_ptr = cache_ptr->dLRU_tail_ptr;

        while ( ( cache_ptr->cLRU_list_size < cache_ptr->min_clean_size ) &&
                ( entries_examined <= initial_list_len ) &&
                ( entry_ptr != NULL )
              )
        {
            HDassert( ! (entry_ptr->is_protected) );
            HDassert( entry_ptr->is_dirty );

            prev_ptr = entry_ptr->aux_prev;

            result = H5C_flush_single_entry(f, 
                                            primary_dxpl_id, 
                                            secondary_dxpl_id,
                                            cache_ptr,
                                            entry_ptr->type, 
                                            entry_ptr->addr, 
                                            (unsigned)0, 
                                            NULL, 
                                            &first_flush, 
                                            FALSE);

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }

            entry_ptr = prev_ptr;
        }
    } else {

        initial_list_len = cache_ptr->cLRU_list_len;
        entry_ptr = cache_ptr->cLRU_tail_ptr;

        while ( ( (cache_ptr->index_size + space_needed) 
                  > 
                  cache_ptr->max_cache_size
                )
                &&
                ( entries_examined <= initial_list_len ) 
                &&
                ( entry_ptr != NULL )
              )
        {
            HDassert( ! (entry_ptr->is_protected) );
            HDassert( ! (entry_ptr->is_dirty) );

            prev_ptr = entry_ptr->aux_prev;

            result = H5C_flush_single_entry(f, 
                                            primary_dxpl_id, 
                                            secondary_dxpl_id,
                                            cache_ptr,
                                            entry_ptr->type, 
                                            entry_ptr->addr, 
                                            H5F_FLUSH_INVALIDATE,
                                            NULL, 
                                            &first_flush, 
                                            TRUE);

            if ( result < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush entry")
            }

            entry_ptr = prev_ptr;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_make_space_in_cache() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_remove_entry_from_tree
 *
 * Purpose:     Remove the specified instance of H5C_cache_entry_t from the 
 *		index tree in the specified instance of H5C_t.  Update
 *		the associated length and size fields.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_remove_entry_from_tree(H5C_t *	       cache_ptr,
		           H5C_cache_entry_t * entry_ptr,
                           H5TB_NODE *	       node_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C_remove_entry_from_tree)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( entry_ptr );
    HDassert( !entry_ptr->is_protected);
    HDassert( node_ptr );
    HDassert( node_ptr->data == entry_ptr );
    HDassert( entry_ptr->size > 0 );

    if ( H5TB_rem(&(cache_ptr->index_tree_ptr->root), node_ptr, NULL) 
         != entry_ptr ) {
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                    "Can't delete entry from tree.")
    } else {
        HDassert( cache_ptr->index_len > 0 );

        cache_ptr->index_len--;

        HDassert( cache_ptr->index_size >= entry_ptr->size );

        cache_ptr->index_size -= entry_ptr->size;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_remove_entry_from_tree */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_update_rp_for_eviction
 *
 * Purpose:     Update the replacement policy data structures for an 
 *		eviction of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_update_rp_for_eviction(H5C_t *		cache_ptr,
			    H5C_cache_entry_t * entry_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C_update_rp_for_eviction)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( entry_ptr );
    HDassert( !entry_ptr->is_protected);
    HDassert( entry_ptr->size > 0 );

    /* modified LRU specific code */

    /* remove the entry from the LRU list. */

    H5C__DLL_REMOVE(entry_ptr, cache_ptr->LRU_head_ptr, \
                     cache_ptr->LRU_tail_ptr, cache_ptr->LRU_list_len, \
                     cache_ptr->LRU_list_size)

    /* If the entry is clean when it is evicted, it should be on the 
     * clean LRU list, if it was dirty, it should be on the dirty LRU list.  
     * Remove it from the appropriate list according to the value of the 
     * dirty flag.
     */

    if ( entry_ptr->is_dirty ) {

        H5C__AUX_DLL_REMOVE(entry_ptr, cache_ptr->dLRU_head_ptr, \
                            cache_ptr->dLRU_tail_ptr, \
                            cache_ptr->dLRU_list_len, \
                            cache_ptr->dLRU_list_size)
    } else {
        H5C__AUX_DLL_REMOVE(entry_ptr, cache_ptr->cLRU_head_ptr, \
                             cache_ptr->cLRU_tail_ptr, \
                             cache_ptr->cLRU_list_len, \
                             cache_ptr->cLRU_list_size)
    }

#if H5C_DO_SANITY_CHECKS
done:
#endif /* H5C_DO_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_update_rp_for_eviction() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_update_rp_for_flush
 *
 * Purpose:     Update the replacement policy data structures for a flush 
 *		of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/6/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_update_rp_for_flush(H5C_t *		    cache_ptr,
		        H5C_cache_entry_t * entry_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C_update_rp_for_flush)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( entry_ptr );
    HDassert( !entry_ptr->is_protected);
    HDassert( entry_ptr->size > 0 );

    /* modified LRU specific code */

    /* remove the entry from the LRU list, and re-insert it at the head. */

    H5C__DLL_REMOVE(entry_ptr, cache_ptr->LRU_head_ptr, \
                     cache_ptr->LRU_tail_ptr, cache_ptr->LRU_list_len, \
                     cache_ptr->LRU_list_size)

    H5C__DLL_PREPEND(entry_ptr, cache_ptr->LRU_head_ptr, \
                      cache_ptr->LRU_tail_ptr, cache_ptr->LRU_list_len, \
                      cache_ptr->LRU_list_size)

    /* since the entry is being flushed or cleared, one would think that it 
     * must be dirty -- but that need not be the case.  Use the dirty flag
     * to infer whether the entry is on the clean or dirty LRU list, and 
     * remove it.  Then insert it at the head of the clean LRU list.  
     *
     * The function presumes that a dirty entry will be either cleared or 
     * flushed shortly, so it is OK if we put a dirty entry on the clean
     * LRU list.
     */

    if ( entry_ptr->is_dirty ) {
        H5C__AUX_DLL_REMOVE(entry_ptr, cache_ptr->dLRU_head_ptr, \
                            cache_ptr->dLRU_tail_ptr, \
                            cache_ptr->dLRU_list_len, \
                            cache_ptr->dLRU_list_size)
    } else {
        H5C__AUX_DLL_REMOVE(entry_ptr, cache_ptr->cLRU_head_ptr, \
                            cache_ptr->cLRU_tail_ptr, \
                            cache_ptr->cLRU_list_len, \
                            cache_ptr->cLRU_list_size)
    }

    H5C__AUX_DLL_PREPEND(entry_ptr, cache_ptr->cLRU_head_ptr, \
                          cache_ptr->cLRU_tail_ptr, cache_ptr->cLRU_list_len, \
                          cache_ptr->cLRU_list_size)

#if H5C_DO_SANITY_CHECKS
done:
#endif /* H5C_DO_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_update_rp_for_flush() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_update_rp_for_insertion
 *
 * Purpose:     Update the replacement policy data structures for an
 *		insertion of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/17/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_update_rp_for_insertion(H5C_t *		cache_ptr,
			    H5C_cache_entry_t * entry_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C_update_rp_for_insertion)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( entry_ptr );
    HDassert( !entry_ptr->is_protected);
    HDassert( entry_ptr->size > 0 );

    /* modified LRU specific code */

    /* insert the entry at the head of the LRU list. */

    H5C__DLL_PREPEND(entry_ptr, cache_ptr->LRU_head_ptr, \
                     cache_ptr->LRU_tail_ptr, cache_ptr->LRU_list_len, \
                     cache_ptr->LRU_list_size)

    /* insert the entry at the head of the clean or dirty LRU list as 
     * appropriate.
     */

    if ( entry_ptr->is_dirty ) {
        H5C__AUX_DLL_PREPEND(entry_ptr, cache_ptr->dLRU_head_ptr, \
                             cache_ptr->dLRU_tail_ptr, \
                             cache_ptr->dLRU_list_len, \
                             cache_ptr->dLRU_list_size)
    } else {
        H5C__AUX_DLL_PREPEND(entry_ptr, cache_ptr->cLRU_head_ptr, \
                              cache_ptr->cLRU_tail_ptr, \
                              cache_ptr->cLRU_list_len, \
                              cache_ptr->cLRU_list_size)
    }

#if H5C_DO_SANITY_CHECKS
done:
#endif /* H5C_DO_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_update_rp_for_insertion() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_update_rp_for_protect
 *
 * Purpose:     Update the replacement policy data structures for a 
 *		protect of the specified cache entry.
 *
 *		To do this, unlink the specified entry from any data 
 *		structures used by the replacement policy, and add the
 *		entry to the protected list.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/17/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_update_rp_for_protect(H5C_t *	      cache_ptr,
		          H5C_cache_entry_t * entry_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C_update_rp_for_protect)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( entry_ptr );
    HDassert( !entry_ptr->is_protected);
    HDassert( entry_ptr->size > 0 );

    /* modified LRU specific code */

    /* remove the entry from the LRU list. */

    H5C__DLL_REMOVE(entry_ptr, cache_ptr->LRU_head_ptr, \
                    cache_ptr->LRU_tail_ptr, cache_ptr->LRU_list_len, \
                    cache_ptr->LRU_list_size)

    /* Similarly, remove the entry from the clean or dirty LRU list
     * as appropriate.
     */

    if ( entry_ptr->is_dirty ) {

        H5C__AUX_DLL_REMOVE(entry_ptr, cache_ptr->dLRU_head_ptr, \
                            cache_ptr->dLRU_tail_ptr, \
                            cache_ptr->dLRU_list_len, \
                            cache_ptr->dLRU_list_size)

    } else {

        H5C__AUX_DLL_REMOVE(entry_ptr, cache_ptr->cLRU_head_ptr, \
                             cache_ptr->cLRU_tail_ptr, \
                             cache_ptr->cLRU_list_len, \
                             cache_ptr->cLRU_list_size)
    }

    /* End modified LRU specific code. */


    /* Regardless of the replacement policy, now add the entry to the 
     * protected list.
     */

    H5C__DLL_APPEND(entry_ptr, cache_ptr->pl_head_ptr, cache_ptr->pl_tail_ptr, \
                     cache_ptr->pl_len, cache_ptr->pl_size)

#if H5C_DO_SANITY_CHECKS
done:
#endif /* H5C_DO_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_update_rp_for_protect() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_update_rp_for_rename
 *
 * Purpose:     Update the replacement policy data structures for a 
 *		rename of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/17/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_update_rp_for_rename(H5C_t *	     cache_ptr,
		         H5C_cache_entry_t * entry_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C_update_rp_for_rename)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( entry_ptr );
    HDassert( !entry_ptr->is_protected);
    HDassert( entry_ptr->size > 0 );

    /* modified LRU specific code */

    /* remove the entry from the LRU list, and re-insert it at the head. */

    H5C__DLL_REMOVE(entry_ptr, cache_ptr->LRU_head_ptr, \
                    cache_ptr->LRU_tail_ptr, cache_ptr->LRU_list_len, \
                    cache_ptr->LRU_list_size)

    H5C__DLL_PREPEND(entry_ptr, cache_ptr->LRU_head_ptr, \
                     cache_ptr->LRU_tail_ptr, cache_ptr->LRU_list_len, \
                     cache_ptr->LRU_list_size)

    /* move the entry to the head of either the clean or dirty LRU list
     * as appropriate.
     */

    if ( entry_ptr->is_dirty ) {

        H5C__AUX_DLL_REMOVE(entry_ptr, cache_ptr->dLRU_head_ptr, \
                            cache_ptr->dLRU_tail_ptr, \
                            cache_ptr->dLRU_list_len, \
                            cache_ptr->dLRU_list_size)

        H5C__AUX_DLL_PREPEND(entry_ptr, cache_ptr->dLRU_head_ptr, \
                             cache_ptr->dLRU_tail_ptr, \
                             cache_ptr->dLRU_list_len, \
                             cache_ptr->dLRU_list_size)

    } else {

        H5C__AUX_DLL_REMOVE(entry_ptr, cache_ptr->cLRU_head_ptr, \
                            cache_ptr->cLRU_tail_ptr, \
                            cache_ptr->cLRU_list_len, \
                            cache_ptr->cLRU_list_size)

        H5C__AUX_DLL_PREPEND(entry_ptr, cache_ptr->cLRU_head_ptr, \
                             cache_ptr->cLRU_tail_ptr, \
                             cache_ptr->cLRU_list_len, \
                             cache_ptr->cLRU_list_size)
    }

#if H5C_DO_SANITY_CHECKS
done:
#endif /* H5C_DO_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_update_rp_for_rename() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_update_rp_for_unprotect
 *
 * Purpose:     Update the replacement policy data structures for an
 *		unprotect of the specified cache entry.
 *
 *		To do this, unlink the specified entry from the protected
 *		list, and re-insert it in the data structures used by the
 *		current replacement policy.
 *
 *		At present, we only support the modified LRU policy, so 
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/19/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_update_rp_for_unprotect(H5C_t *		cache_ptr,
		            H5C_cache_entry_t *	entry_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C_update_rp_for_unprotect)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( entry_ptr );
    HDassert( entry_ptr->is_protected);
    HDassert( entry_ptr->size > 0 );

    /* Regardless of the replacement policy, remove the entry from the 
     * protected list.
     */
    H5C__DLL_REMOVE(entry_ptr, cache_ptr->pl_head_ptr, \
                    cache_ptr->pl_tail_ptr, cache_ptr->pl_len, \
                    cache_ptr->pl_size)


    /* modified LRU specific code */

    /* insert the entry at the head of the LRU list. */

    H5C__DLL_PREPEND(entry_ptr, cache_ptr->LRU_head_ptr, \
                     cache_ptr->LRU_tail_ptr, cache_ptr->LRU_list_len, \
                     cache_ptr->LRU_list_size)

    /* Similarly, insert the entry at the head of either the clean or 
     * dirty LRU list as appropriate.
     */

    if ( entry_ptr->is_dirty ) {

        H5C__AUX_DLL_PREPEND(entry_ptr, cache_ptr->dLRU_head_ptr, \
                             cache_ptr->dLRU_tail_ptr, \
                             cache_ptr->dLRU_list_len, \
                             cache_ptr->dLRU_list_size)

    } else {

        H5C__AUX_DLL_PREPEND(entry_ptr, cache_ptr->cLRU_head_ptr, \
                             cache_ptr->cLRU_tail_ptr, \
                             cache_ptr->cLRU_list_len, \
                             cache_ptr->cLRU_list_size)
    }

    /* End modified LRU specific code. */

#if H5C_DO_SANITY_CHECKS
done:
#endif /* H5C_DO_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_update_rp_for_unprotect() */

