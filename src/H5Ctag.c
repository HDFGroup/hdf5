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
 * Created:     H5Ctag.c
 *              June 5 2016
 *              Quincey Koziol
 *
 * Purpose:     Functions in this file operate on tags for metadata
 *              cache entries.
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
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* Files				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Pprivate.h"         /* Property lists                       */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Typedef for tagged entry iterator callback context - evict tagged entries */
typedef struct {
    H5F_t * f;                          /* File pointer for evicting entry */
    hid_t dxpl_id;                      /* DXPL for evicting entry */
    hbool_t evicted_entries_last_pass;  /* Flag to indicate that an entry was evicted when iterating over cache */
    hbool_t pinned_entries_need_evicted;        /* Flag to indicate that a pinned entry was attempted to be evicted */
} H5C_tag_iter_evict_ctx_t;

/* Typedef for tagged entry iterator callback context - retag tagged entries */
typedef struct {
    haddr_t dest_tag;                   /* New tag value for matching entries */
} H5C_tag_iter_retag_ctx_t;

/* Typedef for tagged entry iterator callback context - expunge tag type metadata */
typedef struct {
    H5F_t * f;                          /* File pointer for evicting entry */
    hid_t dxpl_id;                      /* DXPL for evicting entry */
    int type_id;                        /* Cache entry type to expunge */
    unsigned flags;                     /* Flags for expunging entry */
} H5C_tag_iter_ettm_ctx_t;

/* Typedef for tagged entry iterator callback context - mark corked */
typedef struct {
    hbool_t cork_val;                   /* Corked value */
} H5C_tag_iter_cork_ctx_t;


/********************/
/* Local Prototypes */
/********************/
static herr_t H5C__mark_tagged_entries(H5C_t *cache_ptr, haddr_t tag);


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
 *
 * Function:    H5C_ignore_tags
 *
 * Purpose:     Override all assertion frameworks associated with making
 *              sure proper tags are applied to cache entries. 
 *
 *              NOTE: This should really only be used in tests that need 
 *              to access internal functions without going through 
 *              standard API paths. Since tags are set inside dxpl_id's
 *              before coming into the cache, any external functions that
 *              use the internal library functions (i.e., tests) should
 *              use this function if they don't plan on setting up proper
 *              metadata tags.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              December 1, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_ignore_tags(H5C_t * cache_ptr)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Assertions */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Set variable to ignore tag values upon assignment */
    cache_ptr->ignore_tags = TRUE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5C_ignore_tags */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_get_ignore_tags
 *
 * Purpose:     Retrieve the 'ignore_tags' field for the cache
 *
 * Return:      'ignore_tags' value (can't fail)
 *
 * Programmer:  Quincey Koziol
 *              April 30, 2016
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5C_get_ignore_tags(const H5C_t *cache_ptr)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Return ignore tag value */
    FUNC_LEAVE_NOAPI(cache_ptr->ignore_tags)
} /* H5C_get_ignore_tags */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__tag_entry
 *
 * Purpose:     Tags an entry with the provided tag (contained in the dxpl_id).
 *              If sanity checking is enabled, this function will perform 
 *              validation that a proper tag is contained within the provided 
 *              data access property list id before application.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              January 14, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C__tag_entry(H5C_t *cache, H5C_cache_entry_t *entry, hid_t dxpl_id)
{
    H5P_genplist_t *dxpl;       /* dataset transfer property list */
    H5C_tag_t tag;              /* Tag structure */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_PACKAGE

    /* Assertions */
    HDassert(cache != NULL);
    HDassert(entry != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Get the dataset transfer property list */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Get the tag from the DXPL */
    if((H5P_get(dxpl, "H5C_tag", &tag)) < 0)
	HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to query property value")

    if(cache->ignore_tags) {
        /* if we're ignoring tags, it's because we're running
           tests on internal functions and may not have inserted a tag 
           value into a given dxpl_id before creating some metadata. Thus,
           in this case only, if a tag value has not been set, we can
           arbitrarily set it to something for the sake of passing the tests. 
           If the tag value is set, then we'll just let it get assigned without
           additional checking for correctness. */
        if(!tag.value) {
            tag.value = H5AC__IGNORE_TAG;
            tag.globality = H5C_GLOBALITY_NONE;
        } /* end if */
    } /* end if */
#if H5C_DO_TAGGING_SANITY_CHECKS
    else {
        /* Perform some sanity checks to ensure that a correct tag is being applied */
        if(H5C_verify_tag(entry->type->id, tag.value, tag.globality) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "tag verification failed")
    } /* end else */
#endif

    /* Apply the tag to the entry */
    entry->tag = tag.value;

    /* Apply the tag globality to the entry */
    entry->globality = tag.globality;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__tag_entry */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_iter_tagged_entries
 *
 * Purpose:     Iterate over tagged entries, making a callback for matches
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Quincey Koziol
 *              June 7, 2016
 *
 *-------------------------------------------------------------------------
 */
int
H5C__iter_tagged_entries(H5C_t *cache, haddr_t tag, hbool_t match_global,
    H5C_tag_iter_cb_t cb, void *cb_ctx)
{
    unsigned u;                         /* Local index variable */
    int ret_value = H5_ITER_CONT;       /* Return value */

    /* Function enter macro */
    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    HDassert(cache != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Iterate through entries in the index. */
    for(u = 0; u < H5C__HASH_TABLE_LEN; u++) {
        H5C_cache_entry_t *entry;       /* Pointer to current entry */
        H5C_cache_entry_t *next_entry;  /* Pointer to next entry in hash bucket chain */

        next_entry = cache->index[u];
        while(next_entry != NULL) {
            /* Acquire pointer to current entry and to next entry */
            entry = next_entry;
            next_entry = entry->ht_next;

            /* Check for entry matching tag and/or globality */
            if((entry->tag == tag) || (match_global && entry->globality == H5C_GLOBALITY_MAJOR)) {
                /* Make callback for entry */
                if((ret_value = (cb)(entry, cb_ctx)) != H5_ITER_CONT)
                    HGOTO_ERROR(H5E_CACHE, H5E_BADITER, H5_ITER_ERROR, "Iteration of tagged entries failed")
            } /* end if */
        } /* end while */
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__iter_tagged_entries() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__evict_tagged_entries_cb
 *
 * Purpose:     Callback for evicting tagged entries
 *
 * Return:      H5_ITER_ERROR if error is detected, H5_ITER_CONT otherwise.
 *
 * Programmer:  Mike McGreevy
 *              August 19, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
H5C__evict_tagged_entries_cb(H5C_cache_entry_t *entry, void *_ctx)
{
    H5C_tag_iter_evict_ctx_t *ctx = (H5C_tag_iter_evict_ctx_t *)_ctx; /* Get pointer to iterator context */
    int ret_value = H5_ITER_CONT;       /* Return value */

    /* Function enter macro */
    FUNC_ENTER_STATIC

    /* Santify checks */
    HDassert(entry);
    HDassert(ctx);

    /* Attempt to evict entry */
    if(entry->is_protected)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, H5_ITER_ERROR, "Cannot evict protected entry")
    else if(entry->is_dirty)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, H5_ITER_ERROR, "Cannot evict dirty entry")
    else if(entry->is_pinned)
        /* Can't evict at this time, but let's note that we hit a pinned
            entry and we'll loop back around again (as evicting other
            entries will hopefully unpin this entry) */
        ctx->pinned_entries_need_evicted = TRUE;
    else
        /* Evict the Entry */
        if(H5C__flush_single_entry(ctx->f, ctx->dxpl_id, entry, H5C__FLUSH_INVALIDATE_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, H5_ITER_ERROR, "Entry eviction failed.")
    ctx->evicted_entries_last_pass = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__evict_tagged_entries_cb() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_evict_tagged_entries
 *
 * Purpose:     Evicts all entries with the specified tag from cache
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              August 19, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_evict_tagged_entries(H5F_t * f, hid_t dxpl_id, haddr_t tag, hbool_t match_global)
{
    H5C_t *cache;                   /* Pointer to cache structure */
    H5C_tag_iter_evict_ctx_t ctx;   /* Context for iterator callback */
    herr_t ret_value = SUCCEED;     /* Return value */

    /* Function enter macro */
    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    cache = f->shared->cache;   /* Get cache pointer */
    HDassert(cache != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Construct context for iterator callbacks */
    ctx.f = f;
    ctx.dxpl_id = dxpl_id;

    /* Start evicting entries */
    do {
        /* Reset pinned/evicted tracking flags */
        ctx.pinned_entries_need_evicted = FALSE;
        ctx.evicted_entries_last_pass = FALSE;

        /* Iterate through entries in the cache */
        if(H5C__iter_tagged_entries(cache, tag, match_global, H5C__evict_tagged_entries_cb, &ctx) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "Iteration of tagged entries failed")

        /* Keep doing this until we have stopped evicted entries */
    } while(TRUE == ctx.evicted_entries_last_pass);

    /* Fail if we have finished evicting entries and pinned entries still need evicted */
    if(ctx.pinned_entries_need_evicted)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Pinned entries still need evicted?!")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_evict_tagged_entries() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__mark_tagged_entries_cb
 *
 * Purpose:     Callback to set the flush marker on dirty entries in the cache
 *
 * Return:      H5_ITER_CONT (can't fail)
 *
 * Programmer:  Mike McGreevy
 *              September 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
H5C__mark_tagged_entries_cb(H5C_cache_entry_t *entry, void H5_ATTR_UNUSED *_ctx)
{
    /* Function enter macro */
    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(entry);

    /* We only want to set the flush marker on entries that
     * actually need flushed (i.e., dirty ones) */
    if(entry->is_dirty)
        entry->flush_marker = TRUE;

    FUNC_LEAVE_NOAPI(H5_ITER_CONT)
} /* H5C__mark_tagged_entries_cb() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__mark_tagged_entries
 *
 * Purpose:     Set the flush marker on dirty entries in the cache that have
 *              the specified tag, as well as all globally tagged entries.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              September 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5C__mark_tagged_entries(H5C_t *cache, haddr_t tag)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Function enter macro */
    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Iterate through hash table entries, marking those with specified tag, as
     * well as any major global entries which should always be flushed
     * when flushing based on tag value */
    if(H5C__iter_tagged_entries(cache, tag, TRUE, H5C__mark_tagged_entries_cb, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "Iteration of tagged entries failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__mark_tagged_entries() */

#if H5C_DO_TAGGING_SANITY_CHECKS

/*-------------------------------------------------------------------------
 *
 * Function:    H5C_verify_tag
 *
 * Purpose:     Performs sanity checking on an entrytype/tag pair.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              January 14, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_verify_tag(int id, haddr_t tag, H5C_tag_globality_t globality)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Perform some sanity checks on tag value. Certain entry
     * types require certain tag values, so check that these
     * constraints are met. */
    if(tag == H5AC__IGNORE_TAG)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "cannot ignore a tag while doing verification.")
    else if(tag == H5AC__INVALID_TAG)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "no metadata tag provided")
    else {
        /* Perform some sanity checks on tag value. Certain entry
         * types require certain tag values, so check that these
         * constraints are met. */

        /* Superblock */
        if((id == H5AC_SUPERBLOCK_ID) || (id == H5AC_DRVRINFO_ID)) {
            if(tag != H5AC__SUPERBLOCK_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "superblock not tagged with H5AC__SUPERBLOCK_TAG")
            if(globality != H5C_GLOBALITY_MAJOR)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "superblock/driver-info globality not marked with H5C_GLOBALITY_MAJOR")
        } /* end if */
        else {
            if(tag == H5AC__SUPERBLOCK_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__SUPERBLOCK_TAG applied to non-superblock entry")
        } /* end else */
    
        /* Free Space Manager */
        if((id == H5AC_FSPACE_HDR_ID) || (id == H5AC_FSPACE_SINFO_ID)) {
            if(tag != H5AC__FREESPACE_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "freespace entry not tagged with H5AC__FREESPACE_TAG")
            if(globality != H5C_GLOBALITY_MINOR)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "freespace entry globality not marked with H5C_GLOBALITY_MINOR")
        } /* end if */
        else {
            if(tag == H5AC__FREESPACE_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__FREESPACE_TAG applied to non-freespace entry")
        } /* end else */
    
        /* SOHM */
        if((id == H5AC_SOHM_TABLE_ID) || (id == H5AC_SOHM_LIST_ID)) { 
            if(tag != H5AC__SOHM_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "sohm entry not tagged with H5AC__SOHM_TAG")
            if(globality != H5C_GLOBALITY_MAJOR)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "sohm entry globality not marked with H5C_GLOBALITY_MAJOR")
        } /* end if */
    
        /* Global Heap */
        if(id == H5AC_GHEAP_ID) {
            if(tag != H5AC__GLOBALHEAP_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "global heap not tagged with H5AC__GLOBALHEAP_TAG")
            if(globality != H5C_GLOBALITY_MAJOR)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "global heap entry globality not marked with H5C_GLOBALITY_MAJOR")
        } /* end if */
        else {
            if(tag == H5AC__GLOBALHEAP_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__GLOBALHEAP_TAG applied to non-globalheap entry")
        } /* end else */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_verify_tag */
#endif


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_flush_tagged_entries
 *
 * Purpose:     Flushes all entries with the specified tag to disk.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              August 19, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_flush_tagged_entries(H5F_t * f, hid_t dxpl_id, haddr_t tag)
{
    /* Variable Declarations */
    H5C_t      *cache_ptr = NULL;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Assertions */
    HDassert(f);
    HDassert(f->shared);

    /* Get cache pointer */
    cache_ptr = f->shared->cache;

    /* Mark all entries with specified tag */
    if(H5C__mark_tagged_entries(cache_ptr, tag) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't mark tagged entries")

    /* Flush all marked entries */
    if(H5C__flush_marked_entries(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush marked entries")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_tagged_entries */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__retag_entries_cb
 *
 * Purpose:     Change tag for entries that match current tag
 *
 * Return:      H5_ITER_CONT (can't fail)
 *
 * Programmer:  Mike McGreevy
 *              March 17, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
H5C__retag_entries_cb(H5C_cache_entry_t *entry, void *_ctx)
{
    H5C_tag_iter_retag_ctx_t *ctx = (H5C_tag_iter_retag_ctx_t *)_ctx; /* Get pointer to iterator context */

    /* Function enter macro */
    FUNC_ENTER_STATIC_NOERR

    /* Santify checks */
    HDassert(entry);
    HDassert(ctx);

    entry->tag = ctx->dest_tag;

    FUNC_LEAVE_NOAPI(H5_ITER_CONT)
} /* H5C_retag_entries_cb() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_retag_entries
 *
 * Purpose:     Searches through cache index for all entries with the
 *              value specified by src_tag and changes it to the value
 *              specified by dest_tag.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              March 17, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_retag_entries(H5C_t *cache, haddr_t src_tag, haddr_t dest_tag) 
{
    H5C_tag_iter_retag_ctx_t ctx;       /* Iterator callback context */
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Function enter macro */
    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(cache);

    /* Construct context for iterator callbacks */
    ctx.dest_tag = dest_tag;

    /* Iterate through entries, retagging those with the src_tag tag */
    if(H5C__iter_tagged_entries(cache, src_tag, FALSE, H5C__retag_entries_cb, &ctx) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "Iteration of tagged entries failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_retag_entries() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__expunge_tag_type_metadata_cb
 *
 * Purpose:     Expunge from the cache entries associated
 *              with 'tag' and type id.
 *
 * Return:      H5_ITER_ERROR if error is detected, H5_ITER_CONT otherwise.
 *
 * Programmer:  Vailin Choi
 *		May 2016
 *
 *-------------------------------------------------------------------------
 */
static int
H5C__expunge_tag_type_metadata_cb(H5C_cache_entry_t *entry, void *_ctx)
{
    H5C_tag_iter_ettm_ctx_t *ctx = (H5C_tag_iter_ettm_ctx_t *)_ctx; /* Get pointer to iterator context */
    int ret_value = H5_ITER_CONT;       /* Return value */

    /* Function enter macro */
    FUNC_ENTER_STATIC

    /* Santify checks */
    HDassert(entry);
    HDassert(ctx);

    /* Found one with the same tag and type id */
    if(entry->type->id == ctx->type_id)
        if(H5C_expunge_entry(ctx->f, ctx->dxpl_id, entry->type, entry->addr, ctx->flags) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, H5_ITER_ERROR, "can't expunge entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__expunge_tag_type_metadata_cb() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_expunge_tag_type_metadata
 *
 * Purpose:     Search and expunge from the cache entries associated
 *              with 'tag' and type id.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Vailin Choi
 *		May 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5C_expunge_tag_type_metadata(H5F_t *f, hid_t dxpl_id, haddr_t tag, int type_id,
    unsigned flags)
{
    H5C_t *cache;                       /* Pointer to cache structure */
    H5C_tag_iter_ettm_ctx_t ctx;        /* Context for iterator callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Function enter macro */
    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    cache = f->shared->cache;   /* Get cache pointer */
    HDassert(cache != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Construct context for iterator callbacks */
    ctx.f = f;
    ctx.dxpl_id = dxpl_id;
    ctx.type_id = type_id;
    ctx.flags = flags;

    /* Iterate through hash table entries, expunge those with specified tag and type id */
    if(H5C__iter_tagged_entries(cache, tag, FALSE, H5C__expunge_tag_type_metadata_cb, &ctx) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "Iteration of tagged entries failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_expunge_tag_type_metadata() */


/*-------------------------------------------------------------------------
 * Function:    H5C__mark_tagged_entries_cork_cb
 *		
 * Purpose:     The "is_corked" field to "val" for en entry
 *
 * Return:      H5_ITER_ERROR if error is detected, H5_ITER_CONT otherwise.
 *
 * Programmer:  Vailin Choi
 *		January 2014
 *
 *-------------------------------------------------------------------------
 */
static int
H5C__mark_tagged_entries_cork_cb(H5C_cache_entry_t *entry, void *_ctx)
{
    H5C_tag_iter_cork_ctx_t *ctx = (H5C_tag_iter_cork_ctx_t *)_ctx; /* Get pointer to iterator context */

    /* Function enter macro */
    FUNC_ENTER_STATIC_NOERR

    /* Santify checks */
    HDassert(entry);
    HDassert(ctx);

    /* Set the entry's "corked" field to "val" */
    entry->is_corked = ctx->cork_val;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5C__mark_tagged_entries_cork_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5C__mark_tagged_entries_cork
 *		
 * Purpose:     To set the "is_corked" field to "val" for entries in cache 
 *		with the entry's tag equals to "obj_addr".
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Vailin Choi
 *		January 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5C__mark_tagged_entries_cork(H5C_t *cache, haddr_t obj_addr, hbool_t val)
{
    H5C_tag_iter_cork_ctx_t ctx;        /* Context for iterator callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Function enter macro */
    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Construct context for iterator callbacks */
    ctx.cork_val = val;

    /* Iterate through entries, find each entry with the specified tag */
    /* and set the entry's "corked" field to "val" */
    if(H5C__iter_tagged_entries(cache, obj_addr, FALSE, H5C__mark_tagged_entries_cork_cb, &ctx) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "Iteration of tagged entries failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__mark_tagged_entries_cork() */

