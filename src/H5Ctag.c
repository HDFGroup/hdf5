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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C__tag_entry(H5C_t * cache_ptr, H5C_cache_entry_t * entry_ptr, hid_t dxpl_id)
{
    H5P_genplist_t *dxpl;       /* dataset transfer property list */
    H5C_tag_t tag;              /* Tag structure */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_PACKAGE

    /* Assertions */
    HDassert(cache_ptr != NULL);
    HDassert(entry_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Get the dataset transfer property list */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Get the tag from the DXPL */
    if((H5P_get(dxpl, "H5C_tag", &tag)) < 0)
	HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to query property value")

    if(cache_ptr->ignore_tags != TRUE) {
#if H5C_DO_TAGGING_SANITY_CHECKS
        /* Perform some sanity checks to ensure that a correct tag is being applied */
        if(H5C_verify_tag(entry_ptr->type->id, tag.value, tag.globality) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "tag verification failed")
#endif
    } else {
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

    /* Apply the tag to the entry */
    entry_ptr->tag = tag.value;

    /* Apply the tag globality to the entry */
    entry_ptr->globality = tag.globality;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__tag_entry */


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
H5C__mark_tagged_entries(H5C_t * cache_ptr, haddr_t tag)
{
    unsigned u;                 /* Local index variable */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Iterate through hash table entries, marking those with specified tag, as
     * well as any major global entries which should always be flushed
     * when flushing based on tag value */
    for(u = 0; u < H5C__HASH_TABLE_LEN; u++) {
        H5C_cache_entry_t *entry_ptr;   /* Entry pointer */

	entry_ptr = cache_ptr->index[u];
	while(entry_ptr != NULL) {
	    if((entry_ptr->tag == tag) || (entry_ptr->globality == H5C_GLOBALITY_MAJOR)) {
		/* We only want to set the flush marker on entries that
		 * actually need flushed (i.e., dirty ones) */
		if(entry_ptr->is_dirty)
		    entry_ptr->flush_marker = TRUE;
	    } /* end if */

	    entry_ptr = entry_ptr->ht_next;
	} /* end while */
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5C__mark_tagged_entries */

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
void
H5C_retag_entries(H5C_t * cache_ptr, haddr_t src_tag, haddr_t dest_tag) 
{
    unsigned u;         /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(cache_ptr);

    /* Iterate through entries, retagging those with the src_tag tag */
    for(u = 0; u < H5C__HASH_TABLE_LEN; u++) {
        H5C_cache_entry_t *entry_ptr; /* entry pointer */

	entry_ptr = cache_ptr->index[u];
	while(entry_ptr) {
	    if(entry_ptr->tag == src_tag)
                entry_ptr->tag = dest_tag;
	    entry_ptr = entry_ptr->ht_next;
	} /* end while */
    } /* end for */

    FUNC_LEAVE_NOAPI_VOID
} /* H5C_retag_entries */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_expunge_tag_type_metadata
 *
 * Purpose:     Search and expunge from the cache entries associated
 *              with 'tag' and type id.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Vailin Choi; May 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5C_expunge_tag_type_metadata(H5F_t *f, hid_t dxpl_id, haddr_t tag, int type_id, unsigned flags)
{
    unsigned u;                 	/* Local index variable */
    H5C_t *cache_ptr = NULL;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(f->shared->cache->magic == H5C__H5C_T_MAGIC);

    /* Get cache pointer */
    cache_ptr = f->shared->cache;

    /* Iterate through hash table entries, expunge those with specified tag and type id */
    for(u = 0; u < H5C__HASH_TABLE_LEN; u++) {
        H5C_cache_entry_t *entry_ptr;   /* Entry pointer */

	entry_ptr = cache_ptr->index[u];
	while(entry_ptr != NULL) {
	    H5C_cache_entry_t *next_entry_ptr = entry_ptr->ht_next;

	    /* Found one with the same tag and type id */
	    if(entry_ptr->tag == tag && entry_ptr->type->id == type_id) {

		if(H5C_expunge_entry(f, dxpl_id, entry_ptr->type, entry_ptr->addr, flags) < 0)
		    HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, FAIL, "H5C_expunge_entry() failed.")
	    } /* end if */

	    entry_ptr = next_entry_ptr;
	} /* end while */
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_expunge_tag_type_metadata */

