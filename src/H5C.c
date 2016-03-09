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
 *		header comment for H5C_t in H5Cpkg.h.
 *
 *-------------------------------------------------------------------------
 */

/**************************************************************************
 *
 *				To Do:
 *
 *	Code Changes:
 *
 *	 - Remove extra functionality in H5C__flush_single_entry()?
 *
 *	 - Change protect/unprotect to lock/unlock.
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
 *	 - Now that TBBT routines aren't used, fix nodes in memory to
 *         point directly to the skip list node from the LRU list, eliminating
 *         skip list lookups when evicting objects from the cache.
 *
 *	Tests:
 *
 *	 - Trim execution time.  (This is no longer a major issue with the
 *	   shift from the TBBT to a hash table for indexing.)
 *
 *	 - Add random tests.
 *
 **************************************************************************/

/****************/
/* Module Setup */
/****************/

#include "H5Cmodule.h"          /* This source code file is part of the H5C module */
#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#ifdef H5_HAVE_PARALLEL
#define H5AC_FRIEND		/*suppress error about including H5ACpkg	  */
#include "H5ACpkg.h"        /* Metadata cache                       */
#endif /* H5_HAVE_PARALLEL */
#include "H5Cpkg.h"		/* Cache				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* Files				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property lists                       */


/****************/
/* Local Macros */
/****************/
#if H5C_DO_MEMORY_SANITY_CHECKS
#define H5C_IMAGE_EXTRA_SPACE 8
#define H5C_IMAGE_SANITY_VALUE "DeadBeef"
#else /* H5C_DO_MEMORY_SANITY_CHECKS */
#define H5C_IMAGE_EXTRA_SPACE 0
#endif /* H5C_DO_MEMORY_SANITY_CHECKS */


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5C__auto_adjust_cache_size(H5F_t * f,
                                          hid_t dxpl_id,
                                          hbool_t write_permitted);

static herr_t H5C__autoadjust__ageout(H5F_t * f,
                                      hid_t dxpl_id,
                                      double hit_rate,
                                      enum H5C_resize_status * status_ptr,
                                      size_t * new_max_cache_size_ptr,
                                      hbool_t write_permitted);

static herr_t H5C__autoadjust__ageout__cycle_epoch_marker(H5C_t * cache_ptr);

static herr_t H5C__autoadjust__ageout__evict_aged_out_entries(H5F_t * f,
                                                       hid_t dxpl_id,
                                                       hbool_t write_permitted);

static herr_t H5C__autoadjust__ageout__insert_new_marker(H5C_t * cache_ptr);

static herr_t H5C__autoadjust__ageout__remove_all_markers(H5C_t * cache_ptr);

static herr_t H5C__autoadjust__ageout__remove_excess_markers(H5C_t * cache_ptr);

static herr_t H5C__flash_increase_cache_size(H5C_t * cache_ptr,
                                             size_t old_entry_size,
                                             size_t new_entry_size);

static herr_t H5C_flush_invalidate_cache(const H5F_t *  f,
                                          hid_t    dxpl_id,
			                  unsigned flags);

static herr_t H5C_flush_invalidate_ring(const H5F_t * f, hid_t dxpl_id,
    H5C_ring_t ring, unsigned flags);

static herr_t H5C_flush_ring(H5F_t *f, hid_t dxpl_id, H5C_ring_t ring,
    unsigned flags);

static void * H5C_load_entry(H5F_t *             f,
                             hid_t               dxpl_id,
#ifdef H5_HAVE_PARALLEL
                             hbool_t             coll_access,
#endif /* H5_HAVE_PARALLEL */
                             const H5C_class_t * type,
                             haddr_t             addr,
                             void *              udata);

static herr_t H5C_make_space_in_cache(H5F_t * f,
                                      hid_t   dxpl_id,
       	                              size_t  space_needed,
                                      hbool_t write_permitted);

static herr_t H5C_tag_entry(H5C_t * cache_ptr, 
                            H5C_cache_entry_t * entry_ptr,
                            hid_t dxpl_id);

static herr_t H5C_mark_tagged_entries(H5C_t * cache_ptr,
                                      haddr_t tag,
                                      hbool_t mark_clean);

static herr_t H5C_mark_tagged_entries_cork(H5C_t *cache_ptr,
                                           haddr_t obj_addr,
                                           hbool_t val);

static herr_t H5C_flush_marked_entries(H5F_t * f, 
                                       hid_t dxpl_id);

static herr_t H5C__mark_flush_dep_dirty(H5C_cache_entry_t * entry);

static herr_t H5C__mark_flush_dep_clean(H5C_cache_entry_t * entry);

static herr_t H5C_verify_len_eoa (H5F_t *f, 
				  const H5C_class_t * type, 
				  haddr_t addr, 
				  size_t *len, 
				  htri_t actual);

static herr_t H5C__generate_image(const H5F_t *f, H5C_t * cache_ptr, H5C_cache_entry_t *entry_ptr, 
                                  hid_t dxpl_id, int64_t *entry_size_change_ptr);

#if H5C_DO_TAGGING_SANITY_CHECKS
static herr_t H5C_verify_tag(int id, haddr_t tag);
#endif

#if H5C_DO_SLIST_SANITY_CHECKS
static hbool_t H5C_entry_in_skip_list(H5C_t * cache_ptr, 
                                      H5C_cache_entry_t *target_ptr);
#endif /* H5C_DO_SLIST_SANITY_CHECKS */

#if H5C_DO_EXTREME_SANITY_CHECKS
static herr_t H5C_validate_lru_list(H5C_t * cache_ptr);
static herr_t H5C_validate_pinned_entry_list(H5C_t * cache_ptr);
static herr_t H5C_validate_protected_entry_list(H5C_t * cache_ptr);
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

#ifndef NDEBUG
static void H5C__assert_flush_dep_nocycle(H5C_cache_entry_t * entry,
                                          H5C_cache_entry_t * base_entry);
#endif /* NDEBUG */

#if 0 /* debugging routines */
herr_t H5C_dump_cache(H5C_t * cache_ptr, const char *  cache_name);
herr_t H5C_dump_cache_skip_list(H5C_t * cache_ptr, char * calling_fcn);
#endif /* debugging routines */

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

/* Declare a free list to manage the H5C_t struct */
H5FL_DEFINE_STATIC(H5C_t);

/* Declare a free list to manage flush dependency arrays */
H5FL_BLK_DEFINE_STATIC(parent);

/* Declare extern free list to manage the H5C_collective_write_t struct */
H5FL_EXTERN(H5C_collective_write_t);

/* Declare a free list to manage corked object addresses */
H5FL_DEFINE_STATIC(haddr_t);


/****************************************************************************
 *
 * #defines and declarations for epoch marker cache entries.
 *
 * As a strategy for automatic cache size reduction, the cache may insert
 * marker entries in the LRU list at the end of each epoch.  These markers
 * are then used to identify entries that have not been accessed for n
 * epochs so that they can be evicted from the cache.
 *
 ****************************************************************************/

/* Note that H5C__MAX_EPOCH_MARKERS is defined in H5Cpkg.h, not here because
 * it is needed to dimension arrays in H5C_t.
 */

#define H5C__EPOCH_MARKER_TYPE	H5C__MAX_NUM_TYPE_IDS

static herr_t H5C__epoch_marker_get_load_size(const void *image_ptr,
					      void *udata_ptr,
		                              size_t *image_len_ptr,
					      size_t *actual_len,
					      hbool_t *compressed_ptr,
					      size_t *compressed_len_ptr);
static htri_t H5C__epoch_marker_verify_chksum(const void *image_ptr, 
					      size_t len, 
					      void *udata_ptr);
static void * H5C__epoch_marker_deserialize(const void * image_ptr,
		                            size_t len,
			                    void * udata,
			                    hbool_t * dirty_ptr);
static herr_t H5C__epoch_marker_image_len(const void * thing,
		                          size_t *image_len_ptr,
                                          hbool_t *compressed_ptr,
                                          size_t *compressed_len_ptr);
static herr_t H5C__epoch_marker_pre_serialize(const H5F_t *f,
		                             hid_t dxpl_id,
                                             void * thing,
                                             haddr_t addr,
		                             size_t len,
					     size_t compressed_len,
		                             haddr_t * new_addr_ptr,
		                             size_t * new_len_ptr,
					     size_t * new_compressed_len_ptr,
		                             unsigned * flags_ptr);
static herr_t H5C__epoch_marker_serialize(const H5F_t *f,
		                          void * image_ptr,
		                          size_t len,
                                          void * thing);
static herr_t H5C__epoch_marker_notify(H5C_notify_action_t action, void *thing);
static herr_t H5C__epoch_marker_free_icr(void * thing);

static herr_t H5C__epoch_marker_clear(const H5F_t *f, void * thing, 
                                      hbool_t about_to_destroy);
static herr_t H5C__epoch_marker_fsf_size(const void H5_ATTR_UNUSED * thing, 
                                         size_t H5_ATTR_UNUSED * fsf_size_ptr);

const H5C_class_t epoch_marker_class =
{
    /* id               = */ H5C__EPOCH_MARKER_TYPE,
    /* name             = */ "epoch marker",
    /* mem_type         = */ H5FD_MEM_DEFAULT, /* value doesn't matter */
    /* flags		= */ H5AC__CLASS_NO_FLAGS_SET,
    /* get_load_size    = */ H5C__epoch_marker_get_load_size,
    /* verify_chksum    = */ H5C__epoch_marker_verify_chksum,
    /* deserialize      = */ H5C__epoch_marker_deserialize,
    /* image_len        = */ H5C__epoch_marker_image_len,
    /* pre_serialize    = */ H5C__epoch_marker_pre_serialize,
    /* serialize        = */ H5C__epoch_marker_serialize,
    /* notify           = */ H5C__epoch_marker_notify,
    /* free_icr         = */ H5C__epoch_marker_free_icr,
    /* clear            = */ H5C__epoch_marker_clear,
    /* fsf_size         = */ H5C__epoch_marker_fsf_size,
};



/***************************************************************************
 * Class functions for H5C__EPOCH_MAKER_TYPE:
 *
 * None of these functions should ever be called, so there is no point in
 * documenting them separately.
 *                                                     JRM - 11/16/04
 *
 ***************************************************************************/
static herr_t
H5C__epoch_marker_get_load_size(const void H5_ATTR_UNUSED *image_ptr, void H5_ATTR_UNUSED *udata_ptr,
    size_t H5_ATTR_UNUSED *image_len_ptr, size_t H5_ATTR_UNUSED *actual_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_len_ptr)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5C__epoch_marker_get_load_size() */


static htri_t 
H5C__epoch_marker_verify_chksum(const void H5_ATTR_UNUSED *image_ptr, size_t H5_ATTR_UNUSED len, 
    void H5_ATTR_UNUSED *udata_ptr)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FALSE)
} /* end H5C__epoch_marker_verify_chksum() */


static void *
H5C__epoch_marker_deserialize(const void H5_ATTR_UNUSED * image_ptr, size_t H5_ATTR_UNUSED len,
    void H5_ATTR_UNUSED * udata, hbool_t H5_ATTR_UNUSED * dirty_ptr)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(NULL)
} /* end H5C__epoch_marker_deserialize() */


static herr_t
H5C__epoch_marker_image_len(const void H5_ATTR_UNUSED *thing,
    size_t H5_ATTR_UNUSED *image_len_ptr, hbool_t H5_ATTR_UNUSED *compressed_ptr,
    size_t H5_ATTR_UNUSED *compressed_len_ptr)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5C__epoch_marker_image_len() */


static herr_t
H5C__epoch_marker_pre_serialize(const H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id,
    void H5_ATTR_UNUSED *thing, haddr_t H5_ATTR_UNUSED addr, size_t H5_ATTR_UNUSED len,
    size_t H5_ATTR_UNUSED compressed_len, haddr_t H5_ATTR_UNUSED *new_addr_ptr, 
    size_t H5_ATTR_UNUSED *new_len_ptr, size_t H5_ATTR_UNUSED *new_compressed_len_ptr,
    unsigned H5_ATTR_UNUSED *flags_ptr)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5C__epoch_marker_pre_serialize() */


static herr_t
H5C__epoch_marker_serialize(const H5F_t H5_ATTR_UNUSED *f, void H5_ATTR_UNUSED *image_ptr,
    size_t H5_ATTR_UNUSED len, void H5_ATTR_UNUSED *thing)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5C__epoch_marker_serialize() */


static herr_t
H5C__epoch_marker_notify(H5C_notify_action_t H5_ATTR_UNUSED action,
                       void H5_ATTR_UNUSED * thing)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5C__epoch_marker_notify() */


static herr_t
H5C__epoch_marker_free_icr(void H5_ATTR_UNUSED * thing)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5C__epoch_marker_free_icr() */


static herr_t 
H5C__epoch_marker_clear(const H5F_t H5_ATTR_UNUSED *f, void H5_ATTR_UNUSED * thing, hbool_t H5_ATTR_UNUSED about_to_destroy)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5C__epoch_marker_clear() */


static herr_t 
H5C__epoch_marker_fsf_size(const void H5_ATTR_UNUSED * thing, size_t H5_ATTR_UNUSED *fsf_size_ptr)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5C__epoch_marker_fsf_size() */



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
 *		If it is NULL, the cache will use the write_permitted
 *		flag to determine whether writes are permitted.
 *
 * Return:      Success:        Pointer to the new instance.
 *
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 *-------------------------------------------------------------------------
 */
H5C_t *
H5C_create(size_t		      max_cache_size,
           size_t		      min_clean_size,
           int			      max_type_id,
           const char *		      (* type_name_table_ptr),
           H5C_write_permitted_func_t check_write_permitted,
           hbool_t		      write_permitted,
           H5C_log_flush_func_t       log_flush,
           void *                     aux_ptr)
{
    int i;
    H5C_t * cache_ptr = NULL;
    H5C_t * ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    HDassert( max_cache_size >= H5C__MIN_MAX_CACHE_SIZE );
    HDassert( max_cache_size <= H5C__MAX_MAX_CACHE_SIZE );
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

    if ( (cache_ptr->slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)) == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, NULL, "can't create skip list.")
    }

    if ( (cache_ptr->cork_list_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)) == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, NULL, "can't create skip list for corked object addresses.")
    }

    /* If we get this far, we should succeed.  Go ahead and initialize all
     * the fields.
     */

    cache_ptr->magic 				= H5C__H5C_T_MAGIC;

    cache_ptr->flush_in_progress		= FALSE;

    cache_ptr->logging_enabled                  = FALSE;

    cache_ptr->currently_logging                = FALSE;

    cache_ptr->log_file_ptr			= NULL;

    cache_ptr->trace_file_ptr			= NULL;

    cache_ptr->aux_ptr				= aux_ptr;

    cache_ptr->max_type_id			= max_type_id;

    cache_ptr->type_name_table_ptr		= type_name_table_ptr;

    cache_ptr->max_cache_size			= max_cache_size;
    cache_ptr->min_clean_size			= min_clean_size;

    cache_ptr->check_write_permitted		= check_write_permitted;
    cache_ptr->write_permitted			= write_permitted;

    cache_ptr->log_flush			= log_flush;

    cache_ptr->evictions_enabled		= TRUE;

    cache_ptr->index_len			= 0;
    cache_ptr->index_size			= (size_t)0;
    cache_ptr->clean_index_size			= (size_t)0;
    cache_ptr->dirty_index_size			= (size_t)0;

    for(i = 0; i < H5C_RING_NTYPES; i++) {
	cache_ptr->index_ring_len[i]		= 0;
	cache_ptr->index_ring_size[i]		= (size_t)0;
	cache_ptr->clean_index_ring_size[i]	= (size_t)0;
	cache_ptr->dirty_index_ring_size[i]	= (size_t)0;

	cache_ptr->slist_ring_len[i]		= 0;
	cache_ptr->slist_ring_size[i]		= (size_t)0;
    } /* end for */

    /* Tagging Field Initializations */
    cache_ptr->ignore_tags                      = FALSE;

    cache_ptr->slist_changed			= FALSE;
    cache_ptr->slist_change_in_pre_serialize	= FALSE;
    cache_ptr->slist_change_in_serialize	= FALSE;
    cache_ptr->slist_len			= 0;
    cache_ptr->slist_size			= (size_t)0;

#if H5C_DO_SANITY_CHECKS
    cache_ptr->slist_len_increase		= 0;
    cache_ptr->slist_size_increase		= 0;
#endif /* H5C_DO_SANITY_CHECKS */

    for(i = 0; i < H5C__HASH_TABLE_LEN; i++)
        (cache_ptr->index)[i] = NULL;

    cache_ptr->entries_removed_counter		= 0;
    cache_ptr->last_entry_removed_ptr		= NULL;

    cache_ptr->pl_len				= 0;
    cache_ptr->pl_size				= (size_t)0;
    cache_ptr->pl_head_ptr			= NULL;
    cache_ptr->pl_tail_ptr			= NULL;

    cache_ptr->pel_len				= 0;
    cache_ptr->pel_size				= (size_t)0;
    cache_ptr->pel_head_ptr			= NULL;
    cache_ptr->pel_tail_ptr			= NULL;

    cache_ptr->LRU_list_len			= 0;
    cache_ptr->LRU_list_size			= (size_t)0;
    cache_ptr->LRU_head_ptr			= NULL;
    cache_ptr->LRU_tail_ptr			= NULL;

#ifdef H5_HAVE_PARALLEL
    cache_ptr->coll_list_len			= 0;
    cache_ptr->coll_list_size			= (size_t)0;
    cache_ptr->coll_head_ptr			= NULL;
    cache_ptr->coll_tail_ptr			= NULL;
#endif /* H5_HAVE_PARALLEL */

    cache_ptr->cLRU_list_len			= 0;
    cache_ptr->cLRU_list_size			= (size_t)0;
    cache_ptr->cLRU_head_ptr			= NULL;
    cache_ptr->cLRU_tail_ptr			= NULL;

    cache_ptr->dLRU_list_len			= 0;
    cache_ptr->dLRU_list_size			= (size_t)0;
    cache_ptr->dLRU_head_ptr			= NULL;
    cache_ptr->dLRU_tail_ptr			= NULL;

    cache_ptr->size_increase_possible		= FALSE;
    cache_ptr->flash_size_increase_possible     = FALSE;
    cache_ptr->flash_size_increase_threshold    = 0;
    cache_ptr->size_decrease_possible		= FALSE;
    cache_ptr->resize_enabled			= FALSE;
    cache_ptr->cache_full			= FALSE;
    cache_ptr->size_decreased			= FALSE;

    (cache_ptr->resize_ctl).version		= H5C__CURR_AUTO_SIZE_CTL_VER;
    (cache_ptr->resize_ctl).rpt_fcn		= NULL;
    (cache_ptr->resize_ctl).set_initial_size	= FALSE;
    (cache_ptr->resize_ctl).initial_size	= H5C__DEF_AR_INIT_SIZE;
    (cache_ptr->resize_ctl).min_clean_fraction	= H5C__DEF_AR_MIN_CLEAN_FRAC;
    (cache_ptr->resize_ctl).max_size		= H5C__DEF_AR_MAX_SIZE;
    (cache_ptr->resize_ctl).min_size		= H5C__DEF_AR_MIN_SIZE;
    (cache_ptr->resize_ctl).epoch_length	= H5C__DEF_AR_EPOCH_LENGTH;

    (cache_ptr->resize_ctl).incr_mode		= H5C_incr__off;
    (cache_ptr->resize_ctl).lower_hr_threshold	= H5C__DEF_AR_LOWER_THRESHHOLD;
    (cache_ptr->resize_ctl).increment	        = H5C__DEF_AR_INCREMENT;
    (cache_ptr->resize_ctl).apply_max_increment	= TRUE;
    (cache_ptr->resize_ctl).max_increment	= H5C__DEF_AR_MAX_INCREMENT;

    (cache_ptr->resize_ctl).flash_incr_mode     = H5C_flash_incr__off;
    (cache_ptr->resize_ctl).flash_multiple      = 1.0f;
    (cache_ptr->resize_ctl).flash_threshold     = 0.25f;

    (cache_ptr->resize_ctl).decr_mode		= H5C_decr__off;
    (cache_ptr->resize_ctl).upper_hr_threshold	= H5C__DEF_AR_UPPER_THRESHHOLD;
    (cache_ptr->resize_ctl).decrement	        = H5C__DEF_AR_DECREMENT;
    (cache_ptr->resize_ctl).apply_max_decrement	= TRUE;
    (cache_ptr->resize_ctl).max_decrement	= H5C__DEF_AR_MAX_DECREMENT;
    (cache_ptr->resize_ctl).epochs_before_eviction = H5C__DEF_AR_EPCHS_B4_EVICT;
    (cache_ptr->resize_ctl).apply_empty_reserve = TRUE;
    (cache_ptr->resize_ctl).empty_reserve	= H5C__DEF_AR_EMPTY_RESERVE;

    cache_ptr->epoch_markers_active		= 0;

    /* no need to initialize the ring buffer itself */
    cache_ptr->epoch_marker_ringbuf_first	= 1;
    cache_ptr->epoch_marker_ringbuf_last	= 0;
    cache_ptr->epoch_marker_ringbuf_size	= 0;

    /* Initialize all epoch marker entries' fields to zero/FALSE/NULL */
    HDmemset(cache_ptr->epoch_markers, 0, sizeof(cache_ptr->epoch_markers));

    /* Set non-zero/FALSE/NULL fields for epoch markers */
    for ( i = 0; i < H5C__MAX_EPOCH_MARKERS; i++ )
    {
        ((cache_ptr->epoch_markers)[i]).magic		 =
					       H5C__H5C_CACHE_ENTRY_T_MAGIC;
        ((cache_ptr->epoch_markers)[i]).addr		 = (haddr_t)i;
        ((cache_ptr->epoch_markers)[i]).type		 = &epoch_marker_class;
    }

    if ( H5C_reset_cache_hit_rate_stats(cache_ptr) != SUCCEED ) {

        /* this should be impossible... */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, \
                    "H5C_reset_cache_hit_rate_stats failed.")
    }

    H5C_stats__reset(cache_ptr);

    cache_ptr->prefix[0]			= '\0';  /* empty string */

#ifndef NDEBUG
    cache_ptr->get_entry_ptr_from_addr_counter	= 0;  
#endif /* NDEBUG */

    /* Set return value */
    ret_value = cache_ptr;

done:

    if ( ret_value == 0 ) {

        if ( cache_ptr != NULL ) {

            if ( cache_ptr->slist_ptr != NULL )
                H5SL_close(cache_ptr->slist_ptr);

            if ( cache_ptr->cork_list_ptr != NULL )
                H5SL_close(cache_ptr->cork_list_ptr);

            cache_ptr->magic = 0;
            cache_ptr = H5FL_FREE(H5C_t, cache_ptr);

        } /* end if */

    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_create() */


/*-------------------------------------------------------------------------
 * Function:    H5C_def_auto_resize_rpt_fcn
 *
 * Purpose:     Print results of a automatic cache resize.
 *
 *		This function should only be used where HDprintf() behaves
 *		well -- i.e. not on Windows.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *		10/27/04
 *
 *-------------------------------------------------------------------------
 */
void
H5C_def_auto_resize_rpt_fcn(H5C_t * cache_ptr,
#ifndef NDEBUG
                            int32_t version,
#else /* NDEBUG */
                            int32_t H5_ATTR_UNUSED version,
#endif /* NDEBUG */
                            double hit_rate,
                            enum H5C_resize_status status,
                            size_t old_max_cache_size,
                            size_t new_max_cache_size,
                            size_t old_min_clean_size,
                            size_t new_min_clean_size)
{
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( version == H5C__CURR_AUTO_RESIZE_RPT_FCN_VER );

    switch ( status )
    {
        case in_spec:
            HDfprintf(stdout,
                      "%sAuto cache resize -- no change. (hit rate = %lf)\n",
                      cache_ptr->prefix, hit_rate);
            break;

        case increase:
            HDassert( hit_rate < (cache_ptr->resize_ctl).lower_hr_threshold );
            HDassert( old_max_cache_size < new_max_cache_size );

            HDfprintf(stdout,
                      "%sAuto cache resize -- hit rate (%lf) out of bounds low (%6.5lf).\n",
                      cache_ptr->prefix, hit_rate,
                      (cache_ptr->resize_ctl).lower_hr_threshold);

            HDfprintf(stdout,
                    "%s	cache size increased from (%Zu/%Zu) to (%Zu/%Zu).\n",
                    cache_ptr->prefix,
                    old_max_cache_size,
                    old_min_clean_size,
                    new_max_cache_size,
                    new_min_clean_size);
            break;

        case flash_increase:
            HDassert( old_max_cache_size < new_max_cache_size );

            HDfprintf(stdout,
                    "%sflash cache resize(%d) -- size threshold = %Zu.\n",
                    cache_ptr->prefix,
                    (int)((cache_ptr->resize_ctl).flash_incr_mode),
                    cache_ptr->flash_size_increase_threshold);

            HDfprintf(stdout,
                  "%s cache size increased from (%Zu/%Zu) to (%Zu/%Zu).\n",
                   cache_ptr->prefix,
                   old_max_cache_size,
                   old_min_clean_size,
                   new_max_cache_size,
                   new_min_clean_size);
                break;

        case decrease:
            HDassert( old_max_cache_size > new_max_cache_size );

            switch ( (cache_ptr->resize_ctl).decr_mode )
            {
                case H5C_decr__off:
                    HDfprintf(stdout,
                              "%sAuto cache resize -- decrease off.  HR = %lf\n",
                              cache_ptr->prefix, hit_rate);
                    break;

                case H5C_decr__threshold:
                    HDassert( hit_rate >
                              (cache_ptr->resize_ctl).upper_hr_threshold );

                    HDfprintf(stdout,
                              "%sAuto cache resize -- decrease by threshold.  HR = %lf > %6.5lf\n",
                              cache_ptr->prefix, hit_rate,
                              (cache_ptr->resize_ctl).upper_hr_threshold);

                    HDfprintf(stdout, "%sout of bounds high (%6.5lf).\n",
                              cache_ptr->prefix,
                              (cache_ptr->resize_ctl).upper_hr_threshold);
                    break;

                case H5C_decr__age_out:
                    HDfprintf(stdout,
                              "%sAuto cache resize -- decrease by ageout.  HR = %lf\n",
                              cache_ptr->prefix, hit_rate);
                    break;

                case H5C_decr__age_out_with_threshold:
                    HDassert( hit_rate >
                              (cache_ptr->resize_ctl).upper_hr_threshold );

                    HDfprintf(stdout,
                              "%sAuto cache resize -- decrease by ageout with threshold. HR = %lf > %6.5lf\n",
                              cache_ptr->prefix, hit_rate,
                              (cache_ptr->resize_ctl).upper_hr_threshold);
                    break;

                default:
                    HDfprintf(stdout,
                              "%sAuto cache resize -- decrease by unknown mode.  HR = %lf\n",
                              cache_ptr->prefix, hit_rate);
            }

            HDfprintf(stdout,
                      "%s	cache size decreased from (%Zu/%Zu) to (%Zu/%Zu).\n",
                      cache_ptr->prefix,
                      old_max_cache_size,
                      old_min_clean_size,
                      new_max_cache_size,
                      new_min_clean_size);
            break;

        case at_max_size:
            HDfprintf(stdout,
                      "%sAuto cache resize -- hit rate (%lf) out of bounds low (%6.5lf).\n",
                      cache_ptr->prefix, hit_rate,
                      (cache_ptr->resize_ctl).lower_hr_threshold);
            HDfprintf(stdout,
                      "%s	cache already at maximum size so no change.\n",
                      cache_ptr->prefix);
            break;

        case at_min_size:
            HDfprintf(stdout,
                      "%sAuto cache resize -- hit rate (%lf) -- can't decrease.\n",
                      cache_ptr->prefix, hit_rate);
            HDfprintf(stdout, "%s	cache already at minimum size.\n",
                      cache_ptr->prefix);
            break;

        case increase_disabled:
            HDfprintf(stdout,
                      "%sAuto cache resize -- increase disabled -- HR = %lf.",
                      cache_ptr->prefix, hit_rate);
            break;

        case decrease_disabled:
            HDfprintf(stdout,
                      "%sAuto cache resize -- decrease disabled -- HR = %lf.\n",
                      cache_ptr->prefix, hit_rate);
            break;

        case not_full:
            HDassert( hit_rate < (cache_ptr->resize_ctl).lower_hr_threshold );

            HDfprintf(stdout,
                      "%sAuto cache resize -- hit rate (%lf) out of bounds low (%6.5lf).\n",
                      cache_ptr->prefix, hit_rate,
                      (cache_ptr->resize_ctl).lower_hr_threshold);
            HDfprintf(stdout,
                      "%s	cache not full so no increase in size.\n",
                      cache_ptr->prefix);
            break;

        default:
            HDfprintf(stdout, "%sAuto cache resize -- unknown status code.\n",
                      cache_ptr->prefix);
            break;
    }

    return;

} /* H5C_def_auto_resize_rpt_fcn() */


/*-------------------------------------------------------------------------
 * Function:    H5C_free_cork_list_cb
 *
 * Purpose:     Callback function to free the list of object addresses 
 *		on the skip list.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; January 2014
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_free_cork_list_cb(void *_item, void H5_ATTR_UNUSED *key, void H5_ATTR_UNUSED *op_data)
{
    haddr_t *addr = (haddr_t *)_item;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(addr);

    /* Release the item */
    addr = H5FL_FREE(haddr_t, addr);

    FUNC_LEAVE_NOAPI(0)
}  /* H5C_free_cork_list_cb() */



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
 *-------------------------------------------------------------------------
 */
herr_t
H5C_dest(H5F_t * f, hid_t dxpl_id)
{
    H5C_t * cache_ptr = f->shared->cache;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Flush and invalidate all cache entries */
    if(H5C_flush_invalidate_cache(f, dxpl_id, H5C__NO_FLAGS_SET) < 0 )
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")

    if(cache_ptr->slist_ptr != NULL) {
        H5SL_close(cache_ptr->slist_ptr);
        cache_ptr->slist_ptr = NULL;
    } /* end if */

    if(cache_ptr->cork_list_ptr != NULL) {
        H5SL_destroy(cache_ptr->cork_list_ptr, H5C_free_cork_list_cb, NULL);
        cache_ptr->cork_list_ptr = NULL;
    } /* end if */

    /* Only display count of number of calls to H5C_get_entry_ptr_from_add()
     * if NDEBUG is undefined, and H5C_DO_SANITY_CHECKS is defined.  Need 
     * this as the print statement will upset windows, and we frequently
     * run debug builds there.
     *
     * Note that the count is still kept whenever NDEBUG is undefined, and
     * is reasonably accessible via debugger.
     */
#ifndef NDEBUG 
#if H5C_DO_SANITY_CHECKS
    if ( cache_ptr->get_entry_ptr_from_addr_counter > 0 )
        HDfprintf(stdout, 
		  "*** %ld calls to H5C_get_entry_ptr_from_add(). ***\n",
		  cache_ptr->get_entry_ptr_from_addr_counter);
#endif /* H5C_DO_SANITY_CHECKS */
#endif /* NDEBUG */

#ifndef NDEBUG
    cache_ptr->magic = 0;
#endif /* NDEBUG */

    cache_ptr = H5FL_FREE(H5C_t, cache_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5C_evict
 *
 * Purpose:     Evict all except pinned entries in the cache
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Dec 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_evict(H5F_t * f, hid_t dxpl_id)
{
    H5C_t *cache_ptr = f->shared->cache;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Flush and invalidate all cache entries except the pinned entries */
    if(H5C_flush_invalidate_cache(f, dxpl_id, H5C__EVICT_ALLOW_LAST_PINS_FLAG) < 0 )
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to evict entries in the cache")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_evict() */


/*-------------------------------------------------------------------------
 * Function:    H5C_expunge_entry
 *
 * Purpose:     Use this function to tell the cache to expunge an entry
 * 		from the cache without writing it to disk even if it is
 * 		dirty.  The entry may not be either pinned or protected.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/29/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_expunge_entry(H5F_t *f, hid_t dxpl_id, const H5C_class_t *type,
    haddr_t addr, unsigned flags)
{
    H5C_t *		cache_ptr;
    H5C_cache_entry_t *	entry_ptr = NULL;
    unsigned            flush_flags = (H5C__FLUSH_INVALIDATE_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG);
#if H5C_DO_SANITY_CHECKS
    hbool_t entry_was_dirty;
    hsize_t entry_size;
#endif /* H5C_DO_SANITY_CHECKS */
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(f->shared);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

#if H5C_DO_EXTREME_SANITY_CHECKS
    if(H5C_validate_lru_list(cache_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "LRU extreme sanity check failed on entry.\n");
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    /* Look for entry in cache */
    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)
    if((entry_ptr == NULL) || (entry_ptr->type != type))
        /* the target doesn't exist in the cache, so we are done. */
        HGOTO_DONE(SUCCEED)

    HDassert(entry_ptr->addr == addr);
    HDassert(entry_ptr->type == type);

    /* Check for entry being pinned or protected */
    if(entry_ptr->is_protected)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, FAIL, "Target entry is protected.")
    if(entry_ptr->is_pinned)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, FAIL, "Target entry is pinned.")
#ifdef H5_HAVE_PARALLEL
    if(entry_ptr->coll_access) {
        entry_ptr->coll_access = FALSE;
        H5C__REMOVE_FROM_COLL_LIST(cache_ptr, entry_ptr, FAIL)
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    /* If we get this far, call H5C__flush_single_entry() with the
     * H5C__FLUSH_INVALIDATE_FLAG and the H5C__FLUSH_CLEAR_ONLY_FLAG.
     * This will clear the entry, and then delete it from the cache.
     */

    /* Pass along 'free file space' flag to  cache client.  */
    flush_flags |= (flags & H5C__FREE_FILE_SPACE_FLAG);

#if H5C_DO_SANITY_CHECKS
    entry_was_dirty = entry_ptr->is_dirty;
    entry_size      = entry_ptr->size;
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */
    
    /* Delete the entry from the skip list on destroy */
    flush_flags |= H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG;

    if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, flush_flags, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, FAIL, "can't flush entry")

#if H5C_DO_SANITY_CHECKS
    if ( entry_was_dirty )
    {
        /* we have just removed an entry from the skip list.  Thus 
         * we must touch up cache_ptr->slist_len_increase and
         * cache_ptr->slist_size_increase to keep from skewing
         * the sanity checks on flushes.
         */
        cache_ptr->slist_len_increase -= 1;
        cache_ptr->slist_size_increase -= (int64_t)(entry_size);
    }
#endif /* H5C_DO_SANITY_CHECKS */

done:
#if H5C_DO_EXTREME_SANITY_CHECKS
    if(H5C_validate_lru_list(cache_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "LRU extreme sanity check failed on exit.\n");
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_expunge_entry() */


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
 * Return:      Non-negative on success/Negative on failure or if there was
 *		a request to flush all items and something was protected.
 *
 * Programmer:  John Mainzer
 *		6/2/04
 *
 * Changes:	Modified function to test for slist chamges in 
 *		pre_serialize and serialize callbacks, and re-start
 *		scans through the slist when such changes occur.
 *
 *		This has been a potential problem for some time,
 *		and there has been code in this function to deal 
 *		with elements of this issue.  However the shift 
 *		to the V3 cache in combination with the activities
 *		of some of the cache clients (in particular the 
 *		free space manager and the fractal heap) have
 *		made this re-work necessary.
 *
 *						JRM -- 12/13/14
 *
 *		Modified function to support rings.  Basic idea is that 
 *		every entry in the cache is assigned to a ring.  Entries
 *		in the outermost ring are flushed first, followed by 
 *		those in the next outermost ring, and so on until the 
 *		innermost ring is flushed.  See header comment on 
 *		H5C_ring_t in H5Cprivate.h for a more detailed 
 *		discussion.
 *
 *						JRM -- 8/30/15
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_flush_cache(H5F_t *f, hid_t dxpl_id, unsigned flags)
{
#if H5C_DO_SANITY_CHECKS
    int			i;
    int32_t		index_len = 0;
    size_t		index_size = (size_t)0;
    size_t		clean_index_size = (size_t)0;
    size_t		dirty_index_size = (size_t)0;
    size_t		slist_size = (size_t)0;
    int32_t		slist_len = 0;
#endif /* H5C_DO_SANITY_CHECKS */
    H5C_ring_t		ring;
    H5C_t             * cache_ptr;
    hbool_t             destroy;
    hbool_t		ignore_protected;
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(f->shared);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_ptr->slist_ptr);

#if H5C_DO_SANITY_CHECKS
    HDassert(cache_ptr->index_ring_len[H5C_RING_UNDEFINED] == 0);
    HDassert(cache_ptr->index_ring_size[H5C_RING_UNDEFINED] == (size_t)0);
    HDassert(cache_ptr->clean_index_ring_size[H5C_RING_UNDEFINED] == (size_t)0);
    HDassert(cache_ptr->dirty_index_ring_size[H5C_RING_UNDEFINED] == (size_t)0);
    HDassert(cache_ptr->slist_ring_len[H5C_RING_UNDEFINED] == 0);
    HDassert(cache_ptr->slist_ring_size[H5C_RING_UNDEFINED] == (size_t)0);

    for(i = H5C_RING_USER; i < H5C_RING_NTYPES; i++) {
        index_len += cache_ptr->index_ring_len[i];
        index_size += cache_ptr->index_ring_size[i];
        clean_index_size += cache_ptr->clean_index_ring_size[i];
        dirty_index_size += cache_ptr->dirty_index_ring_size[i];

	slist_len += cache_ptr->slist_ring_len[i];
        slist_size += cache_ptr->slist_ring_size[i];
    } /* end for */

    HDassert(cache_ptr->index_len == index_len);
    HDassert(cache_ptr->index_size == index_size);
    HDassert(cache_ptr->clean_index_size == clean_index_size);
    HDassert(cache_ptr->dirty_index_size == dirty_index_size);
    HDassert(cache_ptr->slist_len == slist_len);
    HDassert(cache_ptr->slist_size == slist_size);
#endif /* H5C_DO_SANITY_CHECKS */

#if H5C_DO_EXTREME_SANITY_CHECKS
    if((H5C_validate_protected_entry_list(cache_ptr) < 0) ||
            (H5C_validate_pinned_entry_list(cache_ptr) < 0) ||
            (H5C_validate_lru_list(cache_ptr) < 0))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "an extreme sanity check failed on entry.\n");
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    ignore_protected = ( (flags & H5C__FLUSH_IGNORE_PROTECTED_FLAG) != 0 );
    destroy = ( (flags & H5C__FLUSH_INVALIDATE_FLAG) != 0 );
    HDassert( ! ( destroy && ignore_protected ) );
    HDassert( ! ( cache_ptr->flush_in_progress ) );

    cache_ptr->flush_in_progress = TRUE;

    if(destroy) {
        if(H5C_flush_invalidate_cache(f, dxpl_id, flags) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "flush invalidate failed.")
    } /* end if */
    else {
	/* flush each ring, starting from the outermost ring and 
         * working inward.
         */
        ring = H5C_RING_USER;
	while(ring < H5C_RING_NTYPES) {
	    if(H5C_flush_ring(f, dxpl_id, ring, flags) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "flush ring failed.")
            ring++;
        } /* end while */
    } /* end else */

done:
    cache_ptr->flush_in_progress = FALSE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5C_flush_to_min_clean
 *
 * Purpose:	Flush dirty entries until the caches min clean size is
 *		attained.
 *
 *		This function is used in the implementation of the
 *		metadata cache in PHDF5.  To avoid "messages from the
 *		future", the cache on process 0 can't be allowed to
 *		flush entries until the other processes have reached
 *		the same point in the calculation.  If this constraint
 *		is not met, it is possible that the other processes will
 *		read metadata generated at a future point in the
 *		computation.
 *
 *
 * Return:      Non-negative on success/Negative on failure or if
 *		write is not permitted.
 *
 * Programmer:  John Mainzer
 *		9/16/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_flush_to_min_clean(H5F_t * f,
		       hid_t    dxpl_id)
{
    H5C_t *             cache_ptr;
    herr_t      	result;
    hbool_t		write_permitted;
#if 0 /* modified code -- commented out for now */
    int			i;
    int			flushed_entries_count = 0;
    size_t		flushed_entries_size = 0;
    size_t		space_needed = 0;
    haddr_t	      * flushed_entries_list = NULL;
    H5C_cache_entry_t *	entry_ptr = NULL;
#endif /* JRM */
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( f );
    HDassert( f->shared );

    cache_ptr = f->shared->cache;

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( cache_ptr->check_write_permitted != NULL ) {

        result = (cache_ptr->check_write_permitted)(f, &write_permitted);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "Can't get write_permitted")
        }
    } else {

        write_permitted = cache_ptr->write_permitted;
    }

    if ( ! write_permitted ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "cache write is not permitted!?!\n");
    }
#if 1 /* original code */
    result = H5C_make_space_in_cache(f,
                                     dxpl_id,
                                     (size_t)0,
                                     write_permitted);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_make_space_in_cache failed.")
    }
#else /* modified code -- commented out for now */
    if ( cache_ptr->max_cache_size > cache_ptr->index_size ) {

        if ( ((cache_ptr->max_cache_size - cache_ptr->index_size) +
               cache_ptr->cLRU_list_size) >= cache_ptr->min_clean_size ) {

            space_needed = 0;

        } else {

            space_needed = cache_ptr->min_clean_size -
                ((cache_ptr->max_cache_size - cache_ptr->index_size) +
                 cache_ptr->cLRU_list_size);
        }
    } else {

        if ( cache_ptr->min_clean_size <= cache_ptr->cLRU_list_size ) {

           space_needed = 0;

        } else {

            space_needed = cache_ptr->min_clean_size -
                           cache_ptr->cLRU_list_size;
        }
    }

    if ( space_needed > 0 ) { /* we have work to do */

        HDassert( cache_ptr->slist_len > 0 );

        /* allocate an array to keep a list of the entries that we
         * mark for flush.  We need this list to touch up the LRU
         * list after the flush.
         */
        flushed_entries_list = (haddr_t *)H5MM_malloc(sizeof(haddr_t) *
                                              (size_t)(cache_ptr->slist_len));

        if ( flushed_entries_list == NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                        "memory allocation failed for flushed entries list")
        }

        /* Scan the dirty LRU list from tail forward and mark sufficient
         * entries to free up the necessary space.  Keep a list of the
         * entries marked in the order in which they are encountered.
         */
        entry_ptr = cache_ptr->dLRU_tail_ptr;

        while ( ( flushed_entries_size < space_needed ) &&
                ( flushed_entries_count < cache_ptr->slist_len ) &&
                ( entry_ptr != NULL ) )
        {
            HDassert( ! (entry_ptr->is_protected) );
            HDassert( ! (entry_ptr->is_read_only) );
            HDassert( entry_ptr->ro_ref_count == 0 );
            HDassert( entry_ptr->is_dirty );
            HDassert( entry_ptr->in_slist );

            entry_ptr->flush_marker = TRUE;
            flushed_entries_size += entry_ptr->size;
            flushed_entries_list[flushed_entries_count] = entry_ptr->addr;
            flushed_entries_count++;
            entry_ptr = entry_ptr->aux_prev;
        }

        HDassert( flushed_entries_count <= cache_ptr->slist_len );
        HDassert( flushed_entries_size >= space_needed );


        /* Flush the marked entries */
	result = H5C_flush_cache(f, primary_dxpl_id, secondary_dxpl_id,
                                 H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_IGNORE_PROTECTED_FLAG);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_flush_cache failed.")
        }

        /* Now touch up the LRU list so as to place the flushed entries in
         * the order they they would be in if we had flushed them in the
         * order we encountered them in.
         */

        i = 0;
        while ( i < flushed_entries_count )
        {
            H5C__SEARCH_INDEX_NO_STATS(cache_ptr, flushed_entries_list[i], \
                                       entry_ptr, FAIL)

	    /* At present, the above search must always succeed.  However,
             * that may change.  Write the code so we need only remove the
             * following assert in that event.
             */
            HDassert( entry_ptr != NULL );
            H5C__FAKE_RP_FOR_MOST_RECENT_ACCESS(cache_ptr, entry_ptr, FAIL)
            i++;
        }
    } /* if ( space_needed > 0 ) */
#endif /* end modified code -- commented out for now */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_to_min_clean() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_cache_auto_resize_config
 *
 * Purpose:	Copy the current configuration of the cache automatic
 *		re-sizing function into the instance of H5C_auto_size_ctl_t
 *		pointed to by config_ptr.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *		10/8/04
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_get_cache_auto_resize_config(const H5C_t * cache_ptr,
                                 H5C_auto_size_ctl_t *config_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    if ( config_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad config_ptr on entry.")
    }

    *config_ptr = cache_ptr->resize_ctl;

    config_ptr->set_initial_size = FALSE;
    config_ptr->initial_size = cache_ptr->max_cache_size;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_cache_auto_resize_config() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_cache_size
 *
 * Purpose:	Return the cache maximum size, the minimum clean size, the
 *		current size, and the current number of entries in
 *              *max_size_ptr, *min_clean_size_ptr, *cur_size_ptr, and
 *		*cur_num_entries_ptr respectively.  If any of these
 *		parameters are NULL, skip that value.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *		10/8/04
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_get_cache_size(H5C_t * cache_ptr,
                   size_t * max_size_ptr,
                   size_t * min_clean_size_ptr,
                   size_t * cur_size_ptr,
                   int32_t * cur_num_entries_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    if ( max_size_ptr != NULL ) {

        *max_size_ptr = cache_ptr->max_cache_size;
    }

    if ( min_clean_size_ptr != NULL ) {

        *min_clean_size_ptr = cache_ptr->min_clean_size;
    }

    if ( cur_size_ptr != NULL ) {

        *cur_size_ptr = cache_ptr->index_size;
    }

    if ( cur_num_entries_ptr != NULL ) {

        *cur_num_entries_ptr = cache_ptr->index_len;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_cache_size() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_cache_hit_rate
 *
 * Purpose:	Compute and return the current cache hit rate in
 *              *hit_rate_ptr.  If there have been no accesses since the
 *              last time the cache hit rate stats were reset, set
 *		*hit_rate_ptr to 0.0.  On error, *hit_rate_ptr is
 *		undefined.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *		10/7/04
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_get_cache_hit_rate(H5C_t * cache_ptr, double * hit_rate_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if((cache_ptr == NULL) || (cache_ptr->magic != H5C__H5C_T_MAGIC))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    if(hit_rate_ptr == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad hit_rate_ptr on entry.")

    HDassert(cache_ptr->cache_hits >= 0);
    HDassert(cache_ptr->cache_accesses >= cache_ptr->cache_hits);

    if(cache_ptr->cache_accesses > 0)
        *hit_rate_ptr = ((double)(cache_ptr->cache_hits)) /
                         ((double)(cache_ptr->cache_accesses));
    else
        *hit_rate_ptr = 0.0f;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_get_cache_hit_rate() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_get_entry_status
 *
 * Purpose:     This function is used to determine whether the cache
 *		contains an entry with the specified base address.  If
 *		the entry exists, it also reports some status information
 *		on the entry.
 *
 *		Status information is reported in the locations pointed
 *		to by the size_ptr, in_cache_ptr, is_dirty_ptr, and
 *		is_protected_ptr.  While in_cache_ptr must be defined,
 *		the remaining pointers may be NULL, in which case the
 *		associated data is not reported.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              7/1/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_get_entry_status(const H5F_t *f,
                     haddr_t   addr,
                     size_t *  size_ptr,
                     hbool_t * in_cache_ptr,
                     hbool_t * is_dirty_ptr,
                     hbool_t * is_protected_ptr,
		     hbool_t * is_pinned_ptr,
		     hbool_t * is_corked_ptr,
		     hbool_t * is_flush_dep_parent_ptr,
                     hbool_t * is_flush_dep_child_ptr)
{
    H5C_t             * cache_ptr;
    H5C_cache_entry_t *	entry_ptr = NULL;
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( f );
    HDassert( f->shared );

    cache_ptr = f->shared->cache;

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( H5F_addr_defined(addr) );
    HDassert( in_cache_ptr != NULL );

    /* this test duplicates two of the above asserts, but we need an
     * invocation of HGOTO_ERROR to keep the compiler happy.
     */
    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)

    if ( entry_ptr == NULL ) {

        /* the entry doesn't exist in the cache -- report this
         * and quit.
         */
        *in_cache_ptr = FALSE;

    } else {

        *in_cache_ptr = TRUE;

        if ( size_ptr != NULL ) {

            *size_ptr = entry_ptr->size;
        }

        if ( is_dirty_ptr != NULL ) {

            *is_dirty_ptr = entry_ptr->is_dirty;
        }

        if ( is_protected_ptr != NULL ) {

            *is_protected_ptr = entry_ptr->is_protected;
        }

        if ( is_pinned_ptr != NULL ) {

            *is_pinned_ptr = entry_ptr->is_pinned;
        }

        if ( is_corked_ptr != NULL ) {

            *is_corked_ptr = entry_ptr->is_corked;
        }

        if ( is_flush_dep_parent_ptr != NULL ) {

            *is_flush_dep_parent_ptr = (entry_ptr->flush_dep_nchildren > 0);
        }

        if ( is_flush_dep_child_ptr != NULL ) {

            *is_flush_dep_child_ptr = (entry_ptr->flush_dep_nparents > 0);
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_entry_status() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_evictions_enabled()
 *
 * Purpose:     Copy the current value of cache_ptr->evictions_enabled into
 *              *evictions_enabled_ptr.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *              7/27/07
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_get_evictions_enabled(const H5C_t *cache_ptr,
                          hbool_t * evictions_enabled_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    if ( evictions_enabled_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Bad evictions_enabled_ptr on entry.")
    }

    *evictions_enabled_ptr = cache_ptr->evictions_enabled;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_evictions_enabled() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_aux_ptr
 *
 * Purpose:     Get the aux_ptr field from the cache.
 *
 *              This field will either be NULL (when accessing a file serially)
 *              or contains a pointer to the auxiliary info for parallel I/O.
 *
 * Return:      NULL/non-NULL (can't fail)
 *
 * Programmer:  Quincey Koziol
 *              6/29/15
 *
 *-------------------------------------------------------------------------
 */
void *
H5C_get_aux_ptr(const H5C_t *cache_ptr)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Check arguments */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    FUNC_LEAVE_NOAPI(cache_ptr->aux_ptr)
} /* H5C_get_aux_ptr() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_trace_file_ptr
 *
 * Purpose:     Get the trace_file_ptr field from the cache.
 *
 *              This field will either be NULL (which indicates that trace
 *              file logging is turned off), or contain a pointer to the
 *              open file to which trace file data is to be written.
 *
 * Return:      Non-NULL trace file pointer (can't fail)
 *
 * Programmer:  John Mainzer
 *              1/20/06
 *
 *-------------------------------------------------------------------------
 */
FILE *
H5C_get_trace_file_ptr(const H5C_t *cache_ptr)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Check arguments */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    FUNC_LEAVE_NOAPI(cache_ptr->trace_file_ptr)
} /* H5C_get_trace_file_ptr() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_trace_file_ptr_from_entry
 *
 * Purpose:     Get the trace_file_ptr field from the cache, via an entry.
 *
 *              This field will either be NULL (which indicates that trace
 *              file logging is turned off), or contain a pointer to the
 *              open file to which trace file data is to be written.
 *
 * Return:      Non-NULL trace file pointer (can't fail)
 *
 * Programmer:  Quincey Koziol
 *              6/9/08
 *
 *-------------------------------------------------------------------------
 */
FILE *
H5C_get_trace_file_ptr_from_entry(const H5C_cache_entry_t *entry_ptr)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    HDassert(entry_ptr);
    HDassert(entry_ptr->cache_ptr);

    FUNC_LEAVE_NOAPI(H5C_get_trace_file_ptr(entry_ptr->cache_ptr))
} /* H5C_get_trace_file_ptr_from_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_up_logging
 *
 * Purpose:     Setup for metadata cache logging.
 *
 *              Metadata logging is enabled and disabled at two levels. This
 *              function and the associated tear_down function open and close
 *              the log file. the start_ and stop_logging functions are then
 *              used to switch logging on/off. Optionally, logging can begin
 *              as soon as the log file is opened (set via the start_immediately
 *              parameter to this function).
 *
 *              The log functionality is split between the H5C and H5AC
 *              packages. Log state and direct log manipulation resides in
 *              H5C. Log messages are generated in H5AC and sent to
 *              the H5C_write_log_message function.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_set_up_logging(H5C_t *cache_ptr, const char log_location[],
    hbool_t start_immediately)
{
#ifdef H5_HAVE_PARALLEL
    H5AC_aux_t *aux_ptr = NULL;
#endif /*H5_HAVE_PARALLEL*/
    char *file_name = NULL;
    size_t n_chars;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(log_location);

    /* Sanity checks */
    if(NULL == cache_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache_ptr == NULL")

    if(H5C__H5C_T_MAGIC != cache_ptr->magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache magic value incorrect")

    if(cache_ptr->logging_enabled)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "logging already set up")

    if(NULL == log_location)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL log location not allowed")

    /* Possibly fix up the log file name.
     * The extra 39 characters are for adding the rank to the file name
     * under parallel HDF5. 39 characters allows > 2^127 processes which
     * should be enough for anybody.
     *
     * allocation size = <path length> + dot + <rank # length> + \0
     */
    n_chars = HDstrlen(log_location) + 1 + 39 + 1;
    if(NULL == (file_name = (char *)HDcalloc(n_chars, sizeof(char))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, \
            "can't allocate memory for mdc log file name manipulation")

#ifdef H5_HAVE_PARALLEL

    /* Add the rank to the log file name when MPI is in use */
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);

    if(NULL == aux_ptr) {
        HDsnprintf(file_name, n_chars, "%s", log_location);
    }
    else {
        if(aux_ptr->magic != H5AC__H5AC_AUX_T_MAGIC) {
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad aux_ptr->magic")
        }
        HDsnprintf(file_name, n_chars, "%s.%d", log_location, aux_ptr->mpi_rank);
    }

#else /* H5_HAVE_PARALLEL */

    HDsnprintf(file_name, n_chars, "%s", log_location);

#endif /* H5_HAVE_PARALLEL */

    /* Open log file */
    if(NULL == (cache_ptr->log_file_ptr = HDfopen(file_name, "w")))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "can't create mdc log file")

    /* Set logging flags */
    cache_ptr->logging_enabled = TRUE;
    cache_ptr->currently_logging = start_immediately;

 done:
    if(file_name)
        HDfree(file_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_set_up_logging() */


/*-------------------------------------------------------------------------
 * Function:    H5C_tear_down_logging
 *
 * Purpose:     Tear-down for metadata cache logging.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_tear_down_logging(H5C_t *cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Sanity checks */
    if(NULL == cache_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache_ptr == NULL")

    if(H5C__H5C_T_MAGIC != cache_ptr->magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache magic value incorrect")

    if(FALSE == cache_ptr->logging_enabled)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "logging not enabled")

    /* Unset logging flags */
    cache_ptr->logging_enabled = FALSE;
    cache_ptr->currently_logging = FALSE;

    /* Close log file */
    if(EOF == HDfclose(cache_ptr->log_file_ptr))
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problem closing mdc log file")
    cache_ptr->log_file_ptr = NULL;

 done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_tear_down_logging() */


/*-------------------------------------------------------------------------
 * Function:    H5C_start_logging
 *
 * Purpose:     Start logging metadata cache operations.
 *
 *              TODO: Add a function that dumps the current state of the
 *                    metadata cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_start_logging(H5C_t *cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Sanity checks */
    if(NULL == cache_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache_ptr == NULL")

    if(H5C__H5C_T_MAGIC != cache_ptr->magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache magic value incorrect")

    if(FALSE == cache_ptr->logging_enabled)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "logging not enabled")

    if(cache_ptr->currently_logging)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "logging already in progress")

    /* Set logging flags */
    cache_ptr->currently_logging = TRUE;

    /* TODO - Dump cache state */

 done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_start_logging() */


/*-------------------------------------------------------------------------
 * Function:    H5C_stop_logging
 *
 * Purpose:     Stop logging metadata cache operations.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_stop_logging(H5C_t *cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Sanity checks */
    if(NULL == cache_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache_ptr == NULL")

    if(H5C__H5C_T_MAGIC != cache_ptr->magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache magic value incorrect")

    if(FALSE == cache_ptr->logging_enabled)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "logging not enabled")

    if(FALSE == cache_ptr->currently_logging)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "logging not in progress")

    /* Set logging flags */
    cache_ptr->currently_logging = FALSE;

 done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_stop_logging() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_logging_status
 *
 * Purpose:     Determines if the cache is actively logging (via the OUT
 *              parameter).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_get_logging_status(const H5C_t *cache_ptr, /*OUT*/ hbool_t *is_enabled,
                       /*OUT*/ hbool_t *is_currently_logging)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(is_enabled);
    HDassert(is_currently_logging);

    /* Sanity checks */
    if(NULL == cache_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache_ptr == NULL")

    if(H5C__H5C_T_MAGIC != cache_ptr->magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache magic value incorrect")

    *is_enabled = cache_ptr->logging_enabled;
    *is_currently_logging = cache_ptr->currently_logging;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_get_logging_status() */


/*-------------------------------------------------------------------------
 * Function:    H5C_write_log_message
 *
 * Purpose:     Write a message to the log file and flush the file. 
 *              The message string is neither modified nor freed.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_write_log_message(const H5C_t *cache_ptr, const char message[])
{
    size_t n_chars;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(message);

    /* Sanity checks */
    if(NULL == cache_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache_ptr == NULL")

    if(H5C__H5C_T_MAGIC != cache_ptr->magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cache magic value incorrect")

    if(FALSE == cache_ptr->currently_logging)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "not currently logging")

    if(NULL == message)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL log message not allowed")

    /* Write the log message and flush */
    n_chars = HDstrlen(message);
    if((int)n_chars != HDfprintf(cache_ptr->log_file_ptr, message))
        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "error writing log message")
    if(EOF == HDfflush(cache_ptr->log_file_ptr))
        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "error flushing log message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_write_log_message() */


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
 *-------------------------------------------------------------------------
 */
herr_t
H5C_insert_entry(H5F_t *             f,
                 hid_t		     dxpl_id,
                 const H5C_class_t * type,
                 haddr_t 	     addr,
                 void *		     thing,
                 unsigned int        flags)
{
    H5C_t               *cache_ptr;
    H5P_genplist_t      *dxpl;
    H5AC_ring_t         ring = H5C_RING_UNDEFINED;
    hbool_t		insert_pinned;
    hbool_t             flush_last;
#ifdef H5_HAVE_PARALLEL
    hbool_t             coll_access = FALSE; /* whether access to the cache entry is done collectively */
#endif /* H5_HAVE_PARALLEL */
    hbool_t             set_flush_marker;
    hbool_t		write_permitted = TRUE;
    size_t		empty_space;
    H5C_cache_entry_t  *entry_ptr;
    H5C_cache_entry_t  *test_entry_ptr;
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( f );
    HDassert( f->shared );

    cache_ptr = f->shared->cache;

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( type );
    HDassert( H5F_addr_defined(addr) );
    HDassert( thing );

#if H5C_DO_EXTREME_SANITY_CHECKS
    /* no need to verify that entry is not already in the index as */
    /* we already make that check below.                           */

    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on entry.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    set_flush_marker   = ( (flags & H5C__SET_FLUSH_MARKER_FLAG) != 0 );
    insert_pinned      = ( (flags & H5C__PIN_ENTRY_FLAG) != 0 );
    flush_last         = ( (flags & H5C__FLUSH_LAST_FLAG) != 0 );

    /* Get the dataset transfer property list */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADTYPE, FAIL, "not a property list")

    /* Get the ring type from the DXPL */
    if((H5P_get(dxpl, H5AC_RING_NAME, &ring)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "unable to query ring value")

    entry_ptr = (H5C_cache_entry_t *)thing;

    /* verify that the new entry isn't already in the hash table -- scream
     * and die if it is.
     */

    H5C__SEARCH_INDEX(cache_ptr, addr, test_entry_ptr, FAIL)

    if(test_entry_ptr != NULL) {
        if(test_entry_ptr == entry_ptr)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "entry already in cache.")
        else
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "duplicate entry in cache.")
    } /* end if */

    entry_ptr->magic = H5C__H5C_CACHE_ENTRY_T_MAGIC;
    entry_ptr->cache_ptr = cache_ptr;
    entry_ptr->addr  = addr;
    entry_ptr->type  = type;

    entry_ptr->image_ptr = NULL;
    entry_ptr->image_up_to_date = FALSE;

    /* Apply tag to newly inserted entry */
    if(H5C_tag_entry(cache_ptr, entry_ptr, dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "Cannot tag metadata entry")

    /* Set the entry's cork status */
    if(H5C_cork(cache_ptr, entry_ptr->tag, H5C__GET_CORKED, &entry_ptr->is_corked) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Cannot retrieve entry's cork status")

    entry_ptr->is_protected = FALSE;
    entry_ptr->is_read_only = FALSE;
    entry_ptr->ro_ref_count = 0;

    entry_ptr->is_pinned = insert_pinned;
    entry_ptr->pinned_from_client = insert_pinned;
    entry_ptr->pinned_from_cache = FALSE;
    entry_ptr->flush_me_last = flush_last;

    /* newly inserted entries are assumed to be dirty */
    entry_ptr->is_dirty = TRUE;

    /* not protected, so can't be dirtied */
    entry_ptr->dirtied  = FALSE;

    /* Retrieve the size of the thing.  Set the compressed field to FALSE
     * and the compressed_size field to zero first, as they may not be 
     * initialized by the image_len call.
     */
    entry_ptr->compressed = FALSE;
    entry_ptr->compressed_size = 0;
    if((type->image_len)(thing, &(entry_ptr->size), &(entry_ptr->compressed), 
                         &(entry_ptr->compressed_size)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGETSIZE, FAIL, "Can't get size of thing")
    HDassert(entry_ptr->size > 0 &&  entry_ptr->size < H5C_MAX_ENTRY_SIZE);
    HDassert(((type->flags & H5C__CLASS_COMPRESSED_FLAG) != 0) ||
             (entry_ptr->compressed == FALSE));

    /* entry has just been inserted -- thus compressed size cannot have 
     * been computed yet.  Thus if entry_ptr->compressed is TRUE, 
     * entry_ptr->size must equal entry_ptr->compressed_size.
     */
    HDassert((entry_ptr->compressed == FALSE) ||
             (entry_ptr->size == entry_ptr->compressed_size));
    HDassert((entry_ptr->compressed == TRUE) || 
             (entry_ptr->compressed_size == 0));

    entry_ptr->in_slist = FALSE;

#ifdef H5_HAVE_PARALLEL
    entry_ptr->clear_on_unprotect = FALSE;
    entry_ptr->flush_immediately = FALSE;
#endif /* H5_HAVE_PARALLEL */

    entry_ptr->flush_in_progress = FALSE;
    entry_ptr->destroy_in_progress = FALSE;

    entry_ptr->ring = ring;

    /* Initialize flush dependency height fields */
    entry_ptr->flush_dep_parent = NULL;
    entry_ptr->flush_dep_nparents = 0;
    entry_ptr->flush_dep_parent_nalloc = 0;
    entry_ptr->flush_dep_nchildren = 0;
    entry_ptr->flush_dep_ndirty_children = 0;

    entry_ptr->ht_next = NULL;
    entry_ptr->ht_prev = NULL;

    entry_ptr->next = NULL;
    entry_ptr->prev = NULL;

    entry_ptr->aux_next = NULL;
    entry_ptr->aux_prev = NULL;

#ifdef H5_HAVE_PARALLEL
    entry_ptr->coll_next = NULL;
    entry_ptr->coll_prev = NULL;
#endif /* H5_HAVE_PARALLEL */

    H5C__RESET_CACHE_ENTRY_STATS(entry_ptr)

    if ( ( cache_ptr->flash_size_increase_possible ) &&
         ( entry_ptr->size > cache_ptr->flash_size_increase_threshold ) ) {

        if(H5C__flash_increase_cache_size(cache_ptr, 0, entry_ptr->size) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "H5C__flash_increase_cache_size failed.")
    }

    if(cache_ptr->index_size >= cache_ptr->max_cache_size)
       empty_space = 0;
    else
       empty_space = cache_ptr->max_cache_size - cache_ptr->index_size;

    if ( ( cache_ptr->evictions_enabled ) &&
         ( ( (cache_ptr->index_size + entry_ptr->size) >
	     cache_ptr->max_cache_size)
	   ||
	   ( ( ( empty_space + cache_ptr->clean_index_size ) <
	       cache_ptr->min_clean_size ) ) ) ) {

        size_t space_needed;

	if(empty_space <= entry_ptr->size)
            cache_ptr->cache_full = TRUE;

        if(cache_ptr->check_write_permitted != NULL) {
            if((cache_ptr->check_write_permitted)(f, &write_permitted) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "Can't get write_permitted")
        } /* end if */
        else
            write_permitted = cache_ptr->write_permitted;

        HDassert(entry_ptr->size <= H5C_MAX_ENTRY_SIZE);
        space_needed = entry_ptr->size;
        if(space_needed > cache_ptr->max_cache_size)
            space_needed = cache_ptr->max_cache_size;

        /* Note that space_needed is just the amount of space that
         * needed to insert the new entry without exceeding the cache
         * size limit.  The subsequent call to H5C_make_space_in_cache()
         * may evict the entries required to free more or less space
         * depending on conditions.  It MAY be less if the cache is
         * currently undersized, or more if the cache is oversized.
         *
         * The cache can exceed its maximum size limit via the following
         * mechanisms:
         *
         * First, it is possible for the cache to grow without
         * bound as long as entries are protected and not unprotected.
         *
         * Second, when writes are not permitted it is also possible
         * for the cache to grow without bound.
         *
         * Finally, we usually don't check to see if the cache is
         * oversized at the end of an unprotect.  As a result, it is
         * possible to have a vastly oversized cache with no protected
         * entries as long as all the protects preceed the unprotects.
         *
         * Since items 1 and 2 are not changing any time soon, I see
         * no point in worrying about the third.
         */

        if(H5C_make_space_in_cache(f, dxpl_id, space_needed, write_permitted) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "H5C_make_space_in_cache failed.")
    }

    H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, FAIL)

    /* New entries are presumed to be dirty, so this if statement is
     * unnecessary.  Rework it once the rest of the code changes are
     * in and tested.   -- JRM
     */
    if ( entry_ptr->is_dirty ) {

        entry_ptr->flush_marker = set_flush_marker;
        H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, FAIL)

    } else {

        entry_ptr->flush_marker = FALSE;
    }

    H5C__UPDATE_RP_FOR_INSERTION(cache_ptr, entry_ptr, FAIL)

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) )
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "an extreme sanity check failed just before done.\n")
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    /* If the entry's type has a 'notify' callback send a 'after insertion'
     * notice now that the entry is fully integrated into the cache.
     */
    if(entry_ptr->type->notify &&
            (entry_ptr->type->notify)(H5C_NOTIFY_ACTION_AFTER_INSERT, entry_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTNOTIFY, FAIL, "can't notify client about entry inserted into cache")

    H5C__UPDATE_STATS_FOR_INSERTION(cache_ptr, entry_ptr)

#ifdef H5_HAVE_PARALLEL
    /* Get the dataset transfer property list */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI)) {
        coll_access = (H5P_USER_TRUE == f->coll_md_read ? TRUE : FALSE);

        if(!coll_access && H5P_FORCE_FALSE != f->coll_md_read) {
            H5P_coll_md_read_flag_t prop_value;

            /* Get the property value */
            if(H5P_get(dxpl, H5_COLL_MD_READ_FLAG_NAME, &prop_value) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't get collective metadata access flag")
            coll_access = (H5P_USER_TRUE == prop_value ? TRUE : FALSE);
        } /* end if */
    } /* end if */

    entry_ptr->coll_access = coll_access;
    if(coll_access) {
        H5C__INSERT_IN_COLL_LIST(cache_ptr, entry_ptr, FAIL)

        /* Make sure the size of the collective entries in the cache remain in check */
        if(H5P_USER_TRUE == f->coll_md_read) {
            if(cache_ptr->max_cache_size * 80 < cache_ptr->coll_list_size * 100) {
                if(H5C_clear_coll_entries(cache_ptr, TRUE) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "can't clear collective metadata entries")
            } /* end if */
        } /* end if */
        else {
            if(cache_ptr->max_cache_size * 40 < cache_ptr->coll_list_size * 100) {
                if(H5C_clear_coll_entries(cache_ptr, TRUE) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "can't clear collective metadata entries")
            } /* end if */
        } /* end else */
    } /* end if */
#endif

done:
#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) )
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "an extreme sanity check failed on exit.\n")
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_insert_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5C_mark_entry_dirty
 *
 * Purpose:	Mark a pinned or protected entry as dirty.  The target entry
 * 		MUST be either pinned or protected, and MAY be both.
 *
 * 		In the protected case, this call is the functional
 * 		equivalent of setting the H5C__DIRTIED_FLAG on an unprotect
 * 		call.
 *
 * 		In the pinned but not protected case, if the entry is not
 * 		already dirty, the function places function marks the entry
 * 		dirty and places it on the skip list.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              5/15/06
 *
 * 		JRM -- 11/5/08
 * 		Added call to H5C__UPDATE_INDEX_FOR_ENTRY_DIRTY() to
 * 		update the new clean_index_size and dirty_index_size
 * 		fields of H5C_t in the case that the entry was clean
 * 		prior to this call, and is pinned and not protected.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_mark_entry_dirty(void *thing)
{
    H5C_t *             cache_ptr;
    H5C_cache_entry_t * entry_ptr = (H5C_cache_entry_t *)thing;
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(entry_ptr);
    HDassert(H5F_addr_defined(entry_ptr->addr));
    cache_ptr = entry_ptr->cache_ptr;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    if ( entry_ptr->is_protected ) {
	HDassert( ! ((entry_ptr)->is_read_only) );

        /* set the dirtied flag */
        entry_ptr->dirtied = TRUE;

    } else if ( entry_ptr->is_pinned ) {
        hbool_t		was_pinned_unprotected_and_clean;

	was_pinned_unprotected_and_clean = ! ( entry_ptr->is_dirty );

        /* mark the entry as dirty if it isn't already */
        entry_ptr->is_dirty = TRUE;
	entry_ptr->image_up_to_date = FALSE;

        /* Propagate the dirty flag up the flush dependency chain if appropriate
         */
        if(was_pinned_unprotected_and_clean) {
            H5C__UPDATE_INDEX_FOR_ENTRY_DIRTY(cache_ptr, entry_ptr);

            if((entry_ptr->flush_dep_ndirty_children == 0) && (entry_ptr->flush_dep_nparents > 0))
                if(H5C__mark_flush_dep_dirty(entry_ptr) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "Can't propagate flush dep dirty flag")
        } /* end if */

        if ( ! (entry_ptr->in_slist) ) {

            H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, FAIL)
        }

        H5C__UPDATE_STATS_FOR_DIRTY_PIN(cache_ptr, entry_ptr)

    } else {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, \
                    "Entry is neither pinned nor protected??")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_mark_entry_dirty() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_move_entry
 *
 * Purpose:     Use this function to notify the cache that an entry's
 *              file address changed.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_move_entry(H5C_t *	     cache_ptr,
                 const H5C_class_t * type,
                 haddr_t 	     old_addr,
	         haddr_t 	     new_addr)
{
    H5C_cache_entry_t *	entry_ptr = NULL;
    H5C_cache_entry_t *	test_entry_ptr = NULL;
    hbool_t		was_dirty;
#if H5C_DO_SANITY_CHECKS
    hbool_t			removed_entry_from_slist = FALSE;
#endif /* H5C_DO_SANITY_CHECKS */
    herr_t			ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( type );
    HDassert( H5F_addr_defined(old_addr) );
    HDassert( H5F_addr_defined(new_addr) );
    HDassert( H5F_addr_ne(old_addr, new_addr) );

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on entry.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    H5C__SEARCH_INDEX(cache_ptr, old_addr, entry_ptr, FAIL)

    if ( ( entry_ptr == NULL ) || ( entry_ptr->type != type ) ) {

        /* the old item doesn't exist in the cache, so we are done. */
        HGOTO_DONE(SUCCEED)
    }

    HDassert( entry_ptr->addr == old_addr );
    HDassert( entry_ptr->type == type );

    if ( entry_ptr->is_protected ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTMOVE, FAIL, \
		    "Target entry is protected.")
    }

    H5C__SEARCH_INDEX(cache_ptr, new_addr, test_entry_ptr, FAIL)

    if ( test_entry_ptr != NULL ) { /* we are hosed */

        if ( test_entry_ptr->type == type ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTMOVE, FAIL, \
                        "Target already moved & reinserted???.")

        } else {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTMOVE, FAIL, \
                        "New address already in use?.")

        }
    }

    /* If we get this far we have work to do.  Remove *entry_ptr from
     * the hash table (and skip list if necessary), change its address to the
     * new address, mark it as dirty (if it isn't already) and then re-insert.
     *
     * Update the replacement policy for a hit to avoid an eviction before
     * the moved entry is touched.  Update stats for a move.
     *
     * Note that we do not check the size of the cache, or evict anything.
     * Since this is a simple re-name, cache size should be unaffected.
     *
     * Check to see if the target entry is in the process of being destroyed
     * before we delete from the index, etc.  If it is, all we do is
     * change the addr.  If the entry is only in the process of being flushed,
     * don't mark it as dirty either, lest we confuse the flush call back.
     */

    if ( ! ( entry_ptr->destroy_in_progress ) ) {

        H5C__DELETE_FROM_INDEX(cache_ptr, entry_ptr)

        if ( entry_ptr->in_slist ) {

            HDassert( cache_ptr->slist_ptr );

            H5C__REMOVE_ENTRY_FROM_SLIST(cache_ptr, entry_ptr)

#if H5C_DO_SANITY_CHECKS

            removed_entry_from_slist = TRUE;

#endif /* H5C_DO_SANITY_CHECKS */
        }
    }

    entry_ptr->addr = new_addr;

    if ( ! ( entry_ptr->destroy_in_progress ) ) {

        was_dirty = entry_ptr->is_dirty;
	entry_ptr->is_dirty = TRUE;

	/* This shouldn't be needed, but it keeps the test code happy */
        entry_ptr->image_up_to_date = FALSE;

        if ( ! ( entry_ptr->flush_in_progress ) ) {

            /* Propagate the dirty flag up the flush dependency chain if
             * appropriate */
            if ( ! ( was_dirty ) ) {

                if ( ( entry_ptr->flush_dep_ndirty_children == 0) &&
                     ( entry_ptr->flush_dep_nparents > 0 ) ) {

                    if ( H5C__mark_flush_dep_dirty(entry_ptr) < 0 )
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "Can't propagate flush dep dirty flag")
                }
            }
        }

        H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, FAIL)

        H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, FAIL)

#if H5C_DO_SANITY_CHECKS

        if ( removed_entry_from_slist ) {

	    /* we just removed the entry from the slist.  Thus we
	     * must touch up cache_ptr->slist_len_increase and
	     * cache_ptr->slist_size_increase to keep from skewing
	     * the sanity checks.
	     */
	    cache_ptr->slist_len_increase -= 1;
	    cache_ptr->slist_size_increase -= (int64_t)(entry_ptr->size);
        }

#endif /* H5C_DO_SANITY_CHECKS */

	if ( ! ( entry_ptr->flush_in_progress ) ) {

            /* skip the update if a flush is in progress */
            H5C__UPDATE_RP_FOR_MOVE(cache_ptr, entry_ptr, was_dirty, FAIL)
        }
    }

    H5C__UPDATE_STATS_FOR_MOVE(cache_ptr, entry_ptr)

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

} /* H5C_move_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5C_resize_entry
 *
 * Purpose:	Resize a pinned or protected entry.
 *
 * 		Resizing an entry dirties it, so if the entry is not
 * 		already dirty, the function places the entry on the
 * 		skip list.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              7/5/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_resize_entry(void *thing, size_t new_size)
{
    H5C_t             * cache_ptr;
    H5C_cache_entry_t * entry_ptr = (H5C_cache_entry_t *)thing;
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(entry_ptr);
    HDassert(H5F_addr_defined(entry_ptr->addr));
    cache_ptr = entry_ptr->cache_ptr;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Check for usage errors */
    if(new_size <= 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "New size is non-positive.")
    if(!(entry_ptr->is_pinned || entry_ptr->is_protected))
        HGOTO_ERROR(H5E_CACHE, H5E_BADTYPE, FAIL, "Entry isn't pinned or protected??")

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on entry.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    /* update for change in entry size if necessary */
    if ( entry_ptr->size != new_size ) {
        hbool_t		was_clean;

        /* make note of whether the entry was clean to begin with */
        was_clean = ! ( entry_ptr->is_dirty );

        /* mark the entry as dirty if it isn't already */
        entry_ptr->is_dirty = TRUE;
        entry_ptr->image_up_to_date = FALSE;

        /* Release the current image */
        if( entry_ptr->image_ptr )
            entry_ptr->image_ptr = H5MM_xfree(entry_ptr->image_ptr);

        /* Propagate the dirty flag up the flush dependency chain if
         * appropriate */
        if ( was_clean ) {

            if ( ( entry_ptr->flush_dep_ndirty_children == 0) &&
                 ( entry_ptr->flush_dep_nparents > 0 ) ) {

                if ( H5C__mark_flush_dep_dirty(entry_ptr) < 0 )
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "Can't propagate flush dep dirty flag")
            }
        }

        /* do a flash cache size increase if appropriate */
        if ( cache_ptr->flash_size_increase_possible ) {

            if ( new_size > entry_ptr->size ) {
                size_t             	size_increase;

                size_increase = new_size - entry_ptr->size;

                if(size_increase >= cache_ptr->flash_size_increase_threshold) {
                    if(H5C__flash_increase_cache_size(cache_ptr, entry_ptr->size, new_size) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTRESIZE, FAIL, "flash cache increase failed")
                }
            }
        }

        /* update the pinned and/or protected entry list */
        if(entry_ptr->is_pinned) {
            H5C__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr->pel_len), \
                                            (cache_ptr->pel_size), \
                                            (entry_ptr->size), (new_size))
        } /* end if */
        if(entry_ptr->is_protected) {
            H5C__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr->pl_len), \
                                            (cache_ptr->pl_size), \
                                            (entry_ptr->size), (new_size))
        } /* end if */

#ifdef H5_HAVE_PARALLEL
        if(entry_ptr->coll_access) {
            H5C__DLL_UPDATE_FOR_SIZE_CHANGE((cache_ptr->coll_list_len), \
                                            (cache_ptr->coll_list_size), \
                                            (entry_ptr->size), (new_size))
        } /* end if */
#endif /* H5_HAVE_PARALLEL */

        /* update the hash table */
	H5C__UPDATE_INDEX_FOR_SIZE_CHANGE((cache_ptr), (entry_ptr->size),\
                                          (new_size), (entry_ptr), (was_clean));

        /* if the entry is in the skip list, update that too */
        if ( entry_ptr->in_slist ) {
	    H5C__UPDATE_SLIST_FOR_SIZE_CHANGE((cache_ptr), (entry_ptr->size),\
                                              (new_size));
        } /* end if */

        /* update statistics just before changing the entry size */
	H5C__UPDATE_STATS_FOR_ENTRY_SIZE_CHANGE((cache_ptr), (entry_ptr), \
                                                (new_size));

	/* finally, update the entry size proper */
	entry_ptr->size = new_size;

        if(!entry_ptr->in_slist) {
            H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, FAIL)
        } /* end if */

        if(entry_ptr->is_pinned) {
            H5C__UPDATE_STATS_FOR_DIRTY_PIN(cache_ptr, entry_ptr)
        } /* end if */
    } /* end if */

done:

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on exit.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_resize_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5C_pin_entry_from_client()
 *
 * Purpose:	Internal routine to pin a cache entry from a client action.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              3/26/09
 *
 * Changes:	Added sanity checks to clarify the circumstances under
 *		which an entry can be pinned.   JRM -- 4/27/14 
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
static herr_t
H5C_pin_entry_from_client(H5C_t *	          cache_ptr,
                        H5C_cache_entry_t * entry_ptr)
#else
static herr_t
H5C_pin_entry_from_client(H5C_t H5_ATTR_UNUSED *	cache_ptr,
                        H5C_cache_entry_t * entry_ptr)
#endif
{
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity checks */
    HDassert( cache_ptr );
    HDassert( entry_ptr );
    HDassert( entry_ptr->is_protected );

    /* Check if the entry is already pinned */
    if(entry_ptr->is_pinned) {
        /* Check if the entry was pinned through an explicit pin from a client */
        if(entry_ptr->pinned_from_client)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTPIN, FAIL, "Entry is already pinned")
    } /* end if */
    else {
        entry_ptr->is_pinned = TRUE;

        H5C__UPDATE_STATS_FOR_PIN(cache_ptr, entry_ptr)
    } /* end else */

    /* Mark that the entry was pinned through an explicit pin from a client */
    entry_ptr->pinned_from_client = TRUE;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_pin_entry_from_client() */


/*-------------------------------------------------------------------------
 * Function:    H5C_pin_protected_entry()
 *
 * Purpose:	Pin a protected cache entry.  The entry must be protected
 * 		at the time of call, and must be unpinned.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              4/26/06
 *
 * Changes:	Added extreme sanity checks on entry and exit.
 *                                          JRM -- 4/26/14
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_pin_protected_entry(void *thing)
{
    H5C_t             * cache_ptr;
    H5C_cache_entry_t * entry_ptr = (H5C_cache_entry_t *)thing; /* Pointer to entry to pin */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(entry_ptr);
    HDassert(H5F_addr_defined(entry_ptr->addr));
    cache_ptr = entry_ptr->cache_ptr;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on entry.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */


    /* Only protected entries can be pinned */
    if(!entry_ptr->is_protected)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTPIN, FAIL, "Entry isn't protected")

    /* Pin the entry from a client */
    if(H5C_pin_entry_from_client(cache_ptr, entry_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTPIN, FAIL, "Can't pin entry by client")

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
} /* H5C_pin_protected_entry() */


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
 * Return:      Success:        Ptr to the desired entry
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer -  6/2/04
 *
 * 		JRM -- 11/13/08
 * 		Modified function to call H5C_make_space_in_cache() when
 * 		the min_clean_size is violated, not just when there isn't
 * 		enough space for and entry that has just been loaded.
 *
 *              The purpose of this modification is to avoid "metadata
 *              blizzards" in the write only case.  In such instances,
 *              the cache was allowed to fill with dirty metadata.  When
 *              we finally needed to evict an entry to make space, we had
 *              to flush out a whole cache full of metadata -- which has
 *              interesting performance effects.  We hope to avoid (or
 *              perhaps more accurately hide) this effect by maintaining
 *              the min_clean_size, which should force us to start flushing
 *              entries long before we actually have to evict something
 *              to make space.
 *
 *		JRM -- 9/1/14
 *		Replace the old rw parameter with the flags parameter.
 *		This allows H5C_protect to accept flags other than 
 *		H5C__READ_ONLY_FLAG.  
 *
 *		Added support for the H5C__FLUSH_LAST_FLAG.
 *		At present, this flag is only applied if the entry is 
 *              not in cache, and is loaded into the cache as a result of 
 *              this call.
 *
 *-------------------------------------------------------------------------
 */
void *
H5C_protect(H5F_t *		f,
            hid_t	        dxpl_id,
            const H5C_class_t * type,
            haddr_t 	        addr,
            void *              udata,
	    unsigned		flags)
{
    H5C_t *		cache_ptr;
    H5AC_ring_t         ring = H5C_RING_UNDEFINED;
    hbool_t		hit;
    hbool_t		have_write_permitted = FALSE;
    hbool_t		read_only = FALSE;
    hbool_t             flush_last;
#ifdef H5_HAVE_PARALLEL
    hbool_t             coll_access = FALSE; /* whether access to the cache entry is done collectively */
#endif /* H5_HAVE_PARALLEL */
    hbool_t		write_permitted;
    size_t		empty_space;
    void *		thing;
    H5C_cache_entry_t *	entry_ptr;
    H5P_genplist_t    * dxpl;    /* dataset transfer property list */
    void *		ret_value = NULL;       /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check args */
    HDassert( f );
    HDassert( f->shared );

    cache_ptr = f->shared->cache;

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( type );
    HDassert( H5F_addr_defined(addr) );

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, \
                    "an extreme sanity check failed on entry.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    read_only          = ( (flags & H5C__READ_ONLY_FLAG) != 0 );
    flush_last         = ( (flags & H5C__FLUSH_LAST_FLAG) != 0 );

    /* Get the dataset transfer property list */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADTYPE, NULL, "not a property list")

    /* Get the ring type from the DXPL */
    if((H5P_get(dxpl, H5AC_RING_NAME, &ring)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, NULL, "unable to query ring value")

#ifdef H5_HAVE_PARALLEL
    if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI)) {
        coll_access = (H5P_USER_TRUE == f->coll_md_read ? TRUE : FALSE);

        if(!coll_access && H5P_FORCE_FALSE != f->coll_md_read) {
            H5P_coll_md_read_flag_t prop_value;

            /* get the property value */
            if(H5P_get(dxpl, H5_COLL_MD_READ_FLAG_NAME, &prop_value) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "Can't get collective metadata access flag")
            coll_access = (H5P_USER_TRUE == prop_value ? TRUE : FALSE);
        } /* end if */
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    /* first check to see if the target is in cache */
    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, NULL)

    if ( entry_ptr != NULL ) {
        if(entry_ptr->ring != ring)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, "ring type mismatch occured for cache entry\n");

        /* Check for trying to load the wrong type of entry from an address */
        if(entry_ptr->type != type)
            HGOTO_ERROR(H5E_CACHE, H5E_BADTYPE, NULL, "incorrect cache entry type")

        /* if this is a collective metadata read, the entry is not
           marked as collective, and is clean, it is possible that
           other processes will not have it in its cache and will
           expect a bcast of the entry from process 0. So process 0
           will bcast the entry to all other ranks. Ranks that do have
           the entry in their cache still have to participate in the
           bcast. */
#ifdef H5_HAVE_PARALLEL
        if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI) && coll_access) {
            if(!(entry_ptr->is_dirty) && !(entry_ptr->coll_access)) {
                MPI_Comm  comm;           /* File MPI Communicator */
                int       mpi_code;       /* MPI error code */
                int       buf_size;

                if(MPI_COMM_NULL == (comm = H5F_mpi_get_comm(f)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "get_comm request failed")

                if(entry_ptr->image_ptr == NULL) {
                    int mpi_rank;
                    size_t image_size;

                    if((mpi_rank = H5F_mpi_get_rank(f)) < 0)
                        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "Can't get MPI rank")

                    if(entry_ptr->compressed)
                        image_size = entry_ptr->compressed_size;
                    else
                        image_size = entry_ptr->size;
                    HDassert(image_size > 0);

                    if(NULL == (entry_ptr->image_ptr = H5MM_malloc(image_size + H5C_IMAGE_EXTRA_SPACE)))
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, NULL, "memory allocation failed for on disk image buffer")
#if H5C_DO_MEMORY_SANITY_CHECKS
                    HDmemcpy(((uint8_t *)entry_ptr->image_ptr) + image_size, 
                             H5C_IMAGE_SANITY_VALUE, H5C_IMAGE_EXTRA_SPACE);
#endif /* H5C_DO_MEMORY_SANITY_CHECKS */
                    if(0 == mpi_rank)
                        if(H5C__generate_image(f, cache_ptr, entry_ptr, dxpl_id, NULL) < 0)
                            HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, NULL, "can't generate entry's image")
                } /* end if */
                HDassert(entry_ptr->image_ptr);

                H5_CHECKED_ASSIGN(buf_size, int, entry_ptr->size, size_t);
                if(MPI_SUCCESS != (mpi_code = MPI_Bcast(entry_ptr->image_ptr, buf_size, MPI_BYTE, 0, comm)))
                    HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)

                /* Mark the entry as collective and insert into the collective list */
                entry_ptr->coll_access = TRUE;
                H5C__INSERT_IN_COLL_LIST(cache_ptr, entry_ptr, NULL)
            } /* end if */
            else if(entry_ptr->coll_access) {
                H5C__MOVE_TO_TOP_IN_COLL_LIST(cache_ptr, entry_ptr, NULL)
            } /* end else-if */
        } /* end if */
#endif /* H5_HAVE_PARALLEL */

#if H5C_DO_TAGGING_SANITY_CHECKS
{
        H5C_tag_t tag;              /* Tag structure */

        /* The entry is already in the cache, but make sure that the tag value 
           being passed in via dxpl is still legal. This will ensure that had
           the entry NOT been in the cache, tagging was still set up correctly
           and it would have received a legal tag value after getting loaded
           from disk. */

        /* Get the tag from the DXPL */
        if((H5P_get(dxpl, "H5C_tag", &tag)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "unable to query property value");
    
        /* Verify tag value */
        if(cache_ptr->ignore_tags != TRUE) {
            /* Verify legal tag value */
            if((H5C_verify_tag(entry_ptr->type->id, tag.value)) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, NULL, "tag verification failed");
        } /* end if */
}
#endif

        hit = TRUE;
        thing = (void *)entry_ptr;

    } else {

        /* must try to load the entry from disk. */

        hit = FALSE;

        if(NULL == (thing = H5C_load_entry(f, dxpl_id, 
#ifdef H5_HAVE_PARALLEL
                                           coll_access, 
#endif /* H5_HAVE_PARALLEL */
                                           type, addr, udata)))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTLOAD, NULL, "can't load entry")

        entry_ptr = (H5C_cache_entry_t *)thing;
        entry_ptr->ring  = ring;
#ifdef H5_HAVE_PARALLEL
        if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI) && entry_ptr->coll_access)
            H5C__INSERT_IN_COLL_LIST(cache_ptr, entry_ptr, NULL)
#endif /* H5_HAVE_PARALLEL */

        /* Apply tag to newly protected entry */
        if(H5C_tag_entry(cache_ptr, entry_ptr, dxpl_id) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, NULL, "Cannot tag metadata entry")

	/* Set the entry's cork status */
	if(H5C_cork(cache_ptr, entry_ptr->tag, H5C__GET_CORKED, &entry_ptr->is_corked) < 0)
	    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, "Cannot retrieve entry's cork status")

        /* If the entry is very large, and we are configured to allow it,
         * we may wish to perform a flash cache size increase.
         */
        if ( ( cache_ptr->flash_size_increase_possible ) &&
             ( entry_ptr->size > cache_ptr->flash_size_increase_threshold ) ) {

            if(H5C__flash_increase_cache_size(cache_ptr, 0, entry_ptr->size) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, "H5C__flash_increase_cache_size failed.")
        }

        if(cache_ptr->index_size >= cache_ptr->max_cache_size)
           empty_space = 0;
        else
           empty_space = cache_ptr->max_cache_size - cache_ptr->index_size;

	/* try to free up if necceary and if evictions are permitted.  Note
	 * that if evictions are enabled, we will call H5C_make_space_in_cache()
	 * regardless if the min_free_space requirement is not met.
	 */
        if ( ( cache_ptr->evictions_enabled ) &&
             ( ( (cache_ptr->index_size + entry_ptr->size) >
	         cache_ptr->max_cache_size)
	       ||
	       ( ( empty_space + cache_ptr->clean_index_size ) <
	         cache_ptr->min_clean_size )
	     )
           ) {

            size_t space_needed;

	    if(empty_space <= entry_ptr->size)
                cache_ptr->cache_full = TRUE;

            if(cache_ptr->check_write_permitted != NULL) {
                if((cache_ptr->check_write_permitted)(f, &write_permitted) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, "Can't get write_permitted 1")
                else
                    have_write_permitted = TRUE;
            } else {

                write_permitted = cache_ptr->write_permitted;

                have_write_permitted = TRUE;

            }

            HDassert( entry_ptr->size <= H5C_MAX_ENTRY_SIZE );

            space_needed = entry_ptr->size;

            if ( space_needed > cache_ptr->max_cache_size ) {

                space_needed = cache_ptr->max_cache_size;
            }

            /* Note that space_needed is just the amount of space that
             * needed to insert the new entry without exceeding the cache
             * size limit.  The subsequent call to H5C_make_space_in_cache()
             * may evict the entries required to free more or less space
             * depending on conditions.  It MAY be less if the cache is
             * currently undersized, or more if the cache is oversized.
             *
             * The cache can exceed its maximum size limit via the following
             * mechanisms:
             *
             * First, it is possible for the cache to grow without
             * bound as long as entries are protected and not unprotected.
             *
             * Second, when writes are not permitted it is also possible
             * for the cache to grow without bound.
	     *
	     * Third, the user may choose to disable evictions -- causing
	     * the cache to grow without bound until evictions are
	     * re-enabled.
             *
             * Finally, we usually don't check to see if the cache is
             * oversized at the end of an unprotect.  As a result, it is
             * possible to have a vastly oversized cache with no protected
             * entries as long as all the protects preceed the unprotects.
             *
             * Since items 1, 2, and 3 are not changing any time soon, I
             * see no point in worrying about the fourth.
             */

            if(H5C_make_space_in_cache(f, dxpl_id, space_needed, write_permitted) < 0 )
                HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, "H5C_make_space_in_cache failed 1.")
        }

        /* Insert the entry in the hash table.  It can't be dirty yet, so
         * we don't even check to see if it should go in the skip list.
         *
         * This is no longer true -- due to a bug fix, we may modify
         * data on load to repair a file.
         *
         *   *******************************************
         *
         * Set the flush_last field
 	 * of the newly loaded entry before inserting it into the 
         * index.  Must do this, as the index tracked the number of 
         * entries with the flush_last field set, but assumes that 
         * the field will not change after insertion into the index.
         *
         * Note that this means that the H5C__FLUSH_LAST_FLAG flag 
         * is ignored if the entry is already in cache.
         */
        entry_ptr->flush_me_last = flush_last;

        H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, NULL)

        if ( ( entry_ptr->is_dirty ) && ( ! (entry_ptr->in_slist) ) ) {

            H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, NULL)
        }

        /* insert the entry in the data structures used by the replacement
         * policy.  We are just going to take it out again when we update
         * the replacement policy for a protect, but this simplifies the
         * code.  If we do this often enough, we may want to optimize this.
         */
        H5C__UPDATE_RP_FOR_INSERTION(cache_ptr, entry_ptr, NULL)

        /* If the entry's type has a 'notify' callback send a 'after load'
         * notice now that the entry is fully integrated into the cache.
         */
        if(entry_ptr->type->notify &&
                (entry_ptr->type->notify)(H5C_NOTIFY_ACTION_AFTER_LOAD, entry_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTNOTIFY, NULL, "can't notify client about entry inserted into cache")
    }

    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->type == type );

    if ( entry_ptr->is_protected ) {

	if ( ( read_only ) && ( entry_ptr->is_read_only ) ) {

	    HDassert( entry_ptr->ro_ref_count > 0 );

	    (entry_ptr->ro_ref_count)++;

	} else {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, \
                        "Target already protected & not read only?!?.")
	}
    } else {

    	H5C__UPDATE_RP_FOR_PROTECT(cache_ptr, entry_ptr, NULL)

    	entry_ptr->is_protected = TRUE;

	if ( read_only ) {

	    entry_ptr->is_read_only = TRUE;
	    entry_ptr->ro_ref_count = 1;
	}

    	entry_ptr->dirtied = FALSE;
    }

    H5C__UPDATE_CACHE_HIT_RATE_STATS(cache_ptr, hit)

    H5C__UPDATE_STATS_FOR_PROTECT(cache_ptr, entry_ptr, hit)

    ret_value = thing;

    if ( ( cache_ptr->evictions_enabled ) &&
         ( ( cache_ptr->size_decreased ) ||
           ( ( cache_ptr->resize_enabled ) &&
             ( cache_ptr->cache_accesses >=
               (cache_ptr->resize_ctl).epoch_length ) ) ) ) {

        if ( ! have_write_permitted ) {

            if ( cache_ptr->check_write_permitted != NULL ) {
                if((cache_ptr->check_write_permitted)(f, &write_permitted) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, "Can't get write_permitted 2")
                else
                    have_write_permitted = TRUE;
            } else {

                write_permitted = cache_ptr->write_permitted;

                have_write_permitted = TRUE;

            }
        }

        if ( ( cache_ptr->resize_enabled ) &&
             ( cache_ptr->cache_accesses >=
               (cache_ptr->resize_ctl).epoch_length ) ) {

            if(H5C__auto_adjust_cache_size(f, dxpl_id, write_permitted) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, "Cache auto-resize failed.")
        }

        if ( cache_ptr->size_decreased  ) {

            cache_ptr->size_decreased = FALSE;

            /* check to see if the cache is now oversized due to the cache
             * size reduction.  If it is, try to evict enough entries to
             * bring the cache size down to the current maximum cache size.
	     *
	     * Also, if the min_clean_size requirement is not met, we
	     * should also call H5C_make_space_in_cache() to bring us
	     * into complience.
             */

            if(cache_ptr->index_size >= cache_ptr->max_cache_size)
               empty_space = 0;
            else
               empty_space = cache_ptr->max_cache_size - cache_ptr->index_size;

            if ( ( cache_ptr->index_size > cache_ptr->max_cache_size )
	         ||
	         ( ( empty_space + cache_ptr->clean_index_size ) <
	           cache_ptr->min_clean_size) ) {

		if(cache_ptr->index_size > cache_ptr->max_cache_size)
                    cache_ptr->cache_full = TRUE;

                if(H5C_make_space_in_cache(f, dxpl_id, (size_t)0, write_permitted) < 0 )
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, "H5C_make_space_in_cache failed 2.")
            }
        }
    }

#ifdef ASK
    /* If we loaded the entry and the entry's type has a 'notify' callback, send
     * a 'after insertion' notice now that the entry is fully integrated into
     * the cache and protected.  We must wait until it is protected so it is not
     * evicted during the notify callback.
     */
    if(!hit && entry_ptr->type->notify &&
            (entry_ptr->type->notify)(H5C_NOTIFY_ACTION_AFTER_LOAD, entry_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTNOTIFY, NULL, "can't notify client about entry inserted into cache")
#endif

#ifdef H5_HAVE_PARALLEL
    if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI)) {
        /* Make sure the size of the collective entries in the cache remain in check */
        if(coll_access) {
            if(H5P_USER_TRUE == f->coll_md_read) {
                if(cache_ptr->max_cache_size * 80 < cache_ptr->coll_list_size * 100)
                    if(H5C_clear_coll_entries(cache_ptr, TRUE) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, NULL, "can't clear collective metadata entries")
            } /* end if */
            else {
                if(cache_ptr->max_cache_size * 40 < cache_ptr->coll_list_size * 100)
                    if(H5C_clear_coll_entries(cache_ptr, TRUE) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, NULL, "can't clear collective metadata entries")
            } /* end else */
        } /* end if */
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

done:

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) )
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, "an extreme sanity check failed on exit.\n")
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_protect() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_reset_cache_hit_rate_stats()
 *
 * Purpose:     Reset the cache hit rate computation fields.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer, 10/5/04
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_reset_cache_hit_rate_stats(H5C_t * cache_ptr)
{
    herr_t	ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    cache_ptr->cache_hits		= 0;
    cache_ptr->cache_accesses		= 0;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_reset_cache_hit_rate_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_cache_auto_resize_config
 *
 * Purpose:	Set the cache automatic resize configuration to the
 *		provided values if they are in range, and fail if they
 *		are not.
 *
 *		If the new configuration enables automatic cache resizing,
 *		coerce the cache max size and min clean size into agreement
 *		with the new policy and re-set the full cache hit rate
 *		stats.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *		10/8/04
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_set_cache_auto_resize_config(H5C_t *cache_ptr,
                                 H5C_auto_size_ctl_t *config_ptr)
{
    herr_t	result;
    size_t      new_max_cache_size;
    size_t      new_min_clean_size;
    herr_t	ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    if ( config_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "NULL config_ptr on entry.")
    }

    if ( config_ptr->version != H5C__CURR_AUTO_SIZE_CTL_VER ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Unknown config version.")
    }

    /* check general configuration section of the config: */
    if ( SUCCEED != H5C_validate_resize_config(config_ptr,
                                   H5C_RESIZE_CFG__VALIDATE_GENERAL) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, \
                    "error in general configuration fields of new config.")
    }

    /* check size increase control fields of the config: */
    if ( SUCCEED != H5C_validate_resize_config(config_ptr,
                                   H5C_RESIZE_CFG__VALIDATE_INCREMENT) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, \
                    "error in the size increase control fields of new config.")
    }

    /* check size decrease control fields of the config: */
    if ( SUCCEED != H5C_validate_resize_config(config_ptr,
                                   H5C_RESIZE_CFG__VALIDATE_DECREMENT) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, \
                    "error in the size decrease control fields of new config.")
    }

    /* check for conflicts between size increase and size decrease controls: */
    if ( SUCCEED != H5C_validate_resize_config(config_ptr,
                                   H5C_RESIZE_CFG__VALIDATE_INTERACTIONS) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, \
                    "conflicting threshold fields in new config.")
    }

    /* will set the increase possible fields to FALSE later if needed */
    cache_ptr->size_increase_possible       = TRUE;
    cache_ptr->flash_size_increase_possible = TRUE;
    cache_ptr->size_decrease_possible       = TRUE;

    switch ( config_ptr->incr_mode )
    {
        case H5C_incr__off:
            cache_ptr->size_increase_possible = FALSE;
            break;

        case H5C_incr__threshold:
            if ( ( config_ptr->lower_hr_threshold <= (double)0.0f ) ||
                 ( config_ptr->increment <= (double)1.0f ) ||
                 ( ( config_ptr->apply_max_increment ) &&
                   ( config_ptr->max_increment <= 0 ) ) ) {

                 cache_ptr->size_increase_possible = FALSE;
            }
            break;

        default: /* should be unreachable */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Unknown incr_mode?!?!?.")
    }

    /* logically, this is were configuration for flash cache size increases
     * should go.  However, this configuration depends on max_cache_size, so
     * we wait until the end of the function, when this field is set.
     */

    switch ( config_ptr->decr_mode )
    {
        case H5C_decr__off:
            cache_ptr->size_decrease_possible = FALSE;
            break;

        case H5C_decr__threshold:
            if ( ( config_ptr->upper_hr_threshold >= (double)1.0f ) ||
                 ( config_ptr->decrement >= (double)1.0f ) ||
                 ( ( config_ptr->apply_max_decrement ) &&
                   ( config_ptr->max_decrement <= 0 ) ) ) {

                cache_ptr->size_decrease_possible = FALSE;
            }
            break;

        case H5C_decr__age_out:
            if ( ( ( config_ptr->apply_empty_reserve ) &&
                   ( config_ptr->empty_reserve >= (double)1.0f ) ) ||
                 ( ( config_ptr->apply_max_decrement ) &&
                   ( config_ptr->max_decrement <= 0 ) ) ) {

                cache_ptr->size_decrease_possible = FALSE;
            }
            break;

        case H5C_decr__age_out_with_threshold:
            if ( ( ( config_ptr->apply_empty_reserve ) &&
                   ( config_ptr->empty_reserve >= (double)1.0f ) ) ||
                 ( ( config_ptr->apply_max_decrement ) &&
                   ( config_ptr->max_decrement <= 0 ) ) ||
                 ( config_ptr->upper_hr_threshold >= (double)1.0f ) ) {

                cache_ptr->size_decrease_possible = FALSE;
            }
            break;

        default: /* should be unreachable */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Unknown decr_mode?!?!?.")
    }

    if ( config_ptr->max_size == config_ptr->min_size ) {

        cache_ptr->size_increase_possible = FALSE;
	cache_ptr->flash_size_increase_possible = FALSE;
        cache_ptr->size_decrease_possible = FALSE;
    }

    /* flash_size_increase_possible is intentionally omitted from the
     * following:
     */
    cache_ptr->resize_enabled = cache_ptr->size_increase_possible ||
                                cache_ptr->size_decrease_possible;

    cache_ptr->resize_ctl = *config_ptr;

    /* Resize the cache to the supplied initial value if requested, or as
     * necessary to force it within the bounds of the current automatic
     * cache resizing configuration.
     *
     * Note that the min_clean_fraction may have changed, so we
     * go through the exercise even if the current size is within
     * range and an initial size has not been provided.
     */
    if ( (cache_ptr->resize_ctl).set_initial_size ) {

        new_max_cache_size = (cache_ptr->resize_ctl).initial_size;
    }
    else if ( cache_ptr->max_cache_size > (cache_ptr->resize_ctl).max_size ) {

        new_max_cache_size = (cache_ptr->resize_ctl).max_size;
    }
    else if ( cache_ptr->max_cache_size < (cache_ptr->resize_ctl).min_size ) {

        new_max_cache_size = (cache_ptr->resize_ctl).min_size;

    } else {

        new_max_cache_size = cache_ptr->max_cache_size;
    }

    new_min_clean_size = (size_t)
                         ((double)new_max_cache_size *
                          ((cache_ptr->resize_ctl).min_clean_fraction));


    /* since new_min_clean_size is of type size_t, we have
     *
     * 	( 0 <= new_min_clean_size )
     *
     * by definition.
     */
    HDassert( new_min_clean_size <= new_max_cache_size );
    HDassert( (cache_ptr->resize_ctl).min_size <= new_max_cache_size );
    HDassert( new_max_cache_size <= (cache_ptr->resize_ctl).max_size );

    if ( new_max_cache_size < cache_ptr->max_cache_size ) {

        cache_ptr->size_decreased = TRUE;
    }

    cache_ptr->max_cache_size = new_max_cache_size;
    cache_ptr->min_clean_size = new_min_clean_size;

    if ( H5C_reset_cache_hit_rate_stats(cache_ptr) != SUCCEED ) {

        /* this should be impossible... */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_reset_cache_hit_rate_stats failed.")
    }

    /* remove excess epoch markers if any */
    if ( ( config_ptr->decr_mode == H5C_decr__age_out_with_threshold ) ||
         ( config_ptr->decr_mode == H5C_decr__age_out ) ) {

        if ( cache_ptr->epoch_markers_active >
             (cache_ptr->resize_ctl).epochs_before_eviction ) {

            result =
                H5C__autoadjust__ageout__remove_excess_markers(cache_ptr);

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "can't remove excess epoch markers.")
            }
        }
    } else if ( cache_ptr->epoch_markers_active > 0 ) {

        result = H5C__autoadjust__ageout__remove_all_markers(cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "error removing all epoch markers.")
        }
    }

    /* configure flash size increase facility.  We wait until the
     * end of the function, as we need the max_cache_size set before
     * we start to keep things simple.
     *
     * If we haven't already ruled out flash cache size increases above,
     * go ahead and configure it.
     */

    if ( cache_ptr->flash_size_increase_possible ) {

        switch ( config_ptr->flash_incr_mode )
        {
            case H5C_flash_incr__off:
                cache_ptr->flash_size_increase_possible = FALSE;
                break;

            case H5C_flash_incr__add_space:
                cache_ptr->flash_size_increase_possible = TRUE;
                cache_ptr->flash_size_increase_threshold =
                    (size_t)
                    (((double)(cache_ptr->max_cache_size)) *
                     ((cache_ptr->resize_ctl).flash_threshold));
                break;

            default: /* should be unreachable */
                 HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                             "Unknown flash_incr_mode?!?!?.")
                 break;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_set_cache_auto_resize_config() */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_evictions_enabled()
 *
 * Purpose:     Set cache_ptr->evictions_enabled to the value of the
 *              evictions enabled parameter.
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *              7/27/07
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_set_evictions_enabled(H5C_t *cache_ptr, hbool_t evictions_enabled)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if((cache_ptr == NULL) || (cache_ptr->magic != H5C__H5C_T_MAGIC))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")

    /* There is no fundamental reason why we should not permit
     * evictions to be disabled while automatic resize is enabled.
     * However, I can't think of any good reason why one would
     * want to, and allowing it would greatly complicate testing
     * the feature.  Hence the following:
     */
    if((evictions_enabled != TRUE) &&
         ((cache_ptr->resize_ctl.incr_mode != H5C_incr__off) ||
	   (cache_ptr->resize_ctl.decr_mode != H5C_decr__off)))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't disable evictions when auto resize enabled.")

    cache_ptr->evictions_enabled = evictions_enabled;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_set_evictions_enabled() */


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

    if ( ( cache_ptr == NULL ) ||
         ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ||
         ( prefix == NULL ) ||
         ( HDstrlen(prefix) >= H5C__PREFIX_LEN ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad param(s) on entry.")
    }

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
H5C_set_trace_file_ptr(H5C_t * cache_ptr,
                       FILE * trace_file_ptr)
{
    herr_t		ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( ! cache_ptr ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr")
    }

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
    if ( ( ! cache_ptr ) ||
         ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ||
         ( !cache_name ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr or cache_name")
    }

#if H5C_COLLECT_CACHE_STATS

    for ( i = 0; i <= cache_ptr->max_type_id; i++ ) {

        total_hits              += cache_ptr->hits[i];
        total_misses            += cache_ptr->misses[i];
	total_write_protects	+= cache_ptr->write_protects[i];
	total_read_protects	+= cache_ptr->read_protects[i];
	if ( max_read_protects < cache_ptr->max_read_protects[i] ) {
	    max_read_protects = cache_ptr->max_read_protects[i];
	}
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
    if ( aggregate_max_size < cache_ptr->max_size[i] )
        aggregate_max_size = cache_ptr->max_size[i];
    if ( aggregate_max_pins < cache_ptr->max_pins[i] )
        aggregate_max_pins = cache_ptr->max_pins[i];
#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
    }

    if ( ( total_hits > 0 ) || ( total_misses > 0 ) ) {

        hit_rate = (double)100.0f * ((double)(total_hits)) /
                   ((double)(total_hits + total_misses));
    } else {
        hit_rate = 0.0f;
    }

    if ( cache_ptr->successful_ht_searches > 0 ) {

        average_successful_search_depth =
            ((double)(cache_ptr->total_successful_ht_search_depth)) /
            ((double)(cache_ptr->successful_ht_searches));
    }

    if ( cache_ptr->failed_ht_searches > 0 ) {

        average_failed_search_depth =
            ((double)(cache_ptr->total_failed_ht_search_depth)) /
            ((double)(cache_ptr->failed_ht_searches));
    }


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

    if (cache_ptr->calls_to_msic > 0) {
        average_entries_scanned_per_calls_to_msic =
            (((double)(cache_ptr->total_entries_scanned_in_msic)) /
            ((double)(cache_ptr->calls_to_msic)));
    }

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

    if ( display_detailed_stats )
    {

        for ( i = 0; i <= cache_ptr->max_type_id; i++ ) {

            HDfprintf(stdout, "\n");

            HDfprintf(stdout, "%s  Stats on %s:\n",
                      cache_ptr->prefix,
                      ((cache_ptr->type_name_table_ptr))[i]);

            if ( ( cache_ptr->hits[i] > 0 ) || ( cache_ptr->misses[i] > 0 ) ) {

                hit_rate = (double)100.0f * ((double)(cache_ptr->hits[i])) /
                          ((double)(cache_ptr->hits[i] + cache_ptr->misses[i]));
            } else {
                hit_rate = 0.0f;
            }

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

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

#if H5C_COLLECT_CACHE_STATS
    for ( i = 0; i <= cache_ptr->max_type_id; i++ )
    {
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
    }

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

    for ( i = 0; i <= cache_ptr->max_type_id; i++ )
    {
        cache_ptr->max_accesses[i]		= 0;
        cache_ptr->min_accesses[i]		= 1000000;
        cache_ptr->max_clears[i]		= 0;
        cache_ptr->max_flushes[i]		= 0;
        cache_ptr->max_size[i]			= (size_t)0;
        cache_ptr->max_pins[i]			= 0;
    }

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
#endif /* H5C_COLLECT_CACHE_STATS */

    return;

} /* H5C_stats__reset() */


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
H5C_dump_cache(H5C_t * cache_ptr,
               const char *  cache_name)
{
    herr_t              ret_value = SUCCEED;   /* Return value */
    int                 i;
    H5C_cache_entry_t * entry_ptr = NULL;
    H5SL_t *            slist_ptr = NULL;
    H5SL_node_t *       node_ptr = NULL;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_name != NULL );

    /* First, create a skip list */
    slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL);

    if ( slist_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "can't create skip list.")
    }

    /* Next, scan the index, and insert all entries in the skip list.
     * Do this, as we want to display cache entries in increasing address
     * order.
     */
    for ( i = 0; i < H5C__HASH_TABLE_LEN; i++ ) {

        entry_ptr = cache_ptr->index[i];

        while ( entry_ptr != NULL ) {

            HDassert( entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC );

            if ( H5SL_insert(slist_ptr, entry_ptr, &(entry_ptr->addr)) < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, \
                            "Can't insert entry in skip list")
            }

            entry_ptr = entry_ptr->ht_next;
        }
    }

    /* If we get this far, all entries in the cache are listed in the
     * skip list -- scan the skip list generating the desired output.
     */

    HDfprintf(stdout, "\n\nDump of metadata cache \"%s\".\n", cache_name);
    HDfprintf(stdout,
        "Num:    Addr:                             Tag:         Len:    Type:   Prot:   Pinned: Dirty: Corked:\n");

    i = 0;

    node_ptr = H5SL_first(slist_ptr);

    if ( node_ptr != NULL ) {

        entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

    } else {

        entry_ptr = NULL;
    }

    while ( entry_ptr != NULL ) {

        HDassert( entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC );

        HDfprintf(stdout,
            "%s%d       0x%16llx                0x%3llx        0x%3llx      %2d     %d      %d      %d       %d\n",
             cache_ptr->prefix, i,
             (long long)(entry_ptr->addr),
             (long long)(entry_ptr->tag),
             (long long)(entry_ptr->size),
             (int)(entry_ptr->type->id),
             (int)(entry_ptr->is_protected),
             (int)(entry_ptr->is_pinned),
             (int)(entry_ptr->is_dirty),
	     (int)(entry_ptr->is_corked));

        /* increment node_ptr before we delete its target */
        node_ptr = H5SL_next(node_ptr);

        /* remove the first item in the skip list */
        if ( H5SL_remove(slist_ptr, &(entry_ptr->addr)) != entry_ptr ) {

            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, \
                        "Can't delete entry from skip list.")
        }

        if ( node_ptr != NULL ) {

            entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

        } else {

            entry_ptr = NULL;
        }

        i++;
    }

    HDfprintf(stdout, "\n\n");

    /* Finally, discard the skip list */

    HDassert( H5SL_count(slist_ptr) == 0 );

    H5SL_close(slist_ptr);

done:

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

    HDfprintf(stdout, "\n\nDumping metadata cache skip list from %s.\n",
              calling_fcn);
    HDfprintf(stdout, "	slist len = %d.\n", cache_ptr->slist_len);
    HDfprintf(stdout, "	slist size = %lld.\n", 
              (long long)(cache_ptr->slist_size));

    if ( cache_ptr->slist_len > 0 )
    {
        /* If we get this far, all entries in the cache are listed in the
         * skip list -- scan the skip list generating the desired output.
         */

        HDfprintf(stdout,
                  "Num:    Addr:               Len: Prot/Pind: Dirty: Type:\n");

        i = 0;

        node_ptr = H5SL_first(cache_ptr->slist_ptr);

        if ( node_ptr != NULL ) {

            entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

        } else {

            entry_ptr = NULL;
        }

        while ( entry_ptr != NULL ) {

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

            if ( node_ptr != NULL ) {

                entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

            } else {

                entry_ptr = NULL;
            }

            i++;
        }
    }

    HDfprintf(stdout, "\n\n");

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_dump_cache_skip_list() */
#endif /* debugging routine */


/*-------------------------------------------------------------------------
 * Function:    H5C_unpin_entry_from_client()
 *
 * Purpose:	Internal routine to unpin a cache entry from a client action.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              3/24/09
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_unpin_entry_from_client(H5C_t *		  cache_ptr,
                H5C_cache_entry_t *	  entry_ptr,
                hbool_t update_rp)
{
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity checking */
    HDassert( cache_ptr );
    HDassert( entry_ptr );

    /* Error checking (should be sanity checks?) */
    if(!entry_ptr->is_pinned)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "Entry isn't pinned")
    if(!entry_ptr->pinned_from_client)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "Entry wasn't pinned by cache client")

    /* Check if the entry is not pinned from a flush dependency */
    if(!entry_ptr->pinned_from_cache) {
        /* If requested, update the replacement policy if the entry is not protected */
        if(update_rp && !entry_ptr->is_protected)
            H5C__UPDATE_RP_FOR_UNPIN(cache_ptr, entry_ptr, FAIL)

        /* Unpin the entry now */
        entry_ptr->is_pinned = FALSE;

        /* Update the stats for an unpin operation */
        H5C__UPDATE_STATS_FOR_UNPIN(cache_ptr, entry_ptr)
    } /* end if */

    /* Mark the entry as explicitly unpinned by the client */
    entry_ptr->pinned_from_client = FALSE;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_unpin_entry_from_client() */


/*-------------------------------------------------------------------------
 * Function:    H5C_unpin_entry()
 *
 * Purpose:	Unpin a cache entry.  The entry can be either protected or
 * 		unprotected at the time of call, but must be pinned.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              3/22/06
 *
 * Changes:	Added extreme sanity checks on entry and exit.
                				JRM -- 4/26/14 
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_unpin_entry(void *_entry_ptr)
{
    H5C_t             * cache_ptr;
    H5C_cache_entry_t * entry_ptr = (H5C_cache_entry_t *)_entry_ptr; /* Pointer to entry to unpin */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(entry_ptr);
    cache_ptr = entry_ptr->cache_ptr;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on entry.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */


    /* Unpin the entry */
    if(H5C_unpin_entry_from_client(cache_ptr, entry_ptr, TRUE) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "Can't unpin entry from client")

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

} /* H5C_unpin_entry() */


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
 *-------------------------------------------------------------------------
 */
herr_t
H5C_unprotect(H5F_t *		  f,
              hid_t		  dxpl_id,
              haddr_t		  addr,
              void *		  thing,
              unsigned int        flags)
{
    H5C_t *             cache_ptr;
    hbool_t		deleted;
    hbool_t		dirtied;
    hbool_t             set_flush_marker;
    hbool_t		pin_entry;
    hbool_t		unpin_entry;
    hbool_t		free_file_space;
    hbool_t		take_ownership;
    hbool_t 		was_clean;
#ifdef H5_HAVE_PARALLEL
    hbool_t		clear_entry = FALSE;
#endif /* H5_HAVE_PARALLEL */
    H5C_cache_entry_t *	entry_ptr;
    H5C_cache_entry_t *	test_entry_ptr;
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    deleted                = ((flags & H5C__DELETED_FLAG) != 0);
    dirtied                = ((flags & H5C__DIRTIED_FLAG) != 0);
    set_flush_marker       = ((flags & H5C__SET_FLUSH_MARKER_FLAG) != 0);
    pin_entry              = ((flags & H5C__PIN_ENTRY_FLAG) != 0);
    unpin_entry            = ((flags & H5C__UNPIN_ENTRY_FLAG) != 0);
    free_file_space        = ((flags & H5C__FREE_FILE_SPACE_FLAG) != 0);
    take_ownership         = ((flags & H5C__TAKE_OWNERSHIP_FLAG) != 0);

    HDassert( f );
    HDassert( f->shared );

    cache_ptr = f->shared->cache;

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( H5F_addr_defined(addr) );
    HDassert( thing );
    HDassert( ! ( pin_entry && unpin_entry ) );
    HDassert( ( ! free_file_space ) || ( deleted ) );   /* deleted flag must accompany free_file_space */
    HDassert( ( ! take_ownership ) || ( deleted ) );    /* deleted flag must accompany take_ownership */
    HDassert( ! ( free_file_space && take_ownership ) );    /* can't have both free_file_space & take_ownership */

    entry_ptr = (H5C_cache_entry_t *)thing;

    HDassert( entry_ptr->addr == addr );

    /* also set the dirtied variable if the dirtied field is set in
     * the entry.
     */
    dirtied |= entry_ptr->dirtied;
    was_clean = ! ( entry_ptr->is_dirty );

#if H5C_DO_EXTREME_SANITY_CHECKS
    if ( ( H5C_validate_protected_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_pinned_entry_list(cache_ptr) < 0 ) ||
         ( H5C_validate_lru_list(cache_ptr) < 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "an extreme sanity check failed on entry.\n");
    }
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    /* if the entry has multiple read only protects, just decrement
     * the ro_ref_counter.  Don't actually unprotect until the ref count
     * drops to zero.
     */
    if(entry_ptr->ro_ref_count > 1) {
        /* Sanity check */
	HDassert(entry_ptr->is_protected);
        HDassert(entry_ptr->is_read_only);

	if(dirtied)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "Read only entry modified??")

        /* Reduce the RO ref count */
	(entry_ptr->ro_ref_count)--;

        /* Pin or unpin the entry as requested. */
        if(pin_entry) {
            /* Pin the entry from a client */
            if(H5C_pin_entry_from_client(cache_ptr, entry_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTPIN, FAIL, "Can't pin entry by client")
        } else if(unpin_entry) {
            /* Unpin the entry from a client */
            if(H5C_unpin_entry_from_client(cache_ptr, entry_ptr, FALSE) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "Can't unpin entry by client")
        } /* end if */

    } else {
	if(entry_ptr->is_read_only) {
            /* Sanity check */
	    HDassert(entry_ptr->ro_ref_count == 1);

	    if(dirtied)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "Read only entry modified??")

	    entry_ptr->is_read_only = FALSE;
	    entry_ptr->ro_ref_count = 0;
	} /* end if */

#ifdef H5_HAVE_PARALLEL
        /* When the H5C code is used to implement the metadata cache in the
         * PHDF5 case, only the cache on process 0 is allowed to write to file.
         * All the other metadata caches must hold dirty entries until they
         * are told that the entries are clean.
         *
         * The clear_on_unprotect flag in the H5C_cache_entry_t structure
         * exists to deal with the case in which an entry is protected when
         * its cache receives word that the entry is now clean.  In this case,
         * the clear_on_unprotect flag is set, and the entry is flushed with
         * the H5C__FLUSH_CLEAR_ONLY_FLAG.
         *
         * All this is a bit awkward, but until the metadata cache entries
         * are contiguous, with only one dirty flag, we have to let the supplied
         * functions deal with the reseting the is_dirty flag.
         */
        if(entry_ptr->clear_on_unprotect) {
            /* Sanity check */
            HDassert(entry_ptr->is_dirty);

            entry_ptr->clear_on_unprotect = FALSE;
            if(!dirtied)
                clear_entry = TRUE;
        } /* end if */
#endif /* H5_HAVE_PARALLEL */

        if(!entry_ptr->is_protected)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "Entry already unprotected??")

        /* Mark the entry as dirty if appropriate */
        entry_ptr->is_dirty = (entry_ptr->is_dirty || dirtied);

        /* the image_up_to_date field was introduced to support 
         * journaling.  Until we re-introduce journaling, this 
         * field should be equal to !entry_ptr->is_dirty.  
         *
         * When journaling is re-enabled it should be set 
         * to FALSE if dirtied is TRUE.
         */
#if 1 /* JRM */
	entry_ptr->image_up_to_date = FALSE;
#else /* JRM */
	entry_ptr->image_up_to_date = !entry_ptr->is_dirty;
#endif /* JRM */

        /* Update index for newly dirtied entry */
        if(was_clean && entry_ptr->is_dirty) {

            /* Propagate the flush dep dirty flag up the flush dependency chain
             * if appropriate */
            if ( ( entry_ptr->flush_dep_ndirty_children == 0) &&
                 ( entry_ptr->flush_dep_nparents > 0 ) ) {

                if ( H5C__mark_flush_dep_dirty(entry_ptr) < 0 )
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "Can't propagate flush dep dirty flag")
            }

            H5C__UPDATE_INDEX_FOR_ENTRY_DIRTY(cache_ptr, entry_ptr)
        } else if ( ! ( was_clean ) && ! ( entry_ptr->is_dirty ) ) {

            /* Propagate the flush dep clean flag up the flush dependency chain
             * if appropriate */
            if ( ( entry_ptr->flush_dep_ndirty_children == 0) &&
                 ( entry_ptr->flush_dep_nparents > 0 ) ) {

                if ( H5C__mark_flush_dep_clean(entry_ptr) < 0 )
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL,  "Can't propagate flush dep dirty flag")
            }
        }

        /* Pin or unpin the entry as requested. */
        if(pin_entry) {
            /* Pin the entry from a client */
            if(H5C_pin_entry_from_client(cache_ptr, entry_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTPIN, FAIL, "Can't pin entry by client")
        } else if(unpin_entry) {
            /* Unpin the entry from a client */
            if(H5C_unpin_entry_from_client(cache_ptr, entry_ptr, FALSE) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "Can't unpin entry by client")
        } /* end if */

        /* H5C__UPDATE_RP_FOR_UNPROTECT will place the unprotected entry on
         * the pinned entry list if entry_ptr->is_pinned is TRUE.
         */
        H5C__UPDATE_RP_FOR_UNPROTECT(cache_ptr, entry_ptr, FAIL)

        entry_ptr->is_protected = FALSE;

        /* if the entry is dirty, 'or' its flush_marker with the set flush flag,
         * and then add it to the skip list if it isn't there already.
         */
        if(entry_ptr->is_dirty) {
            entry_ptr->flush_marker |= set_flush_marker;
            if(!entry_ptr->in_slist)
                H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, FAIL)
        } /* end if */

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
            unsigned    flush_flags = (H5C__FLUSH_CLEAR_ONLY_FLAG |
                                         H5C__FLUSH_INVALIDATE_FLAG);

	    /* we can't delete a pinned entry */
	    HDassert ( ! (entry_ptr->is_pinned ) );

            /* verify that the target entry is in the cache. */
            H5C__SEARCH_INDEX(cache_ptr, addr, test_entry_ptr, FAIL)
            if(test_entry_ptr == NULL)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "entry not in hash table?!?.")
            else if(test_entry_ptr != entry_ptr)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "hash table contains multiple entries for addr?!?.")

            /* Set the 'free file space' flag for the flush, if needed */
            if(free_file_space)
                flush_flags |= H5C__FREE_FILE_SPACE_FLAG;

            /* Set the "take ownership" flag for the flush, if needed */
            if(take_ownership)
                flush_flags |= H5C__TAKE_OWNERSHIP_FLAG;

            /* Delete the entry from the skip list on destroy */
            flush_flags |= H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG;

            if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, flush_flags, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "Can't flush entry")

#if H5C_DO_SANITY_CHECKS
	    if ( ( take_ownership ) && ( ! was_clean ) )
            {
                /* we have just removed an entry from the skip list.  Thus 
                 * we must touch up cache_ptr->slist_len_increase and
                 * cache_ptr->slist_size_increase to keep from skewing
                 * the sanity checks on flushes.
                 */
                cache_ptr->slist_len_increase -= 1;
                cache_ptr->slist_size_increase -= (int64_t)(entry_ptr->size);
            }
#endif /* H5C_DO_SANITY_CHECKS */
        }
#ifdef H5_HAVE_PARALLEL
        else if ( clear_entry ) {

            /* verify that the target entry is in the cache. */
            H5C__SEARCH_INDEX(cache_ptr, addr, test_entry_ptr, FAIL)
            if(test_entry_ptr == NULL)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "entry not in hash table?!?.")
            else if(test_entry_ptr != entry_ptr)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "hash table contains multiple entries for addr?!?.")

            if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "Can't clear entry")
        }
#endif /* H5_HAVE_PARALLEL */
    }

    H5C__UPDATE_STATS_FOR_UNPROTECT(cache_ptr)

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

} /* H5C_unprotect() */


/*-------------------------------------------------------------------------
 * Function:    H5C_validate_resize_config()
 *
 * Purpose:	Run a sanity check on the specified sections of the
 *		provided instance of struct H5C_auto_size_ctl_t.
 *
 *		Do nothing and return SUCCEED if no errors are detected,
 *		and flag an error and return FAIL otherwise.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              3/23/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_validate_resize_config(H5C_auto_size_ctl_t * config_ptr,
                           unsigned int tests)
{
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if ( config_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "NULL config_ptr on entry.")
    }

    if ( config_ptr->version != H5C__CURR_AUTO_SIZE_CTL_VER ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Unknown config version.")
    }


    if ( (tests & H5C_RESIZE_CFG__VALIDATE_GENERAL) != 0 ) {

        if(config_ptr->max_size > H5C__MAX_MAX_CACHE_SIZE)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "max_size too big")

        if(config_ptr->min_size < H5C__MIN_MAX_CACHE_SIZE)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "min_size too small")

        if(config_ptr->min_size > config_ptr->max_size)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "min_size > max_size")

        if ( ( config_ptr->set_initial_size ) &&
             ( ( config_ptr->initial_size < config_ptr->min_size ) ||
               ( config_ptr->initial_size > config_ptr->max_size ) ) ) {

            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                  "initial_size must be in the interval [min_size, max_size]");
        }

        if ( ( config_ptr->min_clean_fraction < (double)0.0f ) ||
             ( config_ptr->min_clean_fraction > (double)1.0f ) ) {

            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                  "min_clean_fraction must be in the interval [0.0, 1.0]");
        }

        if ( config_ptr->epoch_length < H5C__MIN_AR_EPOCH_LENGTH ) {

            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "epoch_length too small");
        }

        if ( config_ptr->epoch_length > H5C__MAX_AR_EPOCH_LENGTH ) {

            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "epoch_length too big");
        }
    } /* H5C_RESIZE_CFG__VALIDATE_GENERAL */


    if ( (tests & H5C_RESIZE_CFG__VALIDATE_INCREMENT) != 0 ) {

        if ( ( config_ptr->incr_mode != H5C_incr__off ) &&
             ( config_ptr->incr_mode != H5C_incr__threshold ) ) {

            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Invalid incr_mode");
        }

        if ( config_ptr->incr_mode == H5C_incr__threshold ) {

            if((config_ptr->lower_hr_threshold < (double)0.0f) ||
                    (config_ptr->lower_hr_threshold > (double)1.0f))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "lower_hr_threshold must be in the range [0.0, 1.0]")

            if(config_ptr->increment < (double)1.0f)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "increment must be greater than or equal to 1.0")

            /* no need to check max_increment, as it is a size_t,
             * and thus must be non-negative.
             */
        } /* H5C_incr__threshold */

        switch ( config_ptr->flash_incr_mode )
        {
            case H5C_flash_incr__off:
                /* nothing to do here */
                break;

            case H5C_flash_incr__add_space:
                if ( ( config_ptr->flash_multiple < (double)0.1f ) ||
                     ( config_ptr->flash_multiple > (double)10.0f ) ) {

                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                        "flash_multiple must be in the range [0.1, 10.0]");
                }

                if ( ( config_ptr->flash_threshold < (double)0.1f ) ||
                     ( config_ptr->flash_threshold > (double)1.0f ) ) {

                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                           "flash_threshold must be in the range [0.1, 1.0]");
                }
                break;

            default:
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                            "Invalid flash_incr_mode");
                break;
        }
    } /* H5C_RESIZE_CFG__VALIDATE_INCREMENT */


    if ( (tests & H5C_RESIZE_CFG__VALIDATE_DECREMENT) != 0 ) {

        if ( ( config_ptr->decr_mode != H5C_decr__off ) &&
             ( config_ptr->decr_mode != H5C_decr__threshold ) &&
             ( config_ptr->decr_mode != H5C_decr__age_out ) &&
             ( config_ptr->decr_mode != H5C_decr__age_out_with_threshold )
           ) {

            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Invalid decr_mode");
        }

        if ( config_ptr->decr_mode == H5C_decr__threshold ) {

            if ( config_ptr->upper_hr_threshold > (double)1.0f ) {

                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                            "upper_hr_threshold must be <= 1.0");
            }

            if ( ( config_ptr->decrement > (double)1.0f ) ||
                 ( config_ptr->decrement < (double)0.0f ) ) {

                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                            "decrement must be in the interval [0.0, 1.0]");
            }

            /* no need to check max_decrement as it is a size_t
             * and thus must be non-negative.
             */
        } /* H5C_decr__threshold */

        if ( ( config_ptr->decr_mode == H5C_decr__age_out ) ||
             ( config_ptr->decr_mode == H5C_decr__age_out_with_threshold )
           ) {

            if ( config_ptr->epochs_before_eviction < 1 ) {

                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                            "epochs_before_eviction must be positive");
            }

            if(config_ptr->epochs_before_eviction > H5C__MAX_EPOCH_MARKERS)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "epochs_before_eviction too big")

            if((config_ptr->apply_empty_reserve) &&
                    ((config_ptr->empty_reserve > (double)1.0f) ||
                        (config_ptr->empty_reserve < (double)0.0f)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "empty_reserve must be in the interval [0.0, 1.0]")

            /* no need to check max_decrement as it is a size_t
             * and thus must be non-negative.
             */
        } /* H5C_decr__age_out || H5C_decr__age_out_with_threshold */

        if ( config_ptr->decr_mode == H5C_decr__age_out_with_threshold ) {

            if ( ( config_ptr->upper_hr_threshold > (double)1.0f ) ||
                 ( config_ptr->upper_hr_threshold < (double)0.0f ) ) {

                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                       "upper_hr_threshold must be in the interval [0.0, 1.0]");
            }
        } /* H5C_decr__age_out_with_threshold */

    } /* H5C_RESIZE_CFG__VALIDATE_DECREMENT */


    if ( (tests & H5C_RESIZE_CFG__VALIDATE_INTERACTIONS) != 0 ) {

        if ( ( config_ptr->incr_mode == H5C_incr__threshold )
             &&
             ( ( config_ptr->decr_mode == H5C_decr__threshold )
               ||
               ( config_ptr->decr_mode == H5C_decr__age_out_with_threshold )
             )
             &&
             ( config_ptr->lower_hr_threshold
               >=
               config_ptr->upper_hr_threshold
             )
           ) {

            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                        "conflicting threshold fields in config.")
        }
    } /* H5C_RESIZE_CFG__VALIDATE_INTERACTIONS */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_validate_resize_config() */


/*-------------------------------------------------------------------------
 * Function:    H5C_create_flush_dependency()
 *
 * Purpose:	Initiates a parent<->child entry flush dependency.  The parent
 *              entry must be pinned or protected at the time of call, and must
 *              have all dependencies removed before the cache can shut down.
 *
 * Note:	Flush dependencies in the cache indicate that a child entry
 *              must be flushed to the file before its parent.  (This is
 *              currently used to implement Single-Writer/Multiple-Reader (SWMR)
 *              I/O access for data structures in the file).
 *
 *              Creating a flush dependency between two entries will also pin
 *              the parent entry.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              3/05/09
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_create_flush_dependency(void * parent_thing, void * child_thing)
{
    H5C_t             * cache_ptr;
    H5C_cache_entry_t *	parent_entry = (H5C_cache_entry_t *)parent_thing;   /* Ptr to parent thing's entry */
    H5C_cache_entry_t * child_entry = (H5C_cache_entry_t *)child_thing;    /* Ptr to child thing's entry */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(parent_entry);
    HDassert(parent_entry->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(H5F_addr_defined(parent_entry->addr));
    HDassert(child_entry);
    HDassert(child_entry->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(H5F_addr_defined(child_entry->addr));
    cache_ptr = parent_entry->cache_ptr;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_ptr == child_entry->cache_ptr);
#ifndef NDEBUG
    /* Make sure the parent is not already a parent */
    {
        unsigned i;

        for(i=0; i<child_entry->flush_dep_nparents; i++)
            HDassert(child_entry->flush_dep_parent[i] != parent_entry);
    } /* end block */
#endif /* NDEBUG */

    /* More sanity checks */
    if(child_entry == parent_entry)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTDEPEND, FAIL, "Child entry flush dependency parent can't be itself")
    if(!(parent_entry->is_protected || parent_entry->is_pinned))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTDEPEND, FAIL, "Parent entry isn't pinned or protected")

    /* Check for parent not pinned */
    if(!parent_entry->is_pinned) {
        /* Sanity check */
        HDassert(parent_entry->flush_dep_nchildren == 0);
        HDassert(!parent_entry->pinned_from_client);
        HDassert(!parent_entry->pinned_from_cache);

        /* Pin the parent entry */
        parent_entry->is_pinned = TRUE;
        H5C__UPDATE_STATS_FOR_PIN(cache_ptr, parent_entry)
    } /* end else */

    /* Mark the entry as pinned from the cache's action (possibly redundantly) */
    parent_entry->pinned_from_cache = TRUE;

    /* Check if we need to resize the child's parent array */
    if(child_entry->flush_dep_nparents >= child_entry->flush_dep_parent_nalloc) {
        if(child_entry->flush_dep_parent_nalloc == 0) {
            /* Array does not exist yet, allocate it */
            HDassert(!child_entry->flush_dep_parent);

            if(NULL == (child_entry->flush_dep_parent = (H5C_cache_entry_t **)H5FL_BLK_MALLOC(parent, H5C_FLUSH_DEP_PARENT_INIT * sizeof(H5C_cache_entry_t *))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for flush dependency parent list")
            child_entry->flush_dep_parent_nalloc = H5C_FLUSH_DEP_PARENT_INIT;
        } /* end if */
        else {
            /* Resize existing array */
            HDassert(child_entry->flush_dep_parent);

            if(NULL == (child_entry->flush_dep_parent = (H5C_cache_entry_t **)H5FL_BLK_REALLOC(parent, child_entry->flush_dep_parent, 2 * child_entry->flush_dep_parent_nalloc * sizeof(H5C_cache_entry_t *))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for flush dependency parent list")
            child_entry->flush_dep_parent_nalloc *= 2;
        } /* end else */
    } /* end if */

    /* Add the dependency to the child's parent array */
    child_entry->flush_dep_parent[child_entry->flush_dep_nparents] = parent_entry;
    child_entry->flush_dep_nparents++;

    /* Increment parent's number of children */
    parent_entry->flush_dep_nchildren++;

    /* Adjust the number of dirty children */
    if(child_entry->is_dirty || child_entry->flush_dep_ndirty_children > 0) {
        /* Sanity check */
        HDassert(parent_entry->flush_dep_ndirty_children < parent_entry->flush_dep_nchildren);

        parent_entry->flush_dep_ndirty_children++;

        /* Propagate the flush dep dirty flag up the chain if necessary */
        if(!parent_entry->is_dirty
                && parent_entry->flush_dep_ndirty_children == 1)
            if(H5C__mark_flush_dep_dirty(parent_entry) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "can't propagate flush dep dirty flag")
    } /* end if */

    /* Post-conditions, for successful operation */
    HDassert(parent_entry->is_pinned);
    HDassert(parent_entry->flush_dep_nchildren > 0);
    HDassert(child_entry->flush_dep_parent);
    HDassert(child_entry->flush_dep_nparents > 0);
    HDassert(child_entry->flush_dep_parent_nalloc > 0);
#ifndef NDEBUG
    H5C__assert_flush_dep_nocycle(parent_entry, child_entry);
#endif /* NDEBUG */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_create_flush_dependency() */


/*-------------------------------------------------------------------------
 * Function:    H5C_destroy_flush_dependency()
 *
 * Purpose:	Terminates a parent<-> child entry flush dependency.  The
 *              parent entry must be pinned.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              3/05/09
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_destroy_flush_dependency(void *parent_thing, void * child_thing)
{
    H5C_t             * cache_ptr;
    H5C_cache_entry_t *	parent_entry = (H5C_cache_entry_t *)parent_thing; /* Ptr to parent entry */
    H5C_cache_entry_t *	child_entry = (H5C_cache_entry_t *)child_thing; /* Ptr to child entry */
    unsigned            i;                      /* Local index variable */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(parent_entry);
    HDassert(parent_entry->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(H5F_addr_defined(parent_entry->addr));
    HDassert(child_entry);
    HDassert(child_entry->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(H5F_addr_defined(child_entry->addr));
    cache_ptr = parent_entry->cache_ptr;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_ptr == child_entry->cache_ptr);

    /* Usage checks */
    if(!parent_entry->is_pinned)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "Parent entry isn't pinned")
    if(NULL == child_entry->flush_dep_parent)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "Child entry doesn't have a flush dependency parent array")
    if(0 == parent_entry->flush_dep_nchildren)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "Parent entry flush dependency ref. count has no child dependencies")

    /* Search for parent in child's parent array.  This is a linear search
     * because we do not expect large numbers of parents.  If this changes, we
     * may wish to change the parent array to a skip list */
    for(i=0; i<child_entry->flush_dep_nparents; i++)
        if(child_entry->flush_dep_parent[i] == parent_entry)
            break;
    if(i == child_entry->flush_dep_nparents)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "Parent entry isn't a flush dependency parent for child entry")

    /* Remove parent entry from child's parent array */
    if(i < child_entry->flush_dep_nparents - 1)
        HDmemmove(&child_entry->flush_dep_parent[i],
                &child_entry->flush_dep_parent[i+1],
                (child_entry->flush_dep_nparents - i - 1)
                * sizeof(child_entry->flush_dep_parent[0]));
    child_entry->flush_dep_nparents--;

    /* Adjust parent entry's nchildren and unpin parent if it goes to zero */
    parent_entry->flush_dep_nchildren--;
    if(0 == parent_entry->flush_dep_nchildren) {
        /* Sanity check */
        HDassert(parent_entry->pinned_from_cache);

        /* Check if we should unpin parent entry now */
        if(!parent_entry->pinned_from_client) {
            /* Update the replacement policy if the entry is not protected */
            if(!parent_entry->is_protected)
                H5C__UPDATE_RP_FOR_UNPIN(cache_ptr, parent_entry, FAIL)

            /* Unpin the entry now */
            parent_entry->is_pinned = FALSE;

            /* Update the stats for an unpin operation */
            H5C__UPDATE_STATS_FOR_UNPIN(cache_ptr, parent_entry)
        } /* end if */

        /* Mark the entry as unpinned from the cache's action */
        parent_entry->pinned_from_cache = FALSE;
    } /* end if */

    /* Adjust parent entry's ndirty_children */
    if(child_entry->is_dirty || child_entry->flush_dep_ndirty_children > 0) {
        /* Sanity check */
        HDassert(parent_entry->flush_dep_ndirty_children > 0);

        parent_entry->flush_dep_ndirty_children--;

        /* Propagate the flush dep clean flag up the chain if necessary */
        if(!parent_entry->is_dirty
                && parent_entry->flush_dep_ndirty_children == 0)
            if(H5C__mark_flush_dep_clean(parent_entry) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "can't propagate flush dep clean flag")
    } /* end if */

    /* Shrink or free the parent array if apporpriate */
    if(child_entry->flush_dep_nparents == 0) {
        child_entry->flush_dep_parent = (H5C_cache_entry_t **)H5FL_BLK_FREE(parent, child_entry->flush_dep_parent);
        child_entry->flush_dep_parent_nalloc = 0;
    } /* end if */
    else if(child_entry->flush_dep_parent_nalloc > H5C_FLUSH_DEP_PARENT_INIT
            && child_entry->flush_dep_nparents
            <= (child_entry->flush_dep_parent_nalloc / 4)) {
        if(NULL == (child_entry->flush_dep_parent = (H5C_cache_entry_t **)H5FL_BLK_REALLOC(parent, child_entry->flush_dep_parent, (child_entry->flush_dep_parent_nalloc / 4) * sizeof(H5C_cache_entry_t *))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for flush dependency parent list")
        child_entry->flush_dep_parent_nalloc /= 4;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_destroy_flush_dependency() */


/*************************************************************************/
/**************************** Private Functions: *************************/
/*************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Function:	H5C__auto_adjust_cache_size
 *
 * Purpose:    	Obtain the current full cache hit rate, and compare it
 *		with the hit rate thresholds for modifying cache size.
 *		If one of the thresholds has been crossed, adjusts the
 *		size of the cache accordingly.
 *
 *		The function then resets the full cache hit rate
 *		statistics, and exits.
 *
 * Return:      Non-negative on success/Negative on failure or if there was
 *		an attempt to flush a protected item.
 *
 *
 * Programmer:  John Mainzer, 10/7/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__auto_adjust_cache_size(H5F_t * f,
                            hid_t dxpl_id,
                            hbool_t write_permitted)
{
    H5C_t *			cache_ptr = f->shared->cache;
    herr_t			result;
    hbool_t			inserted_epoch_marker = FALSE;
    size_t			new_max_cache_size = 0;
    size_t			old_max_cache_size = 0;
    size_t			new_min_clean_size = 0;
    size_t			old_min_clean_size = 0;
    double			hit_rate;
    enum H5C_resize_status	status = in_spec; /* will change if needed */
    herr_t			ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( f );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->cache_accesses >=
              (cache_ptr->resize_ctl).epoch_length );
    HDassert( (double)0.0f <= (cache_ptr->resize_ctl).min_clean_fraction );
    HDassert( (cache_ptr->resize_ctl).min_clean_fraction <= (double)100.0f );

    if ( !cache_ptr->resize_enabled ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Auto cache resize disabled.")
    }

    HDassert( ( (cache_ptr->resize_ctl).incr_mode != H5C_incr__off ) || \
              ( (cache_ptr->resize_ctl).decr_mode != H5C_decr__off ) );

    if ( H5C_get_cache_hit_rate(cache_ptr, &hit_rate) != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't get hit rate.")
    }

    HDassert( ( (double)0.0f <= hit_rate ) && ( hit_rate <= (double)1.0f ) );

    switch ( (cache_ptr->resize_ctl).incr_mode )
    {
        case H5C_incr__off:
            if ( cache_ptr->size_increase_possible ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                           "size_increase_possible but H5C_incr__off?!?!?")
            }
            break;

        case H5C_incr__threshold:
            if ( hit_rate < (cache_ptr->resize_ctl).lower_hr_threshold ) {

                if ( ! cache_ptr->size_increase_possible ) {

                    status = increase_disabled;

                } else if ( cache_ptr->max_cache_size >=
                            (cache_ptr->resize_ctl).max_size ) {

                    HDassert( cache_ptr->max_cache_size == \
                              (cache_ptr->resize_ctl).max_size );
                    status = at_max_size;

                } else if ( ! cache_ptr->cache_full ) {

                    status = not_full;

                } else {

                    new_max_cache_size = (size_t)
                                     (((double)(cache_ptr->max_cache_size)) *
                                      (cache_ptr->resize_ctl).increment);

                    /* clip to max size if necessary */
                    if ( new_max_cache_size >
                         (cache_ptr->resize_ctl).max_size ) {

                        new_max_cache_size = (cache_ptr->resize_ctl).max_size;
                    }

                    /* clip to max increment if necessary */
                    if ( ( (cache_ptr->resize_ctl).apply_max_increment ) &&
                         ( (cache_ptr->max_cache_size +
                            (cache_ptr->resize_ctl).max_increment) <
                           new_max_cache_size ) ) {

                        new_max_cache_size = cache_ptr->max_cache_size +
                                         (cache_ptr->resize_ctl).max_increment;
                    }

                    status = increase;
                }
            }
            break;

        default:
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unknown incr_mode.")
    }

    /* If the decr_mode is either age out or age out with threshold, we
     * must run the marker maintenance code, whether we run the size
     * reduction code or not.  We do this in two places -- here we
     * insert a new marker if the number of active epoch markers is
     * is less than the the current epochs before eviction, and after
     * the ageout call, we cycle the markers.
     *
     * However, we can't call the ageout code or cycle the markers
     * unless there was a full complement of markers in place on
     * entry.  The inserted_epoch_marker flag is used to track this.
     */

    if ( ( ( (cache_ptr->resize_ctl).decr_mode == H5C_decr__age_out )
           ||
           ( (cache_ptr->resize_ctl).decr_mode ==
              H5C_decr__age_out_with_threshold
           )
         )
         &&
         ( cache_ptr->epoch_markers_active <
           (cache_ptr->resize_ctl).epochs_before_eviction
         )
       ) {

        result = H5C__autoadjust__ageout__insert_new_marker(cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "can't insert new epoch marker.")

        } else {

            inserted_epoch_marker = TRUE;
        }
    }

    /* don't run the cache size decrease code unless the cache size
     * increase code is disabled, or the size increase code sees no need
     * for action.  In either case, status == in_spec at this point.
     */

    if ( status == in_spec ) {

        switch ( (cache_ptr->resize_ctl).decr_mode )
        {
            case H5C_decr__off:
                break;

            case H5C_decr__threshold:
                if ( hit_rate > (cache_ptr->resize_ctl).upper_hr_threshold ) {

                    if ( ! cache_ptr->size_decrease_possible ) {

                        status = decrease_disabled;

                    } else if ( cache_ptr->max_cache_size <=
                                (cache_ptr->resize_ctl).min_size ) {

                        HDassert( cache_ptr->max_cache_size ==
                                  (cache_ptr->resize_ctl).min_size );
                        status = at_min_size;

                    } else {

                        new_max_cache_size = (size_t)
                                 (((double)(cache_ptr->max_cache_size)) *
                                  (cache_ptr->resize_ctl).decrement);

                        /* clip to min size if necessary */
                        if ( new_max_cache_size <
                             (cache_ptr->resize_ctl).min_size ) {

                            new_max_cache_size =
                                (cache_ptr->resize_ctl).min_size;
                        }

                        /* clip to max decrement if necessary */
                        if ( ( (cache_ptr->resize_ctl).apply_max_decrement ) &&
                             ( ((cache_ptr->resize_ctl).max_decrement +
                                new_max_cache_size) <
                               cache_ptr->max_cache_size ) ) {

                            new_max_cache_size = cache_ptr->max_cache_size -
                                         (cache_ptr->resize_ctl).max_decrement;
                        }

                        status = decrease;
                    }
                }
                break;

            case H5C_decr__age_out_with_threshold:
            case H5C_decr__age_out:
                if ( ! inserted_epoch_marker ) {

                    if ( ! cache_ptr->size_decrease_possible ) {

                        status = decrease_disabled;

                    } else {

                        result = H5C__autoadjust__ageout(f,
                                                         dxpl_id,
                                                         hit_rate,
                                                         &status,
                                                         &new_max_cache_size,
                                                         write_permitted);

                        if ( result != SUCCEED ) {

                            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                                        "ageout code failed.")
                        }
                    }
                }
                break;

            default:
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unknown incr_mode.")
        }
    }

    /* cycle the epoch markers here if appropriate */
    if ( ( ( (cache_ptr->resize_ctl).decr_mode == H5C_decr__age_out )
           ||
           ( (cache_ptr->resize_ctl).decr_mode ==
              H5C_decr__age_out_with_threshold
           )
         )
         &&
         ( ! inserted_epoch_marker )
       ) {

        /* move last epoch marker to the head of the LRU list */
        result = H5C__autoadjust__ageout__cycle_epoch_marker(cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "error cycling epoch marker.")
        }
    }

    if ( ( status == increase ) || ( status == decrease ) ) {

        old_max_cache_size = cache_ptr->max_cache_size;
        old_min_clean_size = cache_ptr->min_clean_size;

        new_min_clean_size = (size_t)
                             ((double)new_max_cache_size *
                              ((cache_ptr->resize_ctl).min_clean_fraction));

        /* new_min_clean_size is of size_t, and thus must be non-negative.
         * Hence we have
         *
         * 	( 0 <= new_min_clean_size ).
         *
 	 * by definition.
         */
        HDassert( new_min_clean_size <= new_max_cache_size );
        HDassert( (cache_ptr->resize_ctl).min_size <= new_max_cache_size );
        HDassert( new_max_cache_size <= (cache_ptr->resize_ctl).max_size );

        cache_ptr->max_cache_size = new_max_cache_size;
        cache_ptr->min_clean_size = new_min_clean_size;

        if ( status == increase ) {

            cache_ptr->cache_full = FALSE;

        } else if ( status == decrease ) {

            cache_ptr->size_decreased = TRUE;
        }

	/* update flash cache size increase fields as appropriate */
	if ( cache_ptr->flash_size_increase_possible ) {

            switch ( (cache_ptr->resize_ctl).flash_incr_mode )
            {
                case H5C_flash_incr__off:

                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                     "flash_size_increase_possible but H5C_flash_incr__off?!")
                    break;

                case H5C_flash_incr__add_space:
                    cache_ptr->flash_size_increase_threshold =
                        (size_t)
                        (((double)(cache_ptr->max_cache_size)) *
                         ((cache_ptr->resize_ctl).flash_threshold));
                     break;

                default: /* should be unreachable */
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                                "Unknown flash_incr_mode?!?!?.")
                    break;
            }
        }
    }

    if ( (cache_ptr->resize_ctl).rpt_fcn != NULL ) {

        (*((cache_ptr->resize_ctl).rpt_fcn))
            (cache_ptr,
             H5C__CURR_AUTO_RESIZE_RPT_FCN_VER,
             hit_rate,
             status,
             old_max_cache_size,
             new_max_cache_size,
             old_min_clean_size,
             new_min_clean_size);
    }

    if ( H5C_reset_cache_hit_rate_stats(cache_ptr) != SUCCEED ) {

        /* this should be impossible... */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_reset_cache_hit_rate_stats failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__auto_adjust_cache_size() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout
 *
 * Purpose:     Implement the ageout automatic cache size decrement
 *		algorithm.  Note that while this code evicts aged out
 *		entries, the code does not change the maximum cache size.
 *		Instead, the function simply computes the new value (if
 *		any change is indicated) and reports this value in
 *		*new_max_cache_size_ptr.
 *
 * Return:      Non-negative on success/Negative on failure or if there was
 *              an attempt to flush a protected item.
 *
 *
 * Programmer:  John Mainzer, 11/18/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__autoadjust__ageout(H5F_t * f,
                        hid_t dxpl_id,
                        double hit_rate,
                        enum H5C_resize_status * status_ptr,
                        size_t * new_max_cache_size_ptr,
                        hbool_t write_permitted)
{
    H5C_t *     cache_ptr = f->shared->cache;
    herr_t	result;
    size_t	test_size;
    herr_t	ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( f );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( ( status_ptr ) && ( *status_ptr == in_spec ) );
    HDassert( ( new_max_cache_size_ptr ) && ( *new_max_cache_size_ptr == 0 ) );

    /* remove excess epoch markers if any */
    if ( cache_ptr->epoch_markers_active >
         (cache_ptr->resize_ctl).epochs_before_eviction ) {

        result = H5C__autoadjust__ageout__remove_excess_markers(cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "can't remove excess epoch markers.")
        }
    }

    if ( ( (cache_ptr->resize_ctl).decr_mode == H5C_decr__age_out )
         ||
         ( ( (cache_ptr->resize_ctl).decr_mode ==
              H5C_decr__age_out_with_threshold
               )
           &&
           ( hit_rate >= (cache_ptr->resize_ctl).upper_hr_threshold )
         )
       ) {

        if ( cache_ptr->max_cache_size > (cache_ptr->resize_ctl).min_size ){

            /* evict aged out cache entries if appropriate... */
            if(H5C__autoadjust__ageout__evict_aged_out_entries(f, dxpl_id, write_permitted) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "error flushing aged out entries.")

            /* ... and then reduce cache size if appropriate */
            if ( cache_ptr->index_size < cache_ptr->max_cache_size ) {

                if ( (cache_ptr->resize_ctl).apply_empty_reserve ) {

                    test_size = (size_t)(((double)cache_ptr->index_size) /
                                (1 - (cache_ptr->resize_ctl).empty_reserve));

                    if ( test_size < cache_ptr->max_cache_size ) {

                        *status_ptr = decrease;
                        *new_max_cache_size_ptr = test_size;
                    }
                } else {

                    *status_ptr = decrease;
                    *new_max_cache_size_ptr = cache_ptr->index_size;
                }

                if ( *status_ptr == decrease ) {

                    /* clip to min size if necessary */
                    if ( *new_max_cache_size_ptr <
                         (cache_ptr->resize_ctl).min_size ) {

                        *new_max_cache_size_ptr =
                                (cache_ptr->resize_ctl).min_size;
                    }

                    /* clip to max decrement if necessary */
                    if ( ( (cache_ptr->resize_ctl).apply_max_decrement ) &&
                         ( ((cache_ptr->resize_ctl).max_decrement +
                            *new_max_cache_size_ptr) <
                           cache_ptr->max_cache_size ) ) {

                        *new_max_cache_size_ptr = cache_ptr->max_cache_size -
                                         (cache_ptr->resize_ctl).max_decrement;
                    }
                }
            }
        } else {

            *status_ptr = at_min_size;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__cycle_epoch_marker
 *
 * Purpose:     Remove the oldest epoch marker from the LRU list,
 *		and reinsert it at the head of the LRU list.  Also
 *		remove the epoch marker's index from the head of the
 *		ring buffer, and re-insert it at the tail of the ring
 *		buffer.
 *
 * Return:      SUCCEED on success/FAIL on failure.
 *
 * Programmer:  John Mainzer, 11/22/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__autoadjust__ageout__cycle_epoch_marker(H5C_t * cache_ptr)
{
    herr_t                      ret_value = SUCCEED;      /* Return value */
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( cache_ptr->epoch_markers_active <= 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "No active epoch markers on entry?!?!?.")
    }

    /* remove the last marker from both the ring buffer and the LRU list */

    i = cache_ptr->epoch_marker_ringbuf[cache_ptr->epoch_marker_ringbuf_first];

    cache_ptr->epoch_marker_ringbuf_first =
            (cache_ptr->epoch_marker_ringbuf_first + 1) %
            (H5C__MAX_EPOCH_MARKERS + 1);

    cache_ptr->epoch_marker_ringbuf_size -= 1;

    if ( cache_ptr->epoch_marker_ringbuf_size < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer underflow.")
    }

    if ( (cache_ptr->epoch_marker_active)[i] != TRUE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unused marker in LRU?!?")
    }

    H5C__DLL_REMOVE((&((cache_ptr->epoch_markers)[i])), \
                    (cache_ptr)->LRU_head_ptr, \
                    (cache_ptr)->LRU_tail_ptr, \
                    (cache_ptr)->LRU_list_len, \
                    (cache_ptr)->LRU_list_size, \
                    (FAIL))

    /* now, re-insert it at the head of the LRU list, and at the tail of
     * the ring buffer.
     */

    HDassert( ((cache_ptr->epoch_markers)[i]).addr == (haddr_t)i );
    HDassert( ((cache_ptr->epoch_markers)[i]).next == NULL );
    HDassert( ((cache_ptr->epoch_markers)[i]).prev == NULL );

    cache_ptr->epoch_marker_ringbuf_last =
        (cache_ptr->epoch_marker_ringbuf_last + 1) %
        (H5C__MAX_EPOCH_MARKERS + 1);

    (cache_ptr->epoch_marker_ringbuf)[cache_ptr->epoch_marker_ringbuf_last] = i;

    cache_ptr->epoch_marker_ringbuf_size += 1;

    if ( cache_ptr->epoch_marker_ringbuf_size > H5C__MAX_EPOCH_MARKERS ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer overflow.")
    }

    H5C__DLL_PREPEND((&((cache_ptr->epoch_markers)[i])), \
                     (cache_ptr)->LRU_head_ptr, \
                     (cache_ptr)->LRU_tail_ptr, \
                     (cache_ptr)->LRU_list_len, \
                     (cache_ptr)->LRU_list_size, \
                     (FAIL))
done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__cycle_epoch_marker() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__evict_aged_out_entries
 *
 * Purpose:     Evict clean entries in the cache that haven't
 *		been accessed for at least
 *              (cache_ptr->resize_ctl).epochs_before_eviction epochs,
 *      	and flush dirty entries that haven't been accessed for
 *		that amount of time.
 *
 *		Depending on configuration, the function will either
 *		flush or evict all such entries, or all such entries it
 *		encounters until it has freed the maximum amount of space
 *		allowed under the maximum decrement.
 *
 *		If we are running in parallel mode, writes may not be
 *		permitted.  If so, the function simply skips any dirty
 *		entries it may encounter.
 *
 *		The function makes no attempt to maintain the minimum
 *		clean size, as there is no guarantee that the cache size
 *		will be changed.
 *
 *		If there is no cache size change, the minimum clean size
 *		constraint will be met through a combination of clean
 *		entries and free space in the cache.
 *
 *		If there is a cache size reduction, the minimum clean size
 *		will be re-calculated, and will be enforced the next time
 *		we have to make space in the cache.
 *
 *              The primary_dxpl_id and secondary_dxpl_id parameters
 *              specify the dxpl_ids used depending on the value of
 *		*first_flush_ptr.  The idea is to use the primary_dxpl_id
 *		on the first write in a sequence of writes, and to use
 *		the secondary_dxpl_id on all subsequent writes.
 *
 *              This is useful in the metadata cache, but may not be
 *		needed elsewhere.  If so, just use the same dxpl_id for
 *		both parameters.
 *
 *              Observe that this function cannot occasion a read.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 11/22/04
 *
 * Changes:	Modified function to detect deletions of entries
 *              during a scan of the LRU, and where appropriate,
 *              restart the scan to avoid proceeding with a next
 *              entry that is no longer in the cache.
 *
 *              Note the absence of checks after flushes of clean
 *              entries.  As a second entry can only be removed by
 *              by a call to the pre_serialize or serialize callback
 *              of the first, and as these callbacks will not be called
 *              on clean entries, no checks are needed.
 *
 *                                              JRM -- 4/6/15
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__autoadjust__ageout__evict_aged_out_entries(H5F_t * f,
                                                hid_t   dxpl_id,
                                                hbool_t write_permitted)
{
    H5C_t *		cache_ptr = f->shared->cache;
    size_t		eviction_size_limit;
    size_t		bytes_evicted = 0;
    hbool_t		prev_is_dirty = FALSE;
    hbool_t             restart_scan;
    H5C_cache_entry_t * entry_ptr;
    H5C_cache_entry_t * next_ptr;
    H5C_cache_entry_t * prev_ptr;
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( f );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    /* if there is a limit on the amount that the cache size can be decrease
     * in any one round of the cache size reduction algorithm, load that
     * limit into eviction_size_limit.  Otherwise, set eviction_size_limit
     * to the equivalent of infinity.  The current size of the index will
     * do nicely.
     */
    if ( (cache_ptr->resize_ctl).apply_max_decrement ) {

        eviction_size_limit = (cache_ptr->resize_ctl).max_decrement;

    } else {

        eviction_size_limit = cache_ptr->index_size; /* i.e. infinity */
    }

    if ( write_permitted ) {

        restart_scan = FALSE;
        entry_ptr = cache_ptr->LRU_tail_ptr;

        while ( ( entry_ptr != NULL ) &&
                ( (entry_ptr->type)->id != H5C__EPOCH_MARKER_TYPE ) &&
                ( bytes_evicted < eviction_size_limit ) )
        {
	    hbool_t		corked = FALSE;

            HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
            HDassert( ! (entry_ptr->is_protected) );
            HDassert( ! (entry_ptr->is_read_only) );
            HDassert( (entry_ptr->ro_ref_count) == 0 );

	    next_ptr = entry_ptr->next;
            prev_ptr = entry_ptr->prev;

	    if ( prev_ptr != NULL ) {

                prev_is_dirty = prev_ptr->is_dirty;
            }

	    /* dirty corked entry is skipped */
	    if(entry_ptr->is_corked && entry_ptr->is_dirty)
		corked = TRUE;
            else if ( entry_ptr->is_dirty ) {

                /* reset entries_removed_counter and
                 * last_entry_removed_ptr prior to the call to
                 * H5C__flush_single_entry() so that we can spot
                 * unexpected removals of entries from the cache,
                 * and set the restart_scan flag if proceeding
                 * would be likely to cause us to scan an entry
                 * that is no longer in the cache.
                 */
                cache_ptr->entries_removed_counter = 0;
                cache_ptr->last_entry_removed_ptr  = NULL;

                if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, H5C__NO_FLAGS_SET, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush entry")

                if ( ( cache_ptr->entries_removed_counter > 1 ) ||
                     ( cache_ptr->last_entry_removed_ptr == prev_ptr ) )

                    restart_scan = TRUE;

            } else {

                bytes_evicted += entry_ptr->size;

                if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, H5C__FLUSH_INVALIDATE_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG, NULL, NULL) < 0 )
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush entry")
            }

            if ( prev_ptr != NULL ) {

		if(corked)   /* dirty corked entry is skipped */
                    entry_ptr = prev_ptr;

		else if ( ( restart_scan )
                     ||
                     ( prev_ptr->is_dirty != prev_is_dirty )
                     ||
                     ( prev_ptr->next != next_ptr )
                     ||
                     ( prev_ptr->is_protected )
                     ||
                     ( prev_ptr->is_pinned ) ) {

                    /* something has happened to the LRU -- start over
		     * from the tail.
                     */
                    restart_scan = FALSE;
                    entry_ptr = cache_ptr->LRU_tail_ptr;

		    H5C__UPDATE_STATS_FOR_LRU_SCAN_RESTART(cache_ptr)

                } else {

                    entry_ptr = prev_ptr;

                }
	    } else {

		entry_ptr = NULL;

	    }
        } /* end while */

        /* for now at least, don't bother to maintain the minimum clean size,
         * as the cache should now be less than its maximum size.  Due to
         * the vaguries of the cache size reduction algorthim, we may not
         * reduce the size of the cache.
         *
         * If we do, we will calculate a new minimum clean size, which will
         * be enforced the next time we try to make space in the cache.
         *
         * If we don't, no action is necessary, as we have just evicted and/or
         * or flushed a bunch of entries and therefore the sum of the clean
         * and free space in the cache must be greater than or equal to the
         * min clean space requirement (assuming that requirement was met on
         * entry).
         */

    } else /* ! write_permitted */  {

        /* since we are not allowed to write, all we can do is evict
         * any clean entries that we may encounter before we either
         * hit the eviction size limit, or encounter the epoch marker.
         *
         * If we are operating read only, this isn't an issue, as there
         * will not be any dirty entries.
         *
         * If we are operating in R/W mode, all the dirty entries we
         * skip will be flushed the next time we attempt to make space
         * when writes are permitted.  This may have some local
         * performance implications, but it shouldn't cause any net
         * slowdown.
         */

        HDassert( H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS );

        entry_ptr = cache_ptr->LRU_tail_ptr;

        while ( ( entry_ptr != NULL ) &&
                ( (entry_ptr->type)->id != H5C__EPOCH_MARKER_TYPE ) &&
                ( bytes_evicted < eviction_size_limit ) )
        {
            HDassert( ! (entry_ptr->is_protected) );

            prev_ptr = entry_ptr->prev;

            if ( ! (entry_ptr->is_dirty) ) {
                if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, H5C__FLUSH_INVALIDATE_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush clean entry")
            }
            /* just skip the entry if it is dirty, as we can't do
             * anything with it now since we can't write.
	     *
	     * Since all entries are clean, serialize() will not be called,
	     * and thus we needn't test to see if the LRU has been changed
	     * out from under us.
             */

            entry_ptr = prev_ptr;

        } /* end while */
    }

    if ( cache_ptr->index_size < cache_ptr->max_cache_size ) {

        cache_ptr->cache_full = FALSE;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__evict_aged_out_entries() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__insert_new_marker
 *
 * Purpose:     Find an unused marker cache entry, mark it as used, and
 *		insert it at the head of the LRU list.  Also add the
 *		marker's index in the epoch_markers array.
 *
 * Return:      SUCCEED on success/FAIL on failure.
 *
 * Programmer:  John Mainzer, 11/19/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__autoadjust__ageout__insert_new_marker(H5C_t * cache_ptr)
{
    herr_t                      ret_value = SUCCEED;      /* Return value */
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( cache_ptr->epoch_markers_active >=
         (cache_ptr->resize_ctl).epochs_before_eviction ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Already have a full complement of markers.")
    }

    /* find an unused marker */
    i = 0;
    while ( ( (cache_ptr->epoch_marker_active)[i] ) &&
            ( i < H5C__MAX_EPOCH_MARKERS ) )
    {
        i++;
    }

    if(i >= H5C__MAX_EPOCH_MARKERS)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't find unused marker.")

    HDassert( ((cache_ptr->epoch_markers)[i]).addr == (haddr_t)i );
    HDassert( ((cache_ptr->epoch_markers)[i]).next == NULL );
    HDassert( ((cache_ptr->epoch_markers)[i]).prev == NULL );

    (cache_ptr->epoch_marker_active)[i] = TRUE;

    cache_ptr->epoch_marker_ringbuf_last =
        (cache_ptr->epoch_marker_ringbuf_last + 1) %
        (H5C__MAX_EPOCH_MARKERS + 1);

    (cache_ptr->epoch_marker_ringbuf)[cache_ptr->epoch_marker_ringbuf_last] = i;

    cache_ptr->epoch_marker_ringbuf_size += 1;

    if ( cache_ptr->epoch_marker_ringbuf_size > H5C__MAX_EPOCH_MARKERS ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer overflow.")
    }

    H5C__DLL_PREPEND((&((cache_ptr->epoch_markers)[i])), \
                     (cache_ptr)->LRU_head_ptr, \
                     (cache_ptr)->LRU_tail_ptr, \
                     (cache_ptr)->LRU_list_len, \
                     (cache_ptr)->LRU_list_size, \
                     (FAIL))

    cache_ptr->epoch_markers_active += 1;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__insert_new_marker() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__remove_all_markers
 *
 * Purpose:     Remove all epoch markers from the LRU list and mark them
 *		as inactive.
 *
 * Return:      SUCCEED on success/FAIL on failure.
 *
 * Programmer:  John Mainzer, 11/22/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__autoadjust__ageout__remove_all_markers(H5C_t * cache_ptr)
{
    herr_t                      ret_value = SUCCEED;      /* Return value */
    int i;
    int ring_buf_index;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    while ( cache_ptr->epoch_markers_active > 0 )
    {
        /* get the index of the last epoch marker in the LRU list
         * and remove it from the ring buffer.
         */

        ring_buf_index = cache_ptr->epoch_marker_ringbuf_first;
        i = (cache_ptr->epoch_marker_ringbuf)[ring_buf_index];

        cache_ptr->epoch_marker_ringbuf_first =
            (cache_ptr->epoch_marker_ringbuf_first + 1) %
            (H5C__MAX_EPOCH_MARKERS + 1);

        cache_ptr->epoch_marker_ringbuf_size -= 1;

        if ( cache_ptr->epoch_marker_ringbuf_size < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer underflow.")
        }

        if ( (cache_ptr->epoch_marker_active)[i] != TRUE ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unused marker in LRU?!?")
        }

        /* remove the epoch marker from the LRU list */
        H5C__DLL_REMOVE((&((cache_ptr->epoch_markers)[i])), \
                        (cache_ptr)->LRU_head_ptr, \
                        (cache_ptr)->LRU_tail_ptr, \
                        (cache_ptr)->LRU_list_len, \
                        (cache_ptr)->LRU_list_size, \
                        (FAIL))

        /* mark the epoch marker as unused. */
        (cache_ptr->epoch_marker_active)[i] = FALSE;

        HDassert( ((cache_ptr->epoch_markers)[i]).addr == (haddr_t)i );
        HDassert( ((cache_ptr->epoch_markers)[i]).next == NULL );
        HDassert( ((cache_ptr->epoch_markers)[i]).prev == NULL );

        /* decrement the number of active epoch markers */
        cache_ptr->epoch_markers_active -= 1;

        HDassert( cache_ptr->epoch_markers_active == \
                  cache_ptr->epoch_marker_ringbuf_size );
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__remove_all_markers() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__autoadjust__ageout__remove_excess_markers
 *
 * Purpose:     Remove epoch markers from the end of the LRU list and
 *		mark them as inactive until the number of active markers
 *		equals the the current value of
 *		(cache_ptr->resize_ctl).epochs_before_eviction.
 *
 * Return:      SUCCEED on success/FAIL on failure.
 *
 * Programmer:  John Mainzer, 11/19/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__autoadjust__ageout__remove_excess_markers(H5C_t * cache_ptr)
{
    herr_t	ret_value = SUCCEED;      /* Return value */
    int		i;
    int		ring_buf_index;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( cache_ptr->epoch_markers_active <=
         (cache_ptr->resize_ctl).epochs_before_eviction ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "no excess markers on entry.")
    }

    while ( cache_ptr->epoch_markers_active >
            (cache_ptr->resize_ctl).epochs_before_eviction )
    {
        /* get the index of the last epoch marker in the LRU list
         * and remove it from the ring buffer.
         */

        ring_buf_index = cache_ptr->epoch_marker_ringbuf_first;
        i = (cache_ptr->epoch_marker_ringbuf)[ring_buf_index];

        cache_ptr->epoch_marker_ringbuf_first =
            (cache_ptr->epoch_marker_ringbuf_first + 1) %
            (H5C__MAX_EPOCH_MARKERS + 1);

        cache_ptr->epoch_marker_ringbuf_size -= 1;

        if ( cache_ptr->epoch_marker_ringbuf_size < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "ring buffer underflow.")
        }

        if ( (cache_ptr->epoch_marker_active)[i] != TRUE ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unused marker in LRU?!?")
        }

        /* remove the epoch marker from the LRU list */
        H5C__DLL_REMOVE((&((cache_ptr->epoch_markers)[i])), \
                        (cache_ptr)->LRU_head_ptr, \
                        (cache_ptr)->LRU_tail_ptr, \
                        (cache_ptr)->LRU_list_len, \
                        (cache_ptr)->LRU_list_size, \
                        (FAIL))

        /* mark the epoch marker as unused. */
        (cache_ptr->epoch_marker_active)[i] = FALSE;

        HDassert( ((cache_ptr->epoch_markers)[i]).addr == (haddr_t)i );
        HDassert( ((cache_ptr->epoch_markers)[i]).next == NULL );
        HDassert( ((cache_ptr->epoch_markers)[i]).prev == NULL );

        /* decrement the number of active epoch markers */
        cache_ptr->epoch_markers_active -= 1;

        HDassert( cache_ptr->epoch_markers_active == \
                  cache_ptr->epoch_marker_ringbuf_size );
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__autoadjust__ageout__remove_excess_markers() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__flash_increase_cache_size
 *
 * Purpose:     If there is not at least new_entry_size - old_entry_size
 *              bytes of free space in the cache and the current
 *              max_cache_size is less than (cache_ptr->resize_ctl).max_size,
 *              perform a flash increase in the cache size and then reset
 *              the full cache hit rate statistics, and exit.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 12/31/07
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__flash_increase_cache_size(H5C_t * cache_ptr,
                               size_t old_entry_size,
                               size_t new_entry_size)
{
    size_t                     new_max_cache_size = 0;
    size_t                     old_max_cache_size = 0;
    size_t                     new_min_clean_size = 0;
    size_t                     old_min_clean_size = 0;
    size_t                     space_needed;
    enum H5C_resize_status     status = flash_increase;  /* may change */
    double                     hit_rate;
    herr_t                     ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->flash_size_increase_possible );
    HDassert( new_entry_size > cache_ptr->flash_size_increase_threshold );
    HDassert( old_entry_size < new_entry_size );

    if ( old_entry_size >= new_entry_size ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "old_entry_size >= new_entry_size")
    }

    space_needed = new_entry_size - old_entry_size;

    if ( ( (cache_ptr->index_size + space_needed) >
                            cache_ptr->max_cache_size ) &&
         ( cache_ptr->max_cache_size < (cache_ptr->resize_ctl).max_size ) ) {

        /* we have work to do */

        switch ( (cache_ptr->resize_ctl).flash_incr_mode )
        {
            case H5C_flash_incr__off:
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                   "flash_size_increase_possible but H5C_flash_incr__off?!")
                break;

            case H5C_flash_incr__add_space:
                if ( cache_ptr->index_size < cache_ptr->max_cache_size ) {

                    HDassert( (cache_ptr->max_cache_size - cache_ptr->index_size)
                               < space_needed );
                    space_needed -= cache_ptr->max_cache_size -
			            cache_ptr->index_size;
                }
                space_needed =
                    (size_t)(((double)space_needed) *
                             (cache_ptr->resize_ctl).flash_multiple);

                new_max_cache_size = cache_ptr->max_cache_size + space_needed;

                break;

            default: /* should be unreachable */
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "Unknown flash_incr_mode?!?!?.")
                break;
        }

        if ( new_max_cache_size > (cache_ptr->resize_ctl).max_size ) {

            new_max_cache_size = (cache_ptr->resize_ctl).max_size;
        }

        HDassert( new_max_cache_size > cache_ptr->max_cache_size );

        new_min_clean_size = (size_t)
                             ((double)new_max_cache_size *
                              ((cache_ptr->resize_ctl).min_clean_fraction));

        HDassert( new_min_clean_size <= new_max_cache_size );

        old_max_cache_size = cache_ptr->max_cache_size;
        old_min_clean_size = cache_ptr->min_clean_size;

        cache_ptr->max_cache_size = new_max_cache_size;
        cache_ptr->min_clean_size = new_min_clean_size;

        /* update flash cache size increase fields as appropriate */
        HDassert ( cache_ptr->flash_size_increase_possible );

        switch ( (cache_ptr->resize_ctl).flash_incr_mode )
        {
            case H5C_flash_incr__off:
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "flash_size_increase_possible but H5C_flash_incr__off?!")
                break;

            case H5C_flash_incr__add_space:
                cache_ptr->flash_size_increase_threshold =
                    (size_t)
                    (((double)(cache_ptr->max_cache_size)) *
                     ((cache_ptr->resize_ctl).flash_threshold));
                break;

            default: /* should be unreachable */
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "Unknown flash_incr_mode?!?!?.")
                break;
        }

        /* note that we don't cycle the epoch markers.  We can
	 * argue either way as to whether we should, but for now
	 * we don't.
	 */

        if ( (cache_ptr->resize_ctl).rpt_fcn != NULL ) {

            /* get the hit rate for the reporting function.  Should still
             * be good as we havent reset the hit rate statistics.
             */
            if ( H5C_get_cache_hit_rate(cache_ptr, &hit_rate) != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't get hit rate.")
            }

            (*((cache_ptr->resize_ctl).rpt_fcn))
                (cache_ptr,
                 H5C__CURR_AUTO_RESIZE_RPT_FCN_VER,
                 hit_rate,
                 status,
                 old_max_cache_size,
                 new_max_cache_size,
                 old_min_clean_size,
                 new_min_clean_size);
        }

        if ( H5C_reset_cache_hit_rate_stats(cache_ptr) != SUCCEED ) {

            /* this should be impossible... */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_reset_cache_hit_rate_stats failed.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C__flash_increase_cache_size() */


/*-------------------------------------------------------------------------
 * Function:    H5C_flush_invalidate_cache
 *
 * Purpose:	Flush and destroy the entries contained in the target
 *		cache.
 *
 *		If the cache contains protected entries, the function will
 *		fail, as protected entries cannot be either flushed or
 *		destroyed.  However all unprotected entries should be
 *		flushed and destroyed before the function returns failure.
 *
 *		While pinned entries can usually be flushed, they cannot
 *		be destroyed.  However, they should be unpinned when all
 *		the entries that reference them have been destroyed (thus
 *		reduding the pinned entry's reference count to 0, allowing
 *		it to be unpinned).
 *
 *		If pinned entries are present, the function makes repeated
 *		passes through the cache, flushing all dirty entries
 *		(including the pinned dirty entries where permitted) and
 *		destroying all unpinned entries.  This process is repeated
 *		until either the cache is empty, or the number of pinned
 *		entries stops decreasing on each pass.
 *
 *		The primary_dxpl_id and secondary_dxpl_id parameters
 *		specify the dxpl_ids used on the first write occasioned
 *		by the flush (primary_dxpl_id), and on all subsequent
 *		writes (secondary_dxpl_id).
 *
 * Return:      Non-negative on success/Negative on failure or if there was
 *		a request to flush all items and something was protected.
 *
 * Programmer:  John Mainzer
 *		3/24/065
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_flush_invalidate_cache(const H5F_t * f, hid_t dxpl_id, unsigned flags)
{
    H5C_t *		cache_ptr;
    H5C_ring_t		ring;
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(f->shared);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_ptr->slist_ptr);

#if H5C_DO_SANITY_CHECKS
{
    int32_t		i;
    int32_t		index_len = 0;
    int32_t		slist_len = 0;
    size_t		index_size = (size_t)0;
    size_t		clean_index_size = (size_t)0;
    size_t		dirty_index_size = (size_t)0;
    size_t		slist_size = (size_t)0;

    HDassert(cache_ptr->index_ring_len[H5C_RING_UNDEFINED] == 0);
    HDassert(cache_ptr->index_ring_size[H5C_RING_UNDEFINED] == (size_t)0);
    HDassert(cache_ptr->clean_index_ring_size[H5C_RING_UNDEFINED] == (size_t)0);
    HDassert(cache_ptr->dirty_index_ring_size[H5C_RING_UNDEFINED] == (size_t)0);
    HDassert(cache_ptr->slist_ring_len[H5C_RING_UNDEFINED] == 0);
    HDassert(cache_ptr->slist_ring_size[H5C_RING_UNDEFINED] == (size_t)0);

    for(i = H5C_RING_USER; i < H5C_RING_NTYPES; i++) {
        index_len += cache_ptr->index_ring_len[i];
        index_size += cache_ptr->index_ring_size[i];
        clean_index_size += cache_ptr->clean_index_ring_size[i];
        dirty_index_size += cache_ptr->dirty_index_ring_size[i];

        slist_len += cache_ptr->slist_ring_len[i];
        slist_size += cache_ptr->slist_ring_size[i];
    } /* end for */

    HDassert(cache_ptr->index_len == index_len);
    HDassert(cache_ptr->index_size == index_size);
    HDassert(cache_ptr->clean_index_size == clean_index_size);
    HDassert(cache_ptr->dirty_index_size == dirty_index_size);
    HDassert(cache_ptr->slist_len == slist_len);
    HDassert(cache_ptr->slist_size == slist_size);
}
#endif /* H5C_DO_SANITY_CHECKS */

    /* remove ageout markers if present */
    if(cache_ptr->epoch_markers_active > 0)
        if(H5C__autoadjust__ageout__remove_all_markers(cache_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "error removing all epoch markers.")

    /* flush invalidate each ring, starting from the outermost ring and
     * working inward.
     */
    ring = H5C_RING_USER;
    while(ring < H5C_RING_NTYPES) {
        if(H5C_flush_invalidate_ring(f, dxpl_id, ring, flags) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "flush invalidate ring failed.")
        ring++;
    } /* end while */

    /* Invariants, after destroying all entries in the hash table */
    if(!(flags & H5C__EVICT_ALLOW_LAST_PINS_FLAG)) {
        HDassert(cache_ptr->index_size == 0);
        HDassert(cache_ptr->clean_index_size == 0);
        HDassert(cache_ptr->dirty_index_size == 0);
        HDassert(cache_ptr->slist_len == 0);
        HDassert(cache_ptr->slist_size == 0);
        HDassert(cache_ptr->pel_len == 0);
        HDassert(cache_ptr->pel_size == 0);
        HDassert(cache_ptr->pl_len == 0);
        HDassert(cache_ptr->pl_size == 0);
        HDassert(cache_ptr->LRU_list_len == 0);
        HDassert(cache_ptr->LRU_list_size == 0);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_invalidate_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5C_flush_invalidate_ring
 *
 * Purpose:	Flush and destroy the entries contained in the target
 *		cache and ring.
 *
 *		If the ring contains protected entries, the function will
 *		fail, as protected entries cannot be either flushed or
 *		destroyed.  However all unprotected entries should be
 *		flushed and destroyed before the function returns failure.
 *
 *		While pinned entries can usually be flushed, they cannot
 *		be destroyed.  However, they should be unpinned when all
 *		the entries that reference them have been destroyed (thus
 *		reduding the pinned entry's reference count to 0, allowing
 *		it to be unpinned).
 *
 *		If pinned entries are present, the function makes repeated
 *		passes through the cache, flushing all dirty entries
 *		(including the pinned dirty entries where permitted) and
 *		destroying all unpinned entries.  This process is repeated
 *		until either the cache is empty, or the number of pinned
 *		entries stops decreasing on each pass.
 *
 *		If flush dependencies appear in the target ring, the 
 *		function makes repeated passes through the cache flushing
 *		entries in flush dependency order.
 *
 * Return:      Non-negative on success/Negative on failure or if there was
 *		a request to flush all items and something was protected.
 *
 * Programmer:  John Mainzer
 *		9/1/15
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_flush_invalidate_ring(const H5F_t * f, hid_t dxpl_id, H5C_ring_t ring,
    unsigned flags)
{
    H5C_t *		cache_ptr;
    hbool_t             restart_slist_scan;
    int32_t		protected_entries = 0;
    int32_t		i;
    int32_t		cur_ring_pel_len;
    int32_t		old_ring_pel_len;
    unsigned		cooked_flags;
    unsigned		evict_flags;
    H5SL_node_t * 	node_ptr = NULL;
    H5C_cache_entry_t *	entry_ptr = NULL;
    H5C_cache_entry_t *	next_entry_ptr = NULL;
#if H5C_DO_SANITY_CHECKS
    int64_t		flushed_slist_len = 0;
    int64_t		initial_slist_len = 0;
    int64_t             flushed_slist_size = 0;
    size_t              initial_slist_size = 0;
    int64_t		entry_size_change;
    int64_t	      * entry_size_change_ptr = &entry_size_change;
#else /* H5C_DO_SANITY_CHECKS */
    int64_t           * entry_size_change_ptr = NULL;
#endif /* H5C_DO_SANITY_CHECKS */
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(f->shared);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_ptr->slist_ptr);
    HDassert(ring > H5C_RING_UNDEFINED);
    HDassert(ring < H5C_RING_NTYPES);

    HDassert(cache_ptr->epoch_markers_active == 0);

    /* Filter out the flags that are not relevant to the flush/invalidate.
     */
    cooked_flags = flags & H5C__FLUSH_CLEAR_ONLY_FLAG;
    evict_flags = flags & H5C__EVICT_ALLOW_LAST_PINS_FLAG;

    /* The flush proceedure here is a bit strange.
     *
     * In the outer while loop we make at least one pass through the
     * cache, and then repeat until either all the pinned entries in 
     * the ring unpin themselves, or until the number of pinned entries 
     * in the ring stops declining.  In this later case, we scream and die.
     *
     * Since the fractal heap can dirty, resize, and/or move entries
     * in is flush callback, it is possible that the cache will still
     * contain dirty entries at this point.  If so, we must make more
     * passes through the skip list to allow it to empty.
     *
     * Further, since clean entries can be dirtied, resized, and/or moved
     * as the result of a flush call back (either the entries own, or that
     * for some other cache entry), we can no longer promise to flush
     * the cache entries in increasing address order.
     *
     * Instead, we just do the best we can -- making a pass through
     * the skip list, and then a pass through the "clean" entries, and
     * then repeating as needed.  Thus it is quite possible that an
     * entry will be evicted from the cache only to be re-loaded later
     * in the flush process (From what Quincey tells me, the pin
     * mechanism makes this impossible, but even it it is true now,
     * we shouldn't count on it in the future.)
     *
     * The bottom line is that entries will probably be flushed in close
     * to increasing address order, but there are no guarantees.
     */

    /* compute the number of pinned entries in this ring */
    entry_ptr = cache_ptr->pel_head_ptr;
    cur_ring_pel_len = 0;
    while(entry_ptr != NULL) {
        HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
        HDassert(entry_ptr->ring >= ring);
        if(entry_ptr->ring == ring)
            cur_ring_pel_len++;

        entry_ptr = entry_ptr->next;
    } /* end while */

    old_ring_pel_len = cur_ring_pel_len;
    while(cache_ptr->index_ring_len[ring] > 0) {
        /* first, try to flush-destroy any dirty entries.   Do this by
         * making a scan through the slist.  Note that new dirty entries
         * may be created by the flush call backs.  Thus it is possible
         * that the slist will not be empty after we finish the scan.
         */

#if H5C_DO_SANITY_CHECKS
        /* Depending on circumstances, H5C__flush_single_entry() will
         * remove dirty entries from the slist as it flushes them.
         * Thus for sanity checks we must make note of the initial
         * slist length and size before we do any flushes.
         */
        initial_slist_len = cache_ptr->slist_len;
        initial_slist_size = cache_ptr->slist_size;

        /* There is also the possibility that entries will be
         * dirtied, resized, moved, and/or removed from the cache
         * as the result of calls to the flush callbacks.  We use 
         * the slist_len_increase and slist_size_increase increase 
         * fields in struct H5C_t to track these changes for purpose 
         * of sanity checking.
         *
         * To this end, we must zero these fields before we start
         * the pass through the slist.
         */
        cache_ptr->slist_len_increase = 0;
        cache_ptr->slist_size_increase = 0;

        /* Finally, reset the flushed_slist_len and flushed_slist_size
         * fields to zero, as these fields are used to accumulate
         * the slist lenght and size that we see as we scan through
         * the slist.
         */
        flushed_slist_len = 0;
        flushed_slist_size = 0;
#endif /* H5C_DO_SANITY_CHECKS */

        /* set the cache_ptr->slist_change_in_pre_serialize and
         * cache_ptr->slist_change_in_serialize to false.
         *
         * These flags are set to TRUE by H5C__flush_single_entry if the
         * slist is modified by a pre_serialize or serialize call 
         * respectively.
         *
         * H5C_flush_invalidate_cache() uses these flags to detect any 
         * modifications to the slist that might corrupt the scan of 
         * the slist -- and restart the scan in this event.
         */
        cache_ptr->slist_change_in_pre_serialize = FALSE;
        cache_ptr->slist_change_in_serialize = FALSE;

        /* this done, start the scan of the slist */
        restart_slist_scan = TRUE;
        while(restart_slist_scan || (node_ptr != NULL)) {
            if(restart_slist_scan) {
                restart_slist_scan = FALSE;

                /* Start at beginning of skip list */
                node_ptr = H5SL_first(cache_ptr->slist_ptr);
                if(node_ptr == NULL)
                    /* the slist is empty -- break out of inner loop */
                    break;

                /* Get cache entry for this node */
                next_entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);
                if(NULL == next_entry_ptr)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "next_entry_ptr == NULL ?!?!")

                HDassert(next_entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
                HDassert(next_entry_ptr->is_dirty);
                HDassert(next_entry_ptr->in_slist);
                HDassert(next_entry_ptr->ring >= ring);
            } /* end if */

            entry_ptr = next_entry_ptr;

            /* It is possible that entries will be dirtied, resized, 
             * flushed, or removed from the cache via the take ownership
             * flag as the result of pre_serialize or serialized callbacks. 
             * 
             * This in turn can corrupt the scan through the slist.
             *
             * We test for slist modifications in the pre_serialize 
             * and serialize callbacks, and restart the scan of the 
             * slist if we find them.  However, best we do some extra
             * sanity checking just in case.
             */
            HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
            HDassert(entry_ptr->in_slist);
            HDassert(entry_ptr->is_dirty);
            HDassert(entry_ptr->ring >= ring);

            /* increment node pointer now, before we delete its target
             * from the slist.
             */
            node_ptr = H5SL_next(node_ptr);
            if(node_ptr != NULL) {
                next_entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);
                if(NULL == next_entry_ptr)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "next_entry_ptr == NULL ?!?!")
                HDassert(next_entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
                HDassert(next_entry_ptr->is_dirty);
                HDassert(next_entry_ptr->in_slist);
                HDassert(next_entry_ptr->ring >= ring);
                HDassert(entry_ptr != next_entry_ptr);
            } /* end if */
            else
                next_entry_ptr = NULL;

            /* Note that we now remove nodes from the slist as we flush
             * the associated entries, instead of leaving them there
             * until we are done, and then destroying all nodes in
             * the slist.
             *
             * While this optimization used to be easy, with the possibility
             * of new entries being added to the slist in the midst of the
             * flush, we must keep the slist in cannonical form at all
             * times.
             */
            HDassert(entry_ptr != NULL);
            HDassert(entry_ptr->in_slist);

            if(((!entry_ptr->flush_me_last) ||
                    ((entry_ptr->flush_me_last) &&
                        (cache_ptr->num_last_entries >= cache_ptr->slist_len))) &&
                    (entry_ptr->flush_dep_nchildren == 0) &&
                    (entry_ptr->ring == ring)) {
                if(entry_ptr->is_protected) {
                    /* we have major problems -- but lets flush
                     * everything we can before we flag an error.
                     */
                    protected_entries++;
                } else if(entry_ptr->is_pinned) {

#if H5C_DO_SANITY_CHECKS
                    /* update flushed_slist_len & flushed_slist_size 
                     * before the flush.  Note that the entry will 
                     * be removed from the slist after the flush, 
                     * and thus may be resized by the flush callback.
                     * This is OK, as we will catch the size delta in
                     * cache_ptr->slist_size_increase.
                     *
                     */
                    flushed_slist_len++;
                    flushed_slist_size += (int64_t)entry_ptr->size;
                    entry_size_change = 0;
#endif /* H5C_DO_SANITY_CHECKS */

                    if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, H5C__NO_FLAGS_SET, entry_size_change_ptr, NULL) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "dirty pinned entry flush failed.")
#if H5C_DO_SANITY_CHECKS
                    /* entry size may have changed during the flush.
                     * Update flushed_slist_size to account for this.
                     */
                    flushed_slist_size += entry_size_change;
#endif /* H5C_DO_SANITY_CHECKS */

                    if((cache_ptr->slist_change_in_serialize) ||
                            (cache_ptr->slist_change_in_pre_serialize)) {
                        /* The slist has been modified by something
                         * other than the simple removal of the
                         * of the flushed entry after the flush.
                         *
                         * This has the potential to corrupt the
                         * scan through the slist, so restart it.
                         */
                        restart_slist_scan = TRUE;
                        cache_ptr->slist_change_in_pre_serialize = FALSE;
                        cache_ptr->slist_change_in_serialize = FALSE;
                        H5C__UPDATE_STATS_FOR_SLIST_SCAN_RESTART(cache_ptr);
                    } /* end if */
                } /* end if */
                else {
#if H5C_DO_SANITY_CHECKS
                    /* update flushed_slist_len & flushed_slist_size 
                     * before the flush.  Note that the entry will 
                     * be removed from the slist after the flush, 
                     * and thus may be resized by the flush callback.
                     * This is OK, as we will catch the size delta in
                     * cache_ptr->slist_size_increase.
                     *
                     */
                    flushed_slist_len++;
                    flushed_slist_size += (int64_t)entry_ptr->size;
                    entry_size_change = 0;
#endif /* H5C_DO_SANITY_CHECKS */

                    if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, 
                                (cooked_flags | H5C__FLUSH_INVALIDATE_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG), 
                                entry_size_change_ptr, NULL) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "dirty entry flush destroy failed.")
#if H5C_DO_SANITY_CHECKS
                    /* entry size may have changed during the flush.
                     * Update flushed_slist_size to account for this.
                     */
                    flushed_slist_size += entry_size_change;
#endif /* H5C_DO_SANITY_CHECKS */

                    if((cache_ptr->slist_change_in_serialize) ||
                            (cache_ptr->slist_change_in_pre_serialize)) {
                        /* The slist has been modified by something
                         * other than the simple removal of the
                         * of the flushed entry after the flush.
                         *
                         * This has the potential to corrupt the
                         * scan through the slist, so restart it.
                         */
                        restart_slist_scan = TRUE;
                        cache_ptr->slist_change_in_pre_serialize = FALSE;
                        cache_ptr->slist_change_in_serialize = FALSE;
                        H5C__UPDATE_STATS_FOR_SLIST_SCAN_RESTART(cache_ptr)
                    } /* end if */
                } /* end else */
            } /* end if */
        } /* end while loop scanning skip list */

#if H5C_DO_SANITY_CHECKS
        /* It is possible that entries were added to the slist during
         * the scan, either before or after scan pointer.  The following
         * asserts take this into account.
         *
         * Don't bother with the sanity checks if node_ptr != NULL, as
         * in this case we broke out of the loop because it got changed
         * out from under us.
         */

        if(node_ptr == NULL) {
            HDassert((flushed_slist_len + cache_ptr->slist_len) ==
                    (initial_slist_len + cache_ptr->slist_len_increase));
            HDassert((flushed_slist_size + (int64_t)cache_ptr->slist_size) ==
                    ((int64_t)initial_slist_size + cache_ptr->slist_size_increase));
        } /* end if */
#endif /* H5C_DO_SANITY_CHECKS */

        /* Since we are doing a destroy, we must make a pass through
         * the hash table and try to flush - destroy all entries that
         * remain.
         *
         * It used to be that all entries remaining in the cache at
         * this point had to be clean, but with the fractal heap mods
         * this may not be the case.  If so, we will flush entries out
         * of increasing address order.
         *
         * Writes to disk are possible here.
         */
        for(i = 0; i < H5C__HASH_TABLE_LEN; i++) {
            next_entry_ptr = cache_ptr->index[i];

            while(next_entry_ptr != NULL) {
                entry_ptr = next_entry_ptr;
                HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
                HDassert(entry_ptr->ring >= ring);

                next_entry_ptr = entry_ptr->ht_next;
                HDassert((next_entry_ptr == NULL) ||
                        (next_entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC));

                if(((!entry_ptr->flush_me_last) ||
                       ((entry_ptr->flush_me_last) &&
                            (cache_ptr->num_last_entries >= cache_ptr->slist_len))) &&
                       (entry_ptr->flush_dep_nchildren == 0) &&
                       (entry_ptr->ring == ring)) {

                    if(entry_ptr->is_protected) {
                        /* we have major problems -- but lets flush and 
                         * destroy everything we can before we flag an 
                         * error.
                         */
                        protected_entries++;
                        if(!entry_ptr->in_slist)
                            HDassert(!(entry_ptr->is_dirty));
                    } else if(!(entry_ptr->is_pinned)) {

                        /* if *entry_ptr is dirty, it is possible 
                         * that one or more other entries may be 
                         * either removed from the cache, loaded 
                         * into the cache, or moved to a new location
                         * in the file as a side effect of the flush.
                         *
                         * If this happens, and one of the target 
                         * entries happens to be the next entry in 
                         * the hash bucket, we could find ourselves 
                         * either find ourselves either scanning a 
                         * non-existant entry, scanning through a 
                         * different bucket, or skipping an entry.
                         *
                         * Neither of these are good, so restart the 
                         * the scan at the head of the hash bucket 
                         * after the flush if *entry_ptr was dirty,
                         * on the off chance that the next entry was
                         * a target.
                         *
                         * This is not as inefficient at it might seem,
                         * as hash buckets typically have at most two
                         * or three entries.
                         */
                        hbool_t entry_was_dirty;

                        entry_was_dirty = entry_ptr->is_dirty;

                        if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, 
                                (cooked_flags | H5C__FLUSH_INVALIDATE_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG), 
                                NULL, NULL) < 0)
                            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Entry flush destroy failed.")

                        if(entry_was_dirty) {
                            /* update stats for hash bucket scan
                             * restart here.
                             *                   -- JRM 
                             */
                            next_entry_ptr = cache_ptr->index[i];
                            H5C__UPDATE_STATS_FOR_HASH_BUCKET_SCAN_RESTART(cache_ptr)
                        } /* end if */
                    } /* end if */
                } /* end if */

                /* We can't do anything if the entry is pinned.  The
                 * hope is that the entry will be unpinned as the
                 * result of destroys of entries that reference it.
                 *
                 * We detect this by noting the change in the number
                 * of pinned entries from pass to pass.  If it stops
                 * shrinking before it hits zero, we scream and die.
                 */
                /* if the serialize function on the entry we last evicted
                 * loaded an entry into cache (as Quincey has promised me
                 * it never will), and if the cache was full, it is
                 * possible that *next_entry_ptr was flushed or evicted.
                 *
                 * Test to see if this happened here.  Note that if this
                 * test is triggred, we are accessing a deallocated piece
                 * of dynamically allocated memory, so we just scream and
                 * die.
                 *
                 * Update: The code to restart the scan after flushes
                 *         of dirty entries should make it impossible 
                 *         to satisfy the following test.  Leave it in
                 *         in case I am wrong.
                 *                                    -- JRM
                 */
                if((next_entry_ptr != NULL) && (next_entry_ptr->magic != H5C__H5C_CACHE_ENTRY_T_MAGIC))
                    /* Something horrible has happened to
                     * *next_entry_ptr -- scream and die.
                     */
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "next_entry_ptr->magic is invalid?!?!?.")
            } /* end while loop scanning hash table bin */
        } /* end for loop scanning hash table */

	old_ring_pel_len = cur_ring_pel_len;
        entry_ptr = cache_ptr->pel_head_ptr;
        cur_ring_pel_len = 0;
        while(entry_ptr != NULL) {
            HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
            HDassert(entry_ptr->ring >= ring);

	    if(entry_ptr->ring == ring)
                cur_ring_pel_len++;

            entry_ptr = entry_ptr->next;
        } /* end while */

	if((cur_ring_pel_len > 0) && (cur_ring_pel_len >= old_ring_pel_len)) {
            /* Don't error if allowed to have pinned entries remaining */
	    if(evict_flags)
                HGOTO_DONE(TRUE)

	   /* The number of pinned entries in the ring is positive, and 
            * it is not declining.  Scream and die.
	    */
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Pinned entry count not decreasing, cur_ring_pel_len = %d, old_ring_pel_len = %d, ring = %d", (int)cur_ring_pel_len, (int)old_ring_pel_len, (int)ring)
        } /* end if */

        HDassert(protected_entries == cache_ptr->pl_len);
        if((protected_entries > 0) && (protected_entries == cache_ptr->index_len))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Only protected entries left in cache, protected_entries = %d", (int)protected_entries)
    } /* main while loop */

    /* Invariants, after destroying all entries in the ring */
    for(i = (int)H5C_RING_UNDEFINED; i <= (int)ring; i++) {
        HDassert(cache_ptr->index_ring_len[i] == 0);
        HDassert(cache_ptr->index_ring_size[i] == (size_t)0);
        HDassert(cache_ptr->clean_index_ring_size[i] == (size_t)0);
        HDassert(cache_ptr->dirty_index_ring_size[i] == (size_t)0);

        HDassert(cache_ptr->slist_ring_len[i] == 0);
        HDassert(cache_ptr->slist_ring_size[i] == (size_t)0);
    } /* end for */

    HDassert(protected_entries <= cache_ptr->pl_len);

    if(protected_entries > 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Cache has protected entries.")
    else if(cur_ring_pel_len > 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't unpin all pinned entries in ring.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_invalidate_ring() */


/*-------------------------------------------------------------------------
 * Function:    H5C_flush_ring
 *
 * Purpose:	Flush the entries contained in the specified cache and 
 *		ring.  All entries in rings outside the specified ring
 *		must have been flushed on entry.
 *
 *		If the cache contains protected entries in the specified
 *		ring, the function will fail, as protected entries cannot 
 *		be flushed.  However all unprotected entries in the target
 *		ring should be flushed before the function returns failure.
 *
 *		If flush dependencies appear in the target ring, the 
 *		function makes repeated passes through the slist flushing
 *		entries in flush dependency order.
 *
 * Return:      Non-negative on success/Negative on failure or if there was
 *		a request to flush all items and something was protected.
 *
 * Programmer:  John Mainzer
 *		9/1/15
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_flush_ring(H5F_t *f, hid_t dxpl_id, H5C_ring_t ring,  unsigned flags)
{
    H5C_t * cache_ptr = f->shared->cache;
    hbool_t		destroy;
    hbool_t		flushed_entries_last_pass;
    hbool_t		flush_marked_entries;
    hbool_t		ignore_protected;
    hbool_t		tried_to_flush_protected_entry = FALSE;
    hbool_t		restart_slist_scan;
    int32_t		protected_entries = 0;
    H5SL_node_t * 	node_ptr = NULL;
    H5C_cache_entry_t *	entry_ptr = NULL;
    H5C_cache_entry_t *	next_entry_ptr = NULL;
#if H5C_DO_SANITY_CHECKS
    int64_t		flushed_entries_count = 0;
    int64_t		flushed_entries_size = 0;
    int64_t		initial_slist_len = 0;
    size_t              initial_slist_size = 0;
    int64_t		entry_size_change;
    int64_t	      * entry_size_change_ptr = &entry_size_change;
#else /* H5C_DO_SANITY_CHECKS */
    int64_t           * entry_size_change_ptr = NULL;
#endif /* H5C_DO_SANITY_CHECKS */
    int                 i;
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_ptr->slist_ptr);
    HDassert((flags & H5C__FLUSH_INVALIDATE_FLAG) == 0);
    HDassert(ring > H5C_RING_UNDEFINED);
    HDassert(ring < H5C_RING_NTYPES);

#if H5C_DO_EXTREME_SANITY_CHECKS
    if((H5C_validate_protected_entry_list(cache_ptr) < 0) ||
            (H5C_validate_pinned_entry_list(cache_ptr) < 0) ||
            (H5C_validate_lru_list(cache_ptr) < 0))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "an extreme sanity check failed on entry.\n");
#endif /* H5C_DO_EXTREME_SANITY_CHECKS */

    ignore_protected = ( (flags & H5C__FLUSH_IGNORE_PROTECTED_FLAG) != 0 );
    destroy = ( (flags & H5C__FLUSH_INVALIDATE_FLAG) != 0 );
    flush_marked_entries = ( (flags & H5C__FLUSH_MARKED_ENTRIES_FLAG) != 0 );

    if(!flush_marked_entries)
        for(i = (int)H5C_RING_UNDEFINED; i < (int)ring; i++)
	    HDassert(cache_ptr->slist_ring_len[i] == 0);

    HDassert(cache_ptr->flush_in_progress);

    /* When we are only flushing marked entries, the slist will usually
     * still contain entries when we have flushed everything we should.
     * Thus we track whether we have flushed any entries in the last
     * pass, and terminate if we haven't.
     */
    flushed_entries_last_pass = TRUE;

    /* set the cache_ptr->slist_change_in_pre_serialize and
     * cache_ptr->slist_change_in_serialize to false.
     *
     * These flags are set to TRUE by H5C__flush_single_entry if the 
     * slist is modified by a pre_serialize or serialize call respectively.
     * H5C_flush_cache uses these flags to detect any modifications
     * to the slist that might corrupt the scan of the slist -- and 
     * restart the scan in this event.
     */
    cache_ptr->slist_change_in_pre_serialize = FALSE;
    cache_ptr->slist_change_in_serialize = FALSE;

    while((cache_ptr->slist_ring_len[ring] > 0) &&
	    (protected_entries == 0)  &&
	    (flushed_entries_last_pass)) {
        flushed_entries_last_pass = FALSE;

#if H5C_DO_SANITY_CHECKS
        /* For sanity checking, try to verify that the skip list has
         * the expected size and number of entries at the end of each
         * internal while loop (see below).
         *
         * Doing this get a bit tricky, as depending on flags, we may
         * or may not flush all the entries in the slist.
         *
         * To make things more entertaining, with the advent of the
         * fractal heap, the entry serialize callback can cause entries
         * to be dirtied, resized, and/or moved.  Also, the 
         * pre_serialize callback can result in an entry being 
         * removed from the cache via the take ownership flag.
         *
         * To deal with this, we first make note of the initial
         * skip list length and size:
         */
        initial_slist_len = cache_ptr->slist_len;
        initial_slist_size = cache_ptr->slist_size;

        /* We then zero counters that we use to track the number
         * and total size of entries flushed:
         */
        flushed_entries_count = 0;
        flushed_entries_size = 0;

        /* As mentioned above, there is the possibility that
         * entries will be dirtied, resized, flushed, or removed
         * from the cache via the take ownership flag  during
         * our pass through the skip list.  To capture the number
         * of entries added, and the skip list size delta,
         * zero the slist_len_increase and slist_size_increase of
         * the cache's instance of H5C_t.  These fields will be
         * updated elsewhere to account for slist insertions and/or
         * dirty entry size changes.
         */
        cache_ptr->slist_len_increase = 0;
        cache_ptr->slist_size_increase = 0;

        /* at the end of the loop, use these values to compute the
         * expected slist length and size and compare this with the
         * value recorded in the cache's instance of H5C_t.
         */
#endif /* H5C_DO_SANITY_CHECKS */

        restart_slist_scan = TRUE;

        while((restart_slist_scan ) || (node_ptr != NULL)) {
            if(restart_slist_scan) {
                restart_slist_scan = FALSE;

                /* Start at beginning of skip list */
                node_ptr = H5SL_first(cache_ptr->slist_ptr);

                if(node_ptr == NULL)
                    /* the slist is empty -- break out of inner loop */
                    break;

                /* Get cache entry for this node */
                next_entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

                if(NULL == next_entry_ptr)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "next_entry_ptr == NULL ?!?!")

                HDassert(next_entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
                HDassert(next_entry_ptr->is_dirty);
                HDassert(next_entry_ptr->in_slist);
            } /* end if */
                
            entry_ptr = next_entry_ptr;

            /* With the advent of the fractal heap, the free space
             * manager, and the version 3 cache, it is possible
             * that the pre-serialize or serialize callback will 
             * dirty, resize, or take ownership of other entries 
             * in the cache.  
             *
             * To deal with this, I have inserted code to detect any
             * change in the skip list not directly under the control
             * of this function.  If such modifications are detected,
             * we must re-start the scan of the skip list to avoid 
             * the possibility that the target of the next_entry_ptr
             * may have been flushed or deleted from the cache.
             *
             * To verify that all such possibilities have been dealt
             * with, we do a bit of extra sanity checking on 
             * entry_ptr.
             */
            HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
            HDassert(entry_ptr->in_slist);
            HDassert(entry_ptr->is_dirty);
            if(!flush_marked_entries || entry_ptr->flush_marker)
                HDassert(entry_ptr->ring >= ring);

            /* increment node pointer now, before we delete its target
             * from the slist.
             */
            node_ptr = H5SL_next(node_ptr);
            if(node_ptr != NULL) {
                next_entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);
                if(NULL == next_entry_ptr)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "next_entry_ptr == NULL ?!?!")

                HDassert(next_entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
                HDassert(next_entry_ptr->is_dirty);
                HDassert(next_entry_ptr->in_slist);
                if(!flush_marked_entries || next_entry_ptr->flush_marker)
                    HDassert(next_entry_ptr->ring >= ring);

                HDassert(entry_ptr != next_entry_ptr);
            } /* end if */
            else
                next_entry_ptr = NULL;

            HDassert(entry_ptr != NULL);
            HDassert(entry_ptr->in_slist);

            if(((!flush_marked_entries) || (entry_ptr->flush_marker)) &&
                    ((!entry_ptr->flush_me_last) ||
                       (entry_ptr->flush_me_last &&
                         ((cache_ptr->num_last_entries >= cache_ptr->slist_len) ||
                           (flush_marked_entries && entry_ptr->flush_marker)))) &&
                       ( ( entry_ptr->flush_dep_nchildren == 0 ) ||
                         ( ( ! destroy ) &&
                           ( entry_ptr->flush_dep_ndirty_children == 0 ) ) ) &&
                     (entry_ptr->ring == ring)) {
                if(entry_ptr->is_protected) {
                    /* we probably have major problems -- but lets 
                     * flush everything we can before we decide 
                     * whether to flag an error.
                     */
                    tried_to_flush_protected_entry = TRUE;
                    protected_entries++;
                } /* end if */
                else if(entry_ptr->is_pinned) {

#if H5C_DO_SANITY_CHECKS
                    flushed_entries_count++;
                    flushed_entries_size += (int64_t)entry_ptr->size;
                    entry_size_change = 0;
#endif /* H5C_DO_SANITY_CHECKS */

                    if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, flags, entry_size_change_ptr, NULL) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "dirty pinned entry flush failed.")

#if H5C_DO_SANITY_CHECKS
                    /* it is possible that the entry size changed
                     * during flush -- update flushed_entries_size
                     * to account for this.
                     */
                    flushed_entries_size += entry_size_change;
#endif /* H5C_DO_SANITY_CHECKS */

                    if((cache_ptr->slist_change_in_serialize) ||
                            (cache_ptr->slist_change_in_pre_serialize)) {
                        /* The slist has been modified by something
                         * other than the simple removal of the 
                         * of the flushed entry after the flush.
                         * 
                         * This has the potential to corrupt the
                         * scan through the slist, so restart it.
                         */
                        restart_slist_scan = TRUE;
                        cache_ptr->slist_change_in_pre_serialize = FALSE;
                        cache_ptr->slist_change_in_serialize = FALSE;

                        H5C__UPDATE_STATS_FOR_SLIST_SCAN_RESTART(cache_ptr)
                    } /* end if */

                    flushed_entries_last_pass = TRUE;
                } /* end else-if */
                else {
#if H5C_DO_SANITY_CHECKS
                    flushed_entries_count++;
                    flushed_entries_size += (int64_t)entry_ptr->size;
                    entry_size_change = 0;
#endif /* H5C_DO_SANITY_CHECKS */
                    if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, flags, entry_size_change_ptr, NULL) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush entry.")

#if H5C_DO_SANITY_CHECKS
                    /* it is possible that the entry size changed
                     * during flush -- update flushed_entries_size
                     * to account for this.
                     */
                    flushed_entries_size += entry_size_change;
#endif /* H5C_DO_SANITY_CHECKS */

                    if((cache_ptr->slist_change_in_serialize) ||
                            (cache_ptr->slist_change_in_pre_serialize)) {
                        /* The slist has been modified by something
                         * other than the simple removal of the 
                         * of the flushed entry after the flush.
                         * 
                         * This has the potential to corrupt the
                         * scan through the slist, so restart it.
                         */
                        restart_slist_scan = TRUE;
                        cache_ptr->slist_change_in_pre_serialize = FALSE;
                        cache_ptr->slist_change_in_serialize = FALSE;

                        H5C__UPDATE_STATS_FOR_SLIST_SCAN_RESTART(cache_ptr)
                    } /* end if */

                    flushed_entries_last_pass = TRUE;
                } /* end else */
            } /* end if */
        } /* while ( ( restart_slist_scan ) || ( node_ptr != NULL ) ) */

#if H5C_DO_SANITY_CHECKS
        /* Verify that the slist size and length are as expected. */
        HDassert((initial_slist_len + cache_ptr->slist_len_increase -
                   flushed_entries_count) == cache_ptr->slist_len);
        HDassert((size_t)((int64_t)initial_slist_size + 
                   cache_ptr->slist_size_increase -
                   flushed_entries_size) == cache_ptr->slist_size);
#endif /* H5C_DO_SANITY_CHECKS */
    } /* while */

    HDassert(protected_entries <= cache_ptr->pl_len);

    if(((cache_ptr->pl_len > 0) && (!ignore_protected)) || (tried_to_flush_protected_entry))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "cache has protected items")

#if H5C_DO_SANITY_CHECKS
    if(!flush_marked_entries) {
        HDassert(cache_ptr->slist_ring_len[ring] == 0);
        HDassert(cache_ptr->slist_ring_size[ring] == 0);
    } /* end if */
#endif /* H5C_DO_SANITY_CHECKS */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_ring() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__flush_single_entry
 *
 * Purpose:     Flush or clear (and evict if requested) the cache entry
 *		with the specified address and type.  If the type is NULL,
 *		any unprotected entry at the specified address will be
 *		flushed (and possibly evicted).
 *
 *		Attempts to flush a protected entry will result in an
 *		error.
 *
 *		If the H5C__FLUSH_INVALIDATE_FLAG flag is set, the entry will
 *		be cleared and not flushed, and the call can't be part of a
 *              sequence of flushes.
 *
 *		If the caller knows the address of the skip list node at
 *		which the target entry resides, it can avoid a lookup
 *		by supplying that address in the tgt_node_ptr parameter.
 *		If this parameter is NULL, the function will do a skip list
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
 * Changes:	Refactored function to remove the type_ptr parameter.
 *
 *						JRM -- 8/7/14
 *
 *              Added code to check for slist changes in pre_serialize and
 *              serialize calls, and set 
 *              cache_ptr->slist_change_in_pre_serialize and 
 *		cache_ptr->slist_change_in_serialize as appropriate.
 *
 *                                              JRM -- 12/13/14
 *
 *		Refactored function to delay all modifications of the 
 *		metadata cache data structures until after any calls 
 *		to the pre-serialize or serialize callbacks.
 *
 *		Need to do this, as some pre-serialize or serialize 
 *		calls result in calls to the metadata cache and 
 *		modifications to its data structures.  Thus, at the
 *		time of any such call, the target entry flags and 
 *		the metadata cache must all be consistant.
 *
 *		Also added the entry_size_change_ptr parameter, which 
 *              allows the function to report back any change in the size 
 *		of the entry during the flush.  Such size changes may 
 *		occur during the pre-serialize callback.
 *
 *						JRM -- 12/24/14
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C__flush_single_entry(const H5F_t *f, hid_t dxpl_id, H5C_cache_entry_t *entry_ptr,
    unsigned flags, int64_t *entry_size_change_ptr, H5SL_t
#ifndef H5_HAVE_PARALLEL
    H5_ATTR_UNUSED
#endif /* NDEBUG */
    *collective_write_list)
{
    H5C_t *	     	cache_ptr;              /* Cache for file */
    hbool_t		destroy;		/* external flag */
    hbool_t		clear_only;		/* external flag */
    hbool_t		free_file_space;	/* external flag */
    hbool_t		take_ownership;		/* external flag */
    hbool_t             del_from_slist_on_destroy;    /* external flag */
    hbool_t		write_entry;		/* internal flag */
    hbool_t		destroy_entry;		/* internal flag */
    hbool_t		was_dirty;
    haddr_t             entry_addr = HADDR_UNDEF;
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(f);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(entry_ptr);
    HDassert(entry_ptr->ring != H5C_RING_UNDEFINED);

    /* If defined, initialize *entry_size_change_ptr to 0 */
    if(entry_size_change_ptr != NULL)
        *entry_size_change_ptr = 0;

    /* setup external flags from the flags parameter */
    destroy                = ((flags & H5C__FLUSH_INVALIDATE_FLAG) != 0);
    clear_only             = ((flags & H5C__FLUSH_CLEAR_ONLY_FLAG) != 0);
    free_file_space        = ((flags & H5C__FREE_FILE_SPACE_FLAG) != 0);
    take_ownership         = ((flags & H5C__TAKE_OWNERSHIP_FLAG) != 0);
    del_from_slist_on_destroy = ((flags & H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) != 0);

    /* Set the flag for destroying the entry, based on the 'take ownership'
     * and 'destroy' flags
     */
    if(take_ownership)
        destroy_entry = FALSE;
    else
        destroy_entry = destroy;

#ifdef H5_HAVE_PARALLEL
    HDassert(FALSE == entry_ptr->coll_access);
#endif

    /* we will write the entry to disk if it exists, is dirty, and if the
     * clear only flag is not set.
     */
    if(entry_ptr->is_dirty && !clear_only)
        write_entry = TRUE;
    else
        write_entry = FALSE;

    /* run initial sanity checks */
#if H5C_DO_SANITY_CHECKS
    HDassert( ! ( destroy && entry_ptr->is_pinned ) );

    if(entry_ptr->in_slist) {
        HDassert(entry_ptr->is_dirty);

        if((entry_ptr->flush_marker) && (!entry_ptr->is_dirty))
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "entry in slist failed sanity checks.")
    } else {
        HDassert(!entry_ptr->is_dirty);
        HDassert(!entry_ptr->flush_marker);

        if((entry_ptr->is_dirty) || (entry_ptr->flush_marker))
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "entry failed sanity checks.")
    }
#endif /* H5C_DO_SANITY_CHECKS */

    if(entry_ptr->is_protected) {
	HDassert(!entry_ptr->is_protected);

        /* Attempt to flush a protected entry -- scream and die. */
        HGOTO_ERROR(H5E_CACHE, H5E_PROTECT, FAIL, "Attempt to flush a protected entry.")
    } /* end if */

    /* set entry_ptr->flush_in_progress = TRUE and set
     * entry_ptr->flush_marker = FALSE
     *
     * in the parallel case, do some sanity checking in passing.
     */
    HDassert(entry_ptr->type);

    was_dirty = entry_ptr->is_dirty;  /* needed later for logging */

    /* We will set flush_in_progress back to FALSE at the end if the
     * entry still exists at that point.
     */
    entry_ptr->flush_in_progress = TRUE;
    entry_ptr->flush_marker = FALSE;

    /* serialize the entry if necessary, and then write it to disk. */
    if(write_entry) {

	/* The entry is dirty, and we are doing either a flush,
	 * or a flush destroy.  In either case, serialize the
	 * entry and write it to disk.
         *
         * Note that this may cause the entry to be re-sized and/or
         * moved in the cache.  
	 *
         * As we will not update the metadata cache's data structures 
         * until we we finish the write, we must touch up these 
         * data structures for size and location changes even if we 
         * are about to delete the entry from the cache (i.e. on a 
         * flush destroy).
         */
        HDassert(entry_ptr->is_dirty);

#if H5C_DO_SANITY_CHECKS
        if(cache_ptr->check_write_permitted && !(cache_ptr->write_permitted))
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Write when writes are always forbidden!?!?!")
#endif /* H5C_DO_SANITY_CHECKS */

        if(NULL == entry_ptr->image_ptr) {
            size_t image_size;

            if(entry_ptr->compressed)
                image_size = entry_ptr->compressed_size;
            else
                image_size = entry_ptr->size;
            HDassert(image_size > 0);


            if(NULL == (entry_ptr->image_ptr = H5MM_malloc(image_size + H5C_IMAGE_EXTRA_SPACE)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "memory allocation failed for on disk image buffer")
#if H5C_DO_MEMORY_SANITY_CHECKS
            HDmemcpy(((uint8_t *)entry_ptr->image_ptr) + image_size, H5C_IMAGE_SANITY_VALUE, H5C_IMAGE_EXTRA_SPACE);
#endif /* H5C_DO_MEMORY_SANITY_CHECKS */
        } /* end if */

        if(!(entry_ptr->image_up_to_date)) {
            /* Generate the entry's image */
            if(H5C__generate_image(f, cache_ptr, entry_ptr, dxpl_id, entry_size_change_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't generate entry's image")
        } /* end if ( ! (entry_ptr->image_up_to_date) ) */

        /* Finally, write the image to disk.  
         * 
         * Note that if either the H5C__CLASS_NO_IO_FLAG or the 
         * the H5AC__CLASS_SKIP_WRITES flag is set in the 
         * in the entry's type, we silently skip the write.  This
         * flag should only be used in test code. 
         */
        if ( ( ((entry_ptr->type->flags) & H5C__CLASS_NO_IO_FLAG) == 0 ) &&
             ( ((entry_ptr->type->flags) & H5C__CLASS_SKIP_WRITES) == 0 ) )
        {
	    /* If compression is not enabled, the size of the entry on 
             * disk is entry_prt->size.  However if entry_ptr->compressed
             * is TRUE, the on disk size is entry_ptr->compressed_size.
             */
            size_t image_size;

            if(entry_ptr->compressed)
                image_size = entry_ptr->compressed_size;
            else
                image_size = entry_ptr->size;

#ifdef H5_HAVE_PARALLEL
            if(collective_write_list) {
                H5C_collective_write_t *item;

                if(NULL == (item = (H5C_collective_write_t *)H5FL_MALLOC(H5C_collective_write_t)))
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "unable to allocate skip list item")

                item->length = image_size;
                item->free_buf = FALSE;
                item->buf = entry_ptr->image_ptr;
                item->offset = entry_ptr->addr;

                if(H5SL_insert(collective_write_list, item, &item->offset) < 0) {
                    H5MM_free(item);
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "unable to insert skip list item")
                } /* end if */
            } /* end if */
            else
#endif /* H5_HAVE_PARALLEL */
            if(H5F_block_write(f, entry_ptr->type->mem_type, entry_ptr->addr,
                    image_size, dxpl_id, entry_ptr->image_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't write image to file.")
        }

        /* if the entry has a notify callback, notify it that we have 
         * just flushed the entry.
         */
        if(entry_ptr->type->notify &&
             (entry_ptr->type->notify)(H5C_NOTIFY_ACTION_AFTER_FLUSH, entry_ptr) < 0 )
            HGOTO_ERROR(H5E_CACHE, H5E_CANTNOTIFY, FAIL, "can't notify client of entry flush")
    } /* if ( write_entry ) */

    /* At this point, all pre-serialize and serialize calls have been
     * made if it was appropriate to make them.  Similarly, the entry
     * has been written to disk if desired.
     *
     * Thus it is now safe to update the cache data structures for the 
     * flush.
     */

    /* start by updating the statistics */
    if(clear_only) {
        /* only log a clear if the entry was dirty */
        if(was_dirty) {
            H5C__UPDATE_STATS_FOR_CLEAR(cache_ptr, entry_ptr)
        } /* end if */
    } else if(write_entry) {
        HDassert(was_dirty);

        /* only log a flush if we actually wrote to disk */
        H5C__UPDATE_STATS_FOR_FLUSH(cache_ptr, entry_ptr)
    } /* end else if */

    if(destroy) {
        if(take_ownership)
            HDassert(!destroy_entry);
        else
            HDassert(destroy_entry);

        H5C__UPDATE_STATS_FOR_EVICTION(cache_ptr, entry_ptr, take_ownership)
    } /* end if */

    /* If the entry's type has a 'notify' callback and the entry is about
     * to be removed from the cache, send a 'before eviction' notice while
     * the entry is still fully integrated in the cache.
     */
    if(destroy)
        if(entry_ptr->type->notify && (entry_ptr->type->notify)(H5C_NOTIFY_ACTION_BEFORE_EVICT, entry_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTNOTIFY, FAIL, "can't notify client about entry to evict")

    /* Update the cache internal data structures. */
    if(destroy) {
        /* Update the cache internal data structures as appropriate
         * for a destroy.  Specifically:
         *
         * 1) Delete it from the index
         *
         * 2) Delete it from the skip list if requested.
         *
         * 3) Update the replacement policy for eviction
         *
         * Finally, if the destroy_entry flag is set, discard the 
         * entry.
         */

        H5C__DELETE_FROM_INDEX(cache_ptr, entry_ptr)

        if(entry_ptr->in_slist && del_from_slist_on_destroy)
            H5C__REMOVE_ENTRY_FROM_SLIST(cache_ptr, entry_ptr)

        H5C__UPDATE_RP_FOR_EVICTION(cache_ptr, entry_ptr, FAIL)

#if 0 /* this is useful debugging code -- leave it in for now.  -- JRM */
	if ( ( entry_ptr->flush_dep_nparents > 0 ) ||
             ( entry_ptr->flush_dep_nchildren > 0 ) ) {

	    int i;

	    HDfprintf(stdout, 
                    "\n\nattempting to evict entry of type \"%s\" at 0X%llx:\n",
                    entry_ptr->type->name, (long long)(entry_ptr->addr));

	    for ( i = 0; i < entry_ptr->flush_dep_nparents; i++ ) {

		HDfprintf(stdout, 
                          "	with FD parent of type \"%s\" at 0X%llx.\n",
			  entry_ptr->flush_dep_parent[i]->type->name,
			  (long long)(entry_ptr->flush_dep_parent[i]->addr));
	    }

	    HDfprintf(stdout, "	with %d FD children.\n\n", 
                      entry_ptr->flush_dep_nchildren);
        }
#endif /* this is useful debugging code -- leave it in for now.  -- JRM */

	/* verify that the entry is no longer part of any flush dependencies */
        HDassert(entry_ptr->flush_dep_nparents == 0);
	HDassert(entry_ptr->flush_dep_nchildren == 0);
    }
    else {
        HDassert(clear_only || write_entry);
        HDassert(entry_ptr->is_dirty);
        HDassert(entry_ptr->in_slist);

        /* We are either doing a flush or a clear.
         *
         * A clear and a flush are the same from the point of
         * view of the replacement policy and the slist.  
         * Hence no differentiation between them.
         *
         * 					JRM -- 7/7/07
         */

        H5C__UPDATE_RP_FOR_FLUSH(cache_ptr, entry_ptr, FAIL)

        H5C__REMOVE_ENTRY_FROM_SLIST(cache_ptr, entry_ptr)

        /* mark the entry as clean and update the index for 
         * entry clean.  Also, call the clear callback 
         * if defined.
         */
        entry_ptr->is_dirty = FALSE;

        H5C__UPDATE_INDEX_FOR_ENTRY_CLEAN(cache_ptr, entry_ptr);

        if(entry_ptr->type->clear && (entry_ptr->type->clear)(f, (void *)entry_ptr, FALSE) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to clear entry")

	/* Propagate the clean flag up the flush dependency chain if
         * appropriate */
        if(was_dirty) {
	    HDassert(entry_ptr->flush_dep_ndirty_children == 0);

	    if(entry_ptr->flush_dep_nparents > 0)
		if(H5C__mark_flush_dep_clean(entry_ptr) < 0)
		    HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "Can't propagate flush dep clean flag")
	} /* end if */

    }

    /* reset the flush_in progress flag */
    entry_ptr->flush_in_progress = FALSE;

    /* capture the cache entry address for the log_flush call at the
       end before the entry_ptr gets freed */
    entry_addr = entry_ptr->addr;

    /* Internal cache data structures should now be up to date, and 
     * consistant with the status of the entry.  
     *
     * Now discard the entry if appropriate.
     */
    if(destroy) {
        /* start by freeing the buffer for the on disk image */
        if(entry_ptr->image_ptr != NULL)
            entry_ptr->image_ptr = H5MM_xfree(entry_ptr->image_ptr);

        /* Check whether we should free the space in the file that 
         * the entry occupies 
         */
        if(free_file_space) {
            size_t fsf_size;

            /* Sanity checks */
            HDassert(H5F_addr_defined(entry_ptr->addr));
            HDassert(!H5F_IS_TMP_ADDR(f, entry_ptr->addr));
#ifndef NDEBUG
{
            hbool_t curr_compressed = FALSE;
            size_t curr_len;
            size_t curr_compressed_len = 0;

            /* Get the actual image size for the thing again */
            entry_ptr->type->image_len((void *)entry_ptr, &curr_len, &curr_compressed, &curr_compressed_len);
            HDassert(curr_len == entry_ptr->size);
            HDassert(curr_compressed == entry_ptr->compressed);
            HDassert(curr_compressed_len == entry_ptr->compressed_size);
}
#endif /* NDEBUG */

            /* if the file space free size callback is defined, use
             * it to get the size of the block of file space to free.
             * Otherwise use entry_ptr->compressed_size if 
             * entry_ptr->compressed == TRUE, and entry_ptr->size
             * if entry_ptr->compressed == FALSE.
             */
            if(entry_ptr->type->fsf_size) {
                if((entry_ptr->type->fsf_size)((void *)entry_ptr, &fsf_size) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "unable to get file space free size")
            } /* end if */
            else if(entry_ptr->compressed) /* use compressed size */
                fsf_size = entry_ptr->compressed_size;
            else    /* no file space free size callback -- use entry size */
                fsf_size = entry_ptr->size;

            /* Release the space on disk */
            if(H5MF_xfree(f, entry_ptr->type->mem_type, dxpl_id, entry_ptr->addr, (hsize_t)fsf_size) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "unable to free file space for cache entry")
        } /* end if ( free_file_space ) */

        /* Reset the pointer to the cache the entry is within. -QAK */
        entry_ptr->cache_ptr = NULL;

        /* increment entries_removed_counter and set 
         * last_entry_removed_ptr.  As we are likely abuut to 
         * free the entry, recall that last_entry_removed_ptr 
         * must NEVER be dereferenced.
         *
         * Recall that these fields are maintained to allow functions
         * that perform scans of lists of entries to detect the 
         * unexpected removal of entries (via expunge, eviction, 
         * or take ownership at present), so that they can re-start
         * their scans if necessary.
         */
        cache_ptr->last_entry_removed_ptr++;
        cache_ptr->last_entry_removed_ptr = entry_ptr;

        /* Check for actually destroying the entry in memory */
        /* (As opposed to taking ownership of it) */
        if(destroy_entry) {
            /* if the entry is dirty and it has a clear callback,
             * call this callback now.  Since this callback exists,
             * it follows tht the client maintains its own dirty bits, 
             * which must be cleared before the entry is freed to avoid 
             * sanity check failures.  Also clear the dirty flag for 
             * the same reason.
             */
            if(entry_ptr->is_dirty) {
                entry_ptr->is_dirty = FALSE;

                if(entry_ptr->type->clear && (entry_ptr->type->clear)(f, (void *)entry_ptr, TRUE) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to clear entry")
            }

            /* we are about to discard the in core representation --
             * set the magic field to bad magic so we can detect a
             * freed entry if we see one.
             */
            entry_ptr->magic = H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC;

            /* verify that the image has been freed */
            HDassert(entry_ptr->image_ptr == NULL);

            if(entry_ptr->type->free_icr((void *)entry_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "free_icr callback failed.")
        } 
        else {
            HDassert(take_ownership);

            /* client is taking ownership of the entry.
             * set bad magic here too so the cache will choke 
             * unless the entry is re-inserted properly
             */
            entry_ptr->magic = H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC;
        }
    } /* if (destroy) */

    if(cache_ptr->log_flush)
        if((cache_ptr->log_flush)(cache_ptr, entry_addr, was_dirty, flags) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "log_flush callback failed.")

done:
    HDassert( ( ret_value != SUCCEED ) || ( destroy_entry ) || 
              ( ! entry_ptr->flush_in_progress ) );

    HDassert( ( ret_value != SUCCEED ) || ( destroy_entry ) || 
              ( take_ownership ) || ( ! entry_ptr->is_dirty ) );

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__flush_single_entry() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_verify_len_eoa
 *
 * Purpose:     Verify that 'len' does not exceed eoa when 'actual' is
 *              false i.e. 'len" is the initial speculative length from
 *              get_load_size callback with null image pointer.
 *              If exceed, adjust 'len' accordingly.
 *
 *              Verify that 'len' should not exceed eoa when 'actual' is
 *              true i.e. 'len' is the actual length from get_load_size 
 *              callback with non-null image pointer.
 *              If exceed, return error.
 *
 *              The coding is copied and moved from H5C_load_entry().
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Vailin Choi
 *              9/6/15
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_verify_len_eoa (H5F_t *             f,
               	    const H5C_class_t * type,
               	    haddr_t             addr,
		    size_t              *len,
               	    htri_t		actual)
{
    haddr_t eoa;                	/* End-of-allocation in the file */
    H5FD_mem_t cooked_type;
    herr_t ret_value = SUCCEED;      	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* if type == H5FD_MEM_GHEAP, H5F_block_read() forces 
     * type to H5FD_MEM_DRAW via its call to H5F__accum_read().
     * Thus we do the same for purposes of computing the eoa
     * for sanity checks.
     */
    cooked_type = (type->mem_type == H5FD_MEM_GHEAP) ? H5FD_MEM_DRAW : type->mem_type;

    /* Get the file's end-of-allocation value */
    eoa = H5F_get_eoa(f, cooked_type);

    HDassert(H5F_addr_defined(eoa));

    /* Check for bad address in general */
    if ( H5F_addr_gt(addr, eoa) )
	HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "address of object past end of allocation")

    /* Check if the amount of data to read will be past the eoa */
    if( H5F_addr_gt((addr + *len), eoa) ) {

	if(actual)
	    HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "actual len exceeds EOA.")
	else
	    /* Trim down the length of the metadata */
	    /* Note that for some cache clients, this will cause an 
	     * assertion failure.		JRM -- 8/29/14
	     */
	    *len = (size_t)(eoa - addr);
    }

    if ( *len <= 0 )
	HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "len not positive after adjustment for EOA.")

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_verify_len_eoa() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_load_entry
 *
 * Purpose:     Attempt to load the entry at the specified disk address
 *              and with the specified type into memory.  If successful.
 *              return the in memory address of the entry.  Return NULL
 *              on failure.
 *
 *              Note that this function simply loads the entry into
 *              core.  It does not insert it into the cache.
 *
 * Return:      Non-NULL on success / NULL on failure.
 *
 * Programmer:  John Mainzer, 5/18/04
 *
 *-------------------------------------------------------------------------
 */
static void *
H5C_load_entry(H5F_t *             f,
                hid_t               dxpl_id,
#ifdef H5_HAVE_PARALLEL
                hbool_t             coll_access,
#endif /* H5_HAVE_PARALLEL */
                const H5C_class_t * type,
                haddr_t             addr,
                void *              udata)
{
    hbool_t     dirty = FALSE;          /* Flag indicating whether thing was dirtied during deserialize */
    hbool_t     compressed = FALSE;     /* flag indicating whether thing            */
                                        /* will be run through filters on           */
                                        /* on read and write.  Usually FALSE        */
                                        /* set to true if appropriate.              */
    size_t      compressed_size = 0;    /* entry compressed size if                 */
                                        /* known -- otherwise uncompressed.         */
                                        /* Zero indicates compression not           */
                                        /* enabled.                                 */
    uint8_t *   image = NULL;           /* Buffer for disk image                    */
    void *      thing = NULL;           /* Pointer to thing loaded                  */
    H5C_cache_entry_t *entry = NULL;    /* Alias for thing loaded, as cache entry   */
    size_t      len;                    /* Size of image in file                    */
#ifdef H5_HAVE_PARALLEL
    int         mpi_rank = 0;           /* MPI process rank                         */
    MPI_Comm    comm = MPI_COMM_NULL;   /* File MPI Communicator                    */
    int         mpi_code;               /* MPI error code                           */
#endif /* H5_HAVE_PARALLEL */
    void *      ret_value = NULL;       /* Return value                             */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);

    /* verify absence of prohibited or unsupported type flag combinations */
    HDassert(!(type->flags & H5C__CLASS_NO_IO_FLAG));

    /* for now, we do not combine the speculative load and compressed flags */
    HDassert(!((type->flags & H5C__CLASS_SPECULATIVE_LOAD_FLAG) &&
               (type->flags & H5C__CLASS_COMPRESSED_FLAG)));

    /* Can't see how skip reads could be usefully combined with
     * either the speculative read or compressed flags.  Hence disallow.
     */
    HDassert(!((type->flags & H5C__CLASS_SKIP_READS) &&
               (type->flags & H5C__CLASS_SPECULATIVE_LOAD_FLAG)));
    HDassert(!((type->flags & H5C__CLASS_SKIP_READS) &&
               (type->flags & H5C__CLASS_COMPRESSED_FLAG)));

    HDassert(H5F_addr_defined(addr));
    HDassert(type->get_load_size);
    HDassert(type->deserialize);

    /* Call the get_load_size callback, to retrieve the initial 
     * size of image 
     */
    if(type->get_load_size(NULL, udata, &len, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, NULL, "can't retrieve image size")

    HDassert(len > 0);

    /* Check for possible speculative read off the end of the file */
    if(type->flags & H5C__CLASS_SPECULATIVE_LOAD_FLAG)
        if(H5C_verify_len_eoa(f, type, addr, &len, FALSE) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, NULL, "invalid len with respect to EOA.")

    /* Allocate the buffer for reading the on-disk entry image */
    if(NULL == (image = (uint8_t *)H5MM_malloc(len + H5C_IMAGE_EXTRA_SPACE)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, NULL, "memory allocation failed for on disk image buffer.")

#if H5C_DO_MEMORY_SANITY_CHECKS
    HDmemcpy(image + len, H5C_IMAGE_SANITY_VALUE, H5C_IMAGE_EXTRA_SPACE);
#endif /* H5C_DO_MEMORY_SANITY_CHECKS */

#ifdef H5_HAVE_PARALLEL
    if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI)) {
        if((mpi_rank = H5F_mpi_get_rank(f)) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "Can't get MPI rank")
        if((comm = H5F_mpi_get_comm(f)) == MPI_COMM_NULL)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "get_comm request failed")
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    /* Get the on-disk entry image */
    if(0 == (type->flags & H5C__CLASS_SKIP_READS)) {
        unsigned tries, max_tries;      /* The # of read attempts               */
        unsigned retries;               /* The # of retries                     */
        htri_t chk_ret;                 /* return from verify_chksum callback   */
        size_t actual_len = len;
        void *new_image = NULL;         /* Pointer to image                     */

        /* Get the # of read attempts */
        max_tries = tries = H5F_GET_READ_ATTEMPTS(f);

        /* 
         * This do/while loop performs the following till the metadata checksum is correct or the
         * file's allowed read attempts are reached.
         *   --read the metadata
         *   --determine the actual size of the metadata
         *   --perform checksum verification
         */
        do {
            compressed = FALSE;
            compressed_size = 0;

            if(actual_len != len) {
                if(NULL == (new_image = H5MM_realloc(image, len + H5C_IMAGE_EXTRA_SPACE)))
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, NULL, "image null after H5MM_realloc()")
                image = (uint8_t *)new_image;
            } /* end if */

#ifdef H5_HAVE_PARALLEL
            if(!coll_access || 0 == mpi_rank) {
#endif /* H5_HAVE_PARALLEL */
                if(H5F_block_read(f, type->mem_type, addr, len, dxpl_id, image) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_READERROR, NULL, "Can't read image*")
#ifdef H5_HAVE_PARALLEL
            } /* end if */
            /* if the collective metadata read optimization is turned on,
             * bcast the metadata read from process 0 to all ranks in the file
             * communicator
             */
            if(coll_access) {
                int buf_size;

                H5_CHECKED_ASSIGN(buf_size, int, len, size_t);
                if(MPI_SUCCESS != (mpi_code = MPI_Bcast(image, buf_size, MPI_BYTE, 0, comm)))
                    HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)
            } /* end if */
#endif /* H5_HAVE_PARALLEL */
            actual_len = len;

            if(type->get_load_size(image, udata, &len, &actual_len, &compressed, &compressed_size) < 0)
                continue;   /* Transfer control to while() and count towards retries */

            HDassert(((type->flags & H5C__CLASS_COMPRESSED_FLAG) != 0) ||
                     ((compressed == FALSE) && (compressed_size == 0)));
            HDassert((compressed == TRUE) || (compressed_size == 0));

            if(actual_len != len) {

                if(type->flags & H5C__CLASS_COMPRESSED_FLAG) {
                    /* if actual_len != len, then compression must be enabled on the entry.  
                     * In this case, the get_load_size callback should have set compressed to TRUE,
                     * compressed_size to the compressed size (which must equal to len),
                     * and actual_len to the uncompressed size of the entry,
                     * We can't verify the uncompressed size, but we can verify the rest
                     * with the following assertions.
                     */
                    HDassert(compressed);
                    HDassert(compressed_size == len);
                } else if(type->flags & H5C__CLASS_SPECULATIVE_LOAD_FLAG) {
                    size_t temp_len = actual_len;

                    /* compressed must be FALSE, and compressed_size
                     * must be zero.
                     */
                    HDassert(!compressed);
                    HDassert(compressed_size == 0);

                    if(H5C_verify_len_eoa(f, type, addr, &temp_len, TRUE) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, NULL, "actual_len exceeds EOA.")
                    HDassert(temp_len == actual_len);

                    if(NULL == (new_image = H5MM_realloc(image, actual_len + H5C_IMAGE_EXTRA_SPACE)))
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, NULL, "image null after H5MM_realloc()")
                    image = (uint8_t *)new_image;

#if H5C_DO_MEMORY_SANITY_CHECKS
                    HDmemcpy(image + actual_len, H5C_IMAGE_SANITY_VALUE, H5C_IMAGE_EXTRA_SPACE);
#endif /* H5C_DO_MEMORY_SANITY_CHECKS */

                    if(actual_len > len) {
#ifdef H5_HAVE_PARALLEL
                        if(!coll_access || 0 == mpi_rank) {
#endif /* H5_HAVE_PARALLEL */
                            /* If the thing's image needs to be bigger for a speculatively
                             * loaded thing, go get the on-disk image again (the extra portion).
                             */
                            if(H5F_block_read(f, type->mem_type, addr+len, actual_len-len, dxpl_id, image+len) < 0)
                                HGOTO_ERROR(H5E_CACHE, H5E_CANTLOAD, NULL, "Can't read image")
#ifdef H5_HAVE_PARALLEL
                        }
                        /* if the collective metadata read optimization is turned on,
                           bcast the metadata read from process 0 to all ranks in the file
                           communicator */
                        if(coll_access) {
                            int buf_size;

                            H5_CHECKED_ASSIGN(buf_size, int, actual_len-len, size_t);
                            if(MPI_SUCCESS != (mpi_code = MPI_Bcast(image+len, buf_size, MPI_BYTE, 0, comm)))
                                HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)
                        } /* end if */
#endif /* H5_HAVE_PARALLEL */
                    }
                } else {
                    /* throw an error */
                    HGOTO_ERROR(H5E_CACHE, H5E_UNSUPPORTED, NULL, "size of non-speculative, non-compressed object changed")
                } /* end else */
            } /* end if (actual_len != len) */

            if(type->verify_chksum == NULL)
                break;

            if((chk_ret = type->verify_chksum(image, actual_len, udata)) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_NOSPACE, NULL, "Failure from verify_chksum callback")
            if(chk_ret == TRUE)
                break;
        } while(--tries);

        /* Check for too many tries */
        if(tries == 0)
            HGOTO_ERROR(H5E_CACHE, H5E_READERROR, NULL, "incorrect metadatda checksum after all read attempts")

        /* Calculate and track the # of retries */
        retries = max_tries - tries;
        if(retries) {        /* Does not track 0 retry */
            if(H5F_track_metadata_read_retries(f, (unsigned)type->mem_type, retries) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, NULL, "cannot track read tries = %u ", retries)
        }  /* end if */
        len = actual_len;

    } /* end if !H5C__CLASS_SKIP_READS */

    /* Deserialize the on-disk image into the native memory form */
    if(NULL == (thing = type->deserialize(image, len, udata, &dirty)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTLOAD, NULL, "Can't deserialize image")

    entry = (H5C_cache_entry_t *)thing;

    /* In general, an entry should be clean just after it is loaded.
     *
     * However, when this code is used in the metadata cache, it is
     * possible that object headers will be dirty at this point, as
     * the deserialize function will alter object headers if necessary to
     * fix an old bug.
     *
     * In the following assert:
     *
     * 	HDassert( ( dirty == FALSE ) || ( type->id == 5 || type->id == 6 ) );
     *
     * note that type ids 5 & 6 are associated with object headers in the
     * metadata cache.
     *
     * When we get to using H5C for other purposes, we may wish to
     * tighten up the assert so that the loophole only applies to the
     * metadata cache.
     */

    HDassert( ( dirty == FALSE ) || ( type->id == 5 || type->id == 6) );

    entry->magic                = H5C__H5C_CACHE_ENTRY_T_MAGIC;
    entry->cache_ptr            = f->shared->cache;
    entry->addr                 = addr;
    entry->size                 = len;
    HDassert(entry->size < H5C_MAX_ENTRY_SIZE);
    entry->compressed           = compressed;
    entry->compressed_size      = compressed_size;
    entry->image_ptr            = image;
    entry->image_up_to_date     = TRUE;
    entry->type                 = type;
    entry->is_dirty	            = dirty;
    entry->dirtied              = FALSE;
    entry->is_protected         = FALSE;
    entry->is_read_only         = FALSE;
    entry->ro_ref_count         = 0;
    entry->is_pinned            = FALSE;
    entry->in_slist             = FALSE;
    entry->flush_marker         = FALSE;
#ifdef H5_HAVE_PARALLEL
    entry->clear_on_unprotect   = FALSE;
    entry->flush_immediately    = FALSE;
    entry->coll_access          = coll_access;
#endif /* H5_HAVE_PARALLEL */
    entry->flush_in_progress    = FALSE;
    entry->destroy_in_progress  = FALSE;

    entry->ring                 = H5C_RING_UNDEFINED;

    /* Initialize flush dependency height fields */
    entry->flush_dep_parent     = NULL;
    entry->flush_dep_nparents   = 0;
    entry->flush_dep_parent_nalloc = 0;
    entry->flush_dep_nchildren  = 0;
    entry->flush_dep_ndirty_children = 0;
    entry->ht_next              = NULL;
    entry->ht_prev              = NULL;

    entry->next                 = NULL;
    entry->prev                 = NULL;

    entry->aux_next             = NULL;
    entry->aux_prev             = NULL;

#ifdef H5_HAVE_PARALLEL
    entry->coll_next            = NULL;
    entry->coll_prev            = NULL;
#endif /* H5_HAVE_PARALLEL */

    H5C__RESET_CACHE_ENTRY_STATS(entry);

    ret_value = thing;

done:
    /* Cleanup on error */
    if(NULL == ret_value) {
        /* Release resources */
        if ( thing && type->free_icr(thing) < 0 )
            HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, NULL, "free_icr callback failed")
        if(image)
            image = (uint8_t *)H5MM_xfree(image);
    } /* end if */

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
 * Changes:     Modified function to skip over entries with the 
 *		flush_in_progress flag set.  If this is not done,
 *		an infinite recursion is possible if the cache is 
 *		full, and the pre-serialize or serialize routine 
 *		attempts to load another entry.
 *
 *		This error was exposed by a re-factor of the 
 *		H5C__flush_single_entry() routine.  However, it was 
 *		a potential bug from the moment that entries were 
 *		allowed to load other entries on flush.
 *
 *		In passing, note that the primary and secondary dxpls 
 *		mentioned in the comment above have been replaced by 
 *		a single dxpl at some point, and thus the discussion 
 *		above is somewhat obsolete.  Date of this change is 
 *		unkown.
 *
 *						JRM -- 12/26/14
 *
 *		Modified function to detect deletions of entries 
 *		during a scan of the LRU, and where appropriate, 
 *		restart the scan to avoid proceeding with a next 
 *		entry that is no longer in the cache.
 *
 *		Note the absence of checks after flushes of clean 
 *		entries.  As a second entry can only be removed by 
 *		by a call to the pre_serialize or serialize callback
 *		of the first, and as these callbacks will not be called
 *		on clean entries, no checks are needed.
 *
 *						JRM -- 4/6/15
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_make_space_in_cache(H5F_t *	f,
                        hid_t	dxpl_id,
		        size_t	space_needed,
                        hbool_t	write_permitted)
{
    H5C_t *		cache_ptr = f->shared->cache;
#if H5C_COLLECT_CACHE_STATS
    int32_t             clean_entries_skipped = 0;
    int32_t             total_entries_scanned = 0;
#endif /* H5C_COLLECT_CACHE_STATS */
    int32_t		entries_examined = 0;
    int32_t		initial_list_len;
    size_t		empty_space;
    hbool_t		prev_is_dirty = FALSE;
    hbool_t             didnt_flush_entry = FALSE;
    hbool_t		restart_scan;
    H5C_cache_entry_t *	entry_ptr;
    H5C_cache_entry_t *	prev_ptr;
    H5C_cache_entry_t *	next_ptr;
    int32_t 		num_corked_entries = 0;
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( f );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->index_size ==
	      (cache_ptr->clean_index_size + cache_ptr->dirty_index_size) );

    if ( write_permitted ) {

        restart_scan = FALSE;
        initial_list_len = cache_ptr->LRU_list_len;
        entry_ptr = cache_ptr->LRU_tail_ptr;

	if ( cache_ptr->index_size >= cache_ptr->max_cache_size ) {

	   empty_space = 0;

	} else {

	   empty_space = cache_ptr->max_cache_size - cache_ptr->index_size;

	}

        while ( ( ( (cache_ptr->index_size + space_needed)
                    >
                    cache_ptr->max_cache_size
                  )
		  ||
		  (
		    ( empty_space + cache_ptr->clean_index_size )
		    <
		    ( cache_ptr->min_clean_size )
                  )
		)
                &&
                ( entries_examined <= (2 * initial_list_len) )
                &&
                ( entry_ptr != NULL )
              )
        {
	    HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
            HDassert( !(entry_ptr->is_protected) );
            HDassert( ! (entry_ptr->is_read_only) );
            HDassert( (entry_ptr->ro_ref_count) == 0 );

	    next_ptr = entry_ptr->next;
            prev_ptr = entry_ptr->prev;

	    if ( prev_ptr != NULL ) {

		prev_is_dirty = prev_ptr->is_dirty;
	    }

	    if (entry_ptr->is_corked && entry_ptr->is_dirty) {

                /* Skip "dirty" corked entries.  */
		++num_corked_entries;
                didnt_flush_entry = TRUE;

	    } else if ( ( (entry_ptr->type)->id != H5C__EPOCH_MARKER_TYPE ) &&
                 ( ! entry_ptr->flush_in_progress ) ) {

                didnt_flush_entry = FALSE;

                if ( entry_ptr->is_dirty ) {

#if H5C_COLLECT_CACHE_STATS
                    if ( (cache_ptr->index_size + space_needed)
                           >
                          cache_ptr->max_cache_size ) {

                        cache_ptr->entries_scanned_to_make_space++;
                    }
#endif /* H5C_COLLECT_CACHE_STATS */

		    /* reset entries_removed_counter and 
                     * last_entry_removed_ptr prior to the call to 
                     * H5C__flush_single_entry() so that we can spot 
                     * unexpected removals of entries from the cache,
                     * and set the restart_scan flag if proceeding
                     * would be likely to cause us to scan an entry
                     * that is no longer in the cache.
                     */
                    cache_ptr->entries_removed_counter = 0;
                    cache_ptr->last_entry_removed_ptr  = NULL;

#ifdef H5_HAVE_PARALLEL
                    if(TRUE == entry_ptr->coll_access) {
                        entry_ptr->coll_access = FALSE;
                        H5C__REMOVE_FROM_COLL_LIST(cache_ptr, entry_ptr, FAIL)
                    } /* end if */
#endif

                    if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, H5C__NO_FLAGS_SET, NULL, NULL) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush entry")

		    if ( ( cache_ptr->entries_removed_counter > 1 ) ||
                         ( cache_ptr->last_entry_removed_ptr == prev_ptr ) )

                        restart_scan = TRUE;

                } else if ( (cache_ptr->index_size + space_needed) > cache_ptr->max_cache_size 
#ifdef H5_HAVE_PARALLEL
                            && !(entry_ptr->coll_access)
#endif /* H5_HAVE_PARALLEL */
                            ) {
#if H5C_COLLECT_CACHE_STATS
                    cache_ptr->entries_scanned_to_make_space++;
#endif /* H5C_COLLECT_CACHE_STATS */

                    if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, H5C__FLUSH_INVALIDATE_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG, NULL, NULL) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush entry")
                } else {
                    /* We have enough space so don't flush clean entry. */
#if H5C_COLLECT_CACHE_STATS
                    clean_entries_skipped++;
#endif /* H5C_COLLECT_CACHE_STATS */
                    didnt_flush_entry = TRUE;
                }

#if H5C_COLLECT_CACHE_STATS
                total_entries_scanned++;
#endif /* H5C_COLLECT_CACHE_STATS */

            } else {

                /* Skip epoch markers and entries that are in the process
                 * of being flushed.
                 */
                didnt_flush_entry = TRUE;
            }

	    if ( prev_ptr != NULL ) {

		if ( didnt_flush_entry ) {

		    /* epoch markers don't get flushed, and we don't touch
                     * entries that are in the process of being flushed.
                     * Hence no need for sanity checks, as we haven't
                     * flushed anything.  Thus just set entry_ptr to prev_ptr
                     * and go on.
		     */
                    entry_ptr = prev_ptr;

		} else if ( ( restart_scan )
                            ||
                            ( prev_ptr->is_dirty != prev_is_dirty )
		            ||
		            ( prev_ptr->next != next_ptr )
		            ||
		            ( prev_ptr->is_protected )
		            ||
		            ( prev_ptr->is_pinned ) ) {

		    /* something has happened to the LRU -- start over
		     * from the tail.
		     */
                    restart_scan = FALSE;
	            entry_ptr = cache_ptr->LRU_tail_ptr;
		    H5C__UPDATE_STATS_FOR_LRU_SCAN_RESTART(cache_ptr)

		} else {

		    entry_ptr = prev_ptr;

		}
	    } else {

		entry_ptr = NULL;

	    }

	    entries_examined++;

	    if ( cache_ptr->index_size >= cache_ptr->max_cache_size ) {

	       empty_space = 0;

	    } else {

	       empty_space = cache_ptr->max_cache_size - cache_ptr->index_size;

	    }

	    HDassert( cache_ptr->index_size ==
	              (cache_ptr->clean_index_size +
		       cache_ptr->dirty_index_size) );

	}

#if H5C_COLLECT_CACHE_STATS
        cache_ptr->calls_to_msic++;

        cache_ptr->total_entries_skipped_in_msic += clean_entries_skipped;
        cache_ptr->total_entries_scanned_in_msic += total_entries_scanned;

        if ( clean_entries_skipped > cache_ptr->max_entries_skipped_in_msic ) {

            cache_ptr->max_entries_skipped_in_msic = clean_entries_skipped;
        }

        if ( total_entries_scanned > cache_ptr->max_entries_scanned_in_msic ) {

            cache_ptr->max_entries_scanned_in_msic = total_entries_scanned;
        }
#endif /* H5C_COLLECT_CACHE_STATS */


	/* NEED: work on a better assert for corked entries */
	HDassert( ( entries_examined > (2 * initial_list_len) ) ||
		  ( (cache_ptr->pl_size + cache_ptr->pel_size + cache_ptr->min_clean_size) >
		    cache_ptr->max_cache_size ) ||
		  ( ( cache_ptr->clean_index_size + empty_space )
		    >= cache_ptr->min_clean_size ) ||
		  ( ( num_corked_entries )));
#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

        HDassert( ( entries_examined > (2 * initial_list_len) ) ||
		  ( cache_ptr->cLRU_list_size <= cache_ptr->clean_index_size ) );
        HDassert( ( entries_examined > (2 * initial_list_len) ) ||
		  ( cache_ptr->dLRU_list_size <= cache_ptr->dirty_index_size ) );

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    } else {

        HDassert( H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS );

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
            HDassert( ! (entry_ptr->is_read_only) );
            HDassert( (entry_ptr->ro_ref_count) == 0 );
            HDassert( ! (entry_ptr->is_dirty) );

            prev_ptr = entry_ptr->aux_prev;

#ifdef H5_HAVE_PARALLEL
            if(!(entry_ptr->coll_access)) {
#endif /* H5_HAVE_PARALLEL */
                if(H5C__flush_single_entry(f, dxpl_id, entry_ptr, H5C__FLUSH_INVALIDATE_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush entry")
#ifdef H5_HAVE_PARALLEL
            } /* end if */
#endif /* H5_HAVE_PARALLEL */

	    /* we are scanning the clean LRU, so the serialize function
	     * will not be called on any entry -- thus there is no
	     * concern about the list being modified out from under
	     * this function.
	     */

            entry_ptr = prev_ptr;
	    entries_examined++;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_make_space_in_cache() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_validate_lru_list
 *
 * Purpose:     Debugging function that scans the LRU list for errors.
 *
 *		If an error is detected, the function generates a
 *		diagnostic and returns FAIL.  If no error is detected,
 *		the function returns SUCCEED.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 7/14/05
 *
 * Changes:
 *
 *		Added code to verify that the LRU contains no pinned 
 *		entries.                        JRM -- 4/25/14
 *
 *-------------------------------------------------------------------------
 */
#if H5C_DO_EXTREME_SANITY_CHECKS

static herr_t
H5C_validate_lru_list(H5C_t * cache_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */
    int32_t             len = 0;
    size_t              size = 0;
    H5C_cache_entry_t *	entry_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( ( ( cache_ptr->LRU_head_ptr == NULL )
           ||
           ( cache_ptr->LRU_tail_ptr == NULL )
         )
         &&
         ( cache_ptr->LRU_head_ptr != cache_ptr->LRU_tail_ptr )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 1 failed")
    }

    if(cache_ptr->LRU_list_len < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 2 failed")

    if ( ( cache_ptr->LRU_list_len == 1 )
         &&
         ( ( cache_ptr->LRU_head_ptr != cache_ptr->LRU_tail_ptr )
           ||
           ( cache_ptr->LRU_head_ptr == NULL )
           ||
           ( cache_ptr->LRU_head_ptr->size != cache_ptr->LRU_list_size )
         )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 3 failed")
    }

    if ( ( cache_ptr->LRU_list_len >= 1 )
         &&
         ( ( cache_ptr->LRU_head_ptr == NULL )
           ||
           ( cache_ptr->LRU_head_ptr->prev != NULL )
           ||
           ( cache_ptr->LRU_tail_ptr == NULL )
           ||
           ( cache_ptr->LRU_tail_ptr->next != NULL )
         )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 4 failed")
    }

    entry_ptr = cache_ptr->LRU_head_ptr;
    while ( entry_ptr != NULL )
    {

        if ( ( entry_ptr != cache_ptr->LRU_head_ptr ) &&
             ( ( entry_ptr->prev == NULL ) ||
               ( entry_ptr->prev->next != entry_ptr ) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 5 failed")
        }

        if ( ( entry_ptr != cache_ptr->LRU_tail_ptr ) &&
             ( ( entry_ptr->next == NULL ) ||
               ( entry_ptr->next->prev != entry_ptr ) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 6 failed")
        }

        if ( ( entry_ptr->is_pinned ) || 
             ( entry_ptr->pinned_from_client ) ||
             ( entry_ptr->pinned_from_cache ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 7 failed")
        }

        len++;
        size += entry_ptr->size;
        entry_ptr = entry_ptr->next;
    }

    if ( ( cache_ptr->LRU_list_len != len ) ||
         ( cache_ptr->LRU_list_size != size ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 8 failed")
    }

done:

    if ( ret_value != SUCCEED ) {

        HDassert(0);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_validate_lru_list() */

#endif /* H5C_DO_EXTREME_SANITY_CHECKS */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_validate_pinned_entry_list
 *
 * Purpose:     Debugging function that scans the pinned entry list for 
 *              errors.
 *
 *		If an error is detected, the function generates a
 *		diagnostic and returns FAIL.  If no error is detected,
 *		the function returns SUCCEED.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 4/25/14
 *
 * Changes:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */
#if H5C_DO_EXTREME_SANITY_CHECKS

static herr_t
H5C_validate_pinned_entry_list(H5C_t * cache_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */
    int32_t             len = 0;
    size_t              size = 0;
    H5C_cache_entry_t *	entry_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( ( ( cache_ptr->pel_head_ptr == NULL )
           ||
           ( cache_ptr->pel_tail_ptr == NULL )
         )
         &&
         ( cache_ptr->pel_head_ptr != cache_ptr->pel_tail_ptr )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 1 failed")
    }

    if(cache_ptr->pel_len < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 2 failed")

    if ( ( cache_ptr->pel_len == 1 )
         &&
         ( ( cache_ptr->pel_head_ptr != cache_ptr->pel_tail_ptr )
           ||
           ( cache_ptr->pel_head_ptr == NULL )
           ||
           ( cache_ptr->pel_head_ptr->size != cache_ptr->pel_size )
         )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 3 failed")
    }

    if ( ( cache_ptr->pel_len >= 1 )
         &&
         ( ( cache_ptr->pel_head_ptr == NULL )
           ||
           ( cache_ptr->pel_head_ptr->prev != NULL )
           ||
           ( cache_ptr->pel_tail_ptr == NULL )
           ||
           ( cache_ptr->pel_tail_ptr->next != NULL )
         )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 4 failed")
    }

    entry_ptr = cache_ptr->pel_head_ptr;
    while ( entry_ptr != NULL )
    {

        if ( ( entry_ptr != cache_ptr->pel_head_ptr ) &&
             ( ( entry_ptr->prev == NULL ) ||
               ( entry_ptr->prev->next != entry_ptr ) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 5 failed")
        }

        if ( ( entry_ptr != cache_ptr->pel_tail_ptr ) &&
             ( ( entry_ptr->next == NULL ) ||
               ( entry_ptr->next->prev != entry_ptr ) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 6 failed")
        }

        if ( ! entry_ptr->is_pinned ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 7 failed")
        }

        if ( ! ( ( entry_ptr->pinned_from_client ) ||
                 ( entry_ptr->pinned_from_cache ) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 8 failed")
        }

        len++;
        size += entry_ptr->size;
        entry_ptr = entry_ptr->next;
    }

    if ( ( cache_ptr->pel_len != len ) ||
         ( cache_ptr->pel_size != size ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 9 failed")
    }

done:

    if ( ret_value != SUCCEED ) {

        HDassert(0);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_validate_pinned_entry_list() */

#endif /* H5C_DO_EXTREME_SANITY_CHECKS */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_validate_protected_entry_list
 *
 * Purpose:     Debugging function that scans the protected entry list for 
 *              errors.
 *
 *		If an error is detected, the function generates a
 *		diagnostic and returns FAIL.  If no error is detected,
 *		the function returns SUCCEED.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 4/25/14
 *
 * Changes:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */
#if H5C_DO_EXTREME_SANITY_CHECKS

static herr_t
H5C_validate_protected_entry_list(H5C_t * cache_ptr)
{
    herr_t		ret_value = SUCCEED;      /* Return value */
    int32_t             len = 0;
    size_t              size = 0;
    H5C_cache_entry_t *	entry_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if(((cache_ptr->pl_head_ptr == NULL) || (cache_ptr->pl_tail_ptr == NULL))
             && (cache_ptr->pl_head_ptr != cache_ptr->pl_tail_ptr))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 1 failed")

    if(cache_ptr->pl_len < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 2 failed")

    if ( ( cache_ptr->pl_len == 1 )
         &&
         ( ( cache_ptr->pl_head_ptr != cache_ptr->pl_tail_ptr )
           ||
           ( cache_ptr->pl_head_ptr == NULL )
           ||
           ( cache_ptr->pl_head_ptr->size != cache_ptr->pl_size )
         )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 3 failed")
    }

    if ( ( cache_ptr->pl_len >= 1 )
         &&
         ( ( cache_ptr->pl_head_ptr == NULL )
           ||
           ( cache_ptr->pl_head_ptr->prev != NULL )
           ||
           ( cache_ptr->pl_tail_ptr == NULL )
           ||
           ( cache_ptr->pl_tail_ptr->next != NULL )
         )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 4 failed")
    }

    entry_ptr = cache_ptr->pl_head_ptr;
    while ( entry_ptr != NULL )
    {

        if ( ( entry_ptr != cache_ptr->pl_head_ptr ) &&
             ( ( entry_ptr->prev == NULL ) ||
               ( entry_ptr->prev->next != entry_ptr ) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 5 failed")
        }

        if ( ( entry_ptr != cache_ptr->pl_tail_ptr ) &&
             ( ( entry_ptr->next == NULL ) ||
               ( entry_ptr->next->prev != entry_ptr ) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 6 failed")
        }

        if ( ! entry_ptr->is_protected ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 7 failed")
        }

        if ( ( entry_ptr->is_read_only ) &&
             ( entry_ptr->ro_ref_count <= 0 ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 8 failed")
        }

        len++;
        size += entry_ptr->size;
        entry_ptr = entry_ptr->next;
    }

    if ( ( cache_ptr->pl_len != len ) ||
         ( cache_ptr->pl_size != size ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Check 9 failed")
    }

done:

    if ( ret_value != SUCCEED ) {

        HDassert(0);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_validate_protected_entry_list() */

#endif /* H5C_DO_EXTREME_SANITY_CHECKS */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_entry_in_skip_list
 *
 * Purpose:     Debugging function that scans skip list to see if it 
 *		is in present.  We need this, as it is possible for 
 *		an entry to be in the skip list twice.
 *
 * Return:      FALSE if the entry is not in the skip list, and TRUE 
 *		if it is.
 *
 * Programmer:  John Mainzer, 11/1/14
 *
 * Changes:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */
#if H5C_DO_SLIST_SANITY_CHECKS

static hbool_t
H5C_entry_in_skip_list(H5C_t * cache_ptr, H5C_cache_entry_t *target_ptr)
{
    hbool_t in_slist              = FALSE;
    H5SL_node_t *       node_ptr  = NULL;
    H5C_cache_entry_t *	entry_ptr = NULL;

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->slist_ptr );

    node_ptr = H5SL_first(cache_ptr->slist_ptr);

    while ( ( node_ptr != NULL ) && ( ! in_slist ) )
    {
        entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

	HDassert( entry_ptr );
	HDassert( entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC );
        HDassert( entry_ptr->is_dirty );
        HDassert( entry_ptr->in_slist );

        if ( entry_ptr == target_ptr ) {

	    in_slist = TRUE;

	} else {

	    node_ptr = H5SL_next(node_ptr);
	}
    }

    return(in_slist);

} /* H5C_entry_in_skip_list() */

#endif /* H5C_DO_SLIST_SANITY_CHECKS */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_get_entry_ptr_from_addr()
 *
 * Purpose:     Debugging function that attempts to look up an entry in the 
 *		cache by its file address, and if found, returns a pointer 
 *		to the entry in *entry_ptr_ptr.  If the entry is not in the 
 *		cache, *entry_ptr_ptr is set to NULL.
 *
 *		WARNING: This call should be used only in debugging  
 *			 routines, and it should be avoided when 
 *			 possible.
 *
 *			 Further, if we ever multi-thread the cache, 
 *			 this routine will have to be either discarded 
 *			 or heavily re-worked.
 *
 *			 Finally, keep in mind that the entry whose 
 *			 pointer is obtained in this fashion may not 
 *			 be in a stable state.  
 *
 *		Note that this function is only defined if NDEBUG
 *		is not defined.
 *
 *		As heavy use of this function is almost certainly a 
 *		bad idea, the metadata cache tracks the number of 
 *		successful calls to this function, and (if 
 *              H5C_DO_SANITY_CHECKS is defined) displays any 
 *		non-zero count on cache shutdown.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 5/30/14
 *
 * Changes:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5C_get_entry_ptr_from_addr(const H5F_t *f,
                            haddr_t   addr,
			    void ** entry_ptr_ptr)
{
    H5C_t             * cache_ptr;
    H5C_cache_entry_t * entry_ptr = NULL;
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( f );
    HDassert( f->shared );

    cache_ptr = f->shared->cache;

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( H5F_addr_defined(addr) );
    HDassert( entry_ptr_ptr != NULL );

    /* this test duplicates two of the above asserts, but we need an
     * invocation of HGOTO_ERROR to keep the compiler happy.
     */
    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)

    if ( entry_ptr == NULL ) {

        /* the entry doesn't exist in the cache -- report this
         * and quit.
         */
        *entry_ptr_ptr = NULL;

    } else {

        *entry_ptr_ptr = entry_ptr;

	/* increment call counter */
	(cache_ptr->get_entry_ptr_from_addr_counter)++;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_entry_ptr_from_addr() */

#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_verify_entry_type()
 *
 * Purpose:     Debugging function that attempts to look up an entry in the 
 *		cache by its file address, and if found, test to see if its
 *		type field contains the expted value.
 *
 *		If the specified entry is in cache, *in_cache_ptr is set
 *		to TRUE, and *type_ok_ptr is set to TRUE or FALSE
 *		depending on whether the entries type field matches the 
 *		expected_type parameter
 *
 *		If the target entry is not in cache, *in_cache_ptr is 
 *		set to FALSE, and *type_ok_ptr is undefined.
 *
 *		Note that this function is only defined if NDEBUG
 *		is not defined.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 5/30/14
 *
 * Changes:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5C_verify_entry_type(const H5F_t *f,
                      haddr_t   addr,
                      const H5C_class_t * expected_type,
                      hbool_t * in_cache_ptr,
                      hbool_t * type_ok_ptr)
{
    H5C_t             * cache_ptr;
    H5C_cache_entry_t * entry_ptr = NULL;
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( f );
    HDassert( f->shared );

    cache_ptr = f->shared->cache;

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( H5F_addr_defined(addr) );
    HDassert( in_cache_ptr != NULL );
    HDassert( type_ok_ptr != NULL );

    /* this test duplicates two of the above asserts, but we need an
     * invocation of HGOTO_ERROR to keep the compiler happy.
     */
    if ( ( cache_ptr == NULL ) || ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr on entry.")
    }

    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)

    if ( entry_ptr == NULL ) {

        /* the entry doesn't exist in the cache -- report this
         * and quit.
         */
        *in_cache_ptr = FALSE;

    } else {

        *in_cache_ptr = TRUE;
	*type_ok_ptr = (expected_type == entry_ptr->type);
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_verify_entry_type() */

#endif /* NDEBUG */


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
 * Function:    H5C_tag_entry
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
static herr_t
H5C_tag_entry(H5C_t * cache_ptr, H5C_cache_entry_t * entry_ptr, hid_t dxpl_id)
{
    H5P_genplist_t *dxpl;       /* dataset transfer property list */
    H5C_tag_t tag;              /* Tag structure */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

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
        if(H5C_verify_tag(entry_ptr->type->id, tag.value) < 0)
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
} /* H5C_tag_entry */


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
H5C_evict_tagged_entries(H5F_t * f, hid_t dxpl_id, haddr_t tag)
{
    /* Variable Declarations */
    H5C_t *cache_ptr = NULL;
    H5C_cache_entry_t * entry_ptr = NULL;
    H5C_cache_entry_t * next_entry_ptr = NULL;
    hbool_t evicted_entries_last_pass;
    hbool_t pinned_entries_need_evicted;
    int i;
    herr_t ret_value = SUCCEED;

    /* Function Enter Macro */
    FUNC_ENTER_NOAPI(FAIL)

    /* Assertions */
    HDassert(f);
    HDassert(f->shared);

    /* Get cache pointer */
    cache_ptr = f->shared->cache;

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    /* Start evicting entries */
    do {

	/* Reset pinned/evicted trackers */
	pinned_entries_need_evicted = FALSE;
	evicted_entries_last_pass = FALSE;

	/* Iterate through entries in the index. */
	for (i = 0; i < H5C__HASH_TABLE_LEN; i++) {

	    next_entry_ptr = cache_ptr->index[i];

	    while ( next_entry_ptr != NULL ) {

		entry_ptr = next_entry_ptr;
		next_entry_ptr = entry_ptr->ht_next;

		if(( entry_ptr->tag == tag ) ||
		    ( entry_ptr->globality == H5C_GLOBALITY_MAJOR)) {

		    /* This entry will need to be evicted */

		    if ( entry_ptr->is_protected ) {
			HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Cannot evict protected entry");
		    } else if (entry_ptr->is_dirty) {
			HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Cannot evict dirty entry");
		    } else if (entry_ptr->is_pinned) {
			
			/* Can't evict at this time, but let's note that we hit a pinned
			    entry and we'll loop back around again (as evicting other
			    entries will hopefully unpin this entry) */
    
			pinned_entries_need_evicted = TRUE;

		    } else {

			/* Evict the Entry */

			if(H5C__flush_single_entry(f, dxpl_id, entry_ptr,
						  H5C__FLUSH_INVALIDATE_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG,
						  NULL, NULL) < 0)

			    HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
					"Entry eviction failed.")

			evicted_entries_last_pass = TRUE;

		    } /* end if */

		} /* end if */

	    } /* end while */

	} /* end for */

    /* Keep doing this until we have stopped evicted entries */
    } while (evicted_entries_last_pass == TRUE);

    /* If we stop evicting entries and pinned entries still need evicted, 
       then we have a problem. */
    if (pinned_entries_need_evicted) {
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Pinned entries still need evicted?!");
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);

} /* H5C_evict_tagged_entries */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_mark_tagged_entries
 *
 * Purpose:     Set the flush marker on dirty entries in the cache that have
 *              the specified tag, as well as all globally tagged entries.
 *              If mark_clean is set, this function will also mark all clean
 *              entries, indicating they are to be evicted.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              September 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5C_mark_tagged_entries(H5C_t * cache_ptr, haddr_t tag, hbool_t mark_clean)
{
    /* Variable Declarations */
    int u;                          /* Iterator */
    H5C_cache_entry_t *entry_ptr = NULL; /* entry pointer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Assertions */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Iterate through entries, marking those with specified tag, as
     * well as any major global entries which should always be flushed
     * when flushing based on tag value */
    for(u = 0; u < H5C__HASH_TABLE_LEN; u++) {

	entry_ptr = cache_ptr->index[u];

	while ( entry_ptr != NULL ) {

	    if (( entry_ptr->tag == tag ) || 
		( entry_ptr->globality == H5C_GLOBALITY_MAJOR)) {
    
		/* We only want to set the flush marker on entries that
		 * actually need flushed (i.e., dirty ones), unless 
		 * we've specified otherwise with the mark_clean flag */
		if (entry_ptr->is_dirty || mark_clean) {  

		    entry_ptr->flush_marker = TRUE;

		} /* end if */

	    } /* end if */

	    entry_ptr = entry_ptr->ht_next;
	} /* end while */
    } /* for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5C_mark_tagged_entries */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_flush_marked_entries
 *
 * Purpose:     Flushes all marked entries in the cache.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              November 3, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C_flush_marked_entries(H5F_t * f, hid_t dxpl_id)
{ 
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Assertions */
    HDassert(f != NULL);

    /* Flush all marked entries */
    if(H5C_flush_cache(f, dxpl_id, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_IGNORE_PROTECTED_FLAG) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush cache")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_marked_entries */

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
static herr_t
H5C_verify_tag(int id, haddr_t tag) 
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
        }
        else {
            if(tag == H5AC__SUPERBLOCK_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__SUPERBLOCK_TAG applied to non-superblock entry")
        }
    
        /* Free Space Manager */
        if((id == H5AC_FSPACE_HDR_ID) || (id == H5AC_FSPACE_SINFO_ID)) {
            if(tag != H5AC__FREESPACE_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "freespace entry not tagged with H5AC__FREESPACE_TAG")
        }
        else {
            if(tag == H5AC__FREESPACE_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__FREESPACE_TAG applied to non-freespace entry")
        }
    
        /* SOHM */
        if((id == H5AC_SOHM_TABLE_ID) || (id == H5AC_SOHM_LIST_ID)) { 
            if(tag != H5AC__SOHM_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "sohm entry not tagged with H5AC__SOHM_TAG")
        }
    
        /* Global Heap */
        if(id == H5AC_GHEAP_ID) {
            if(tag != H5AC__GLOBALHEAP_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "global heap not tagged with H5AC__GLOBALHEAP_TAG")
        }
        else {
            if(tag == H5AC__GLOBALHEAP_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__GLOBALHEAP_TAG applied to non-globalheap entry")
        }
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
    if(H5C_mark_tagged_entries(cache_ptr, tag, FALSE) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't mark tagged entries")

    /* Flush all marked entries */
    if(H5C_flush_marked_entries(f, dxpl_id) < 0)
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
    H5C_cache_entry_t *entry_ptr = NULL; /* entry pointer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Iterate through entries, retagging those with the src_tag tag */
    for(u = 0; u < H5C__HASH_TABLE_LEN; u++) {
	entry_ptr = cache_ptr->index[u];
	while(entry_ptr != NULL) {
	    if(cache_ptr->index[u] != NULL)
		if((cache_ptr->index[u])->tag == src_tag) {
		    (cache_ptr->index[u])->tag = dest_tag;
		}
	    entry_ptr = entry_ptr->ht_next;
	} /* end while */
    } /* end for */

    FUNC_LEAVE_NOAPI_VOID
} /* H5C_retag_entries */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_entry_ring
 *
 * Purpose:     Given a file address, retrieve the ring for an entry at that
 *              address.
 *
 * 		On error, the value of *ring is not modified.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              9/8/15
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_get_entry_ring(const H5F_t *f, haddr_t addr, H5C_ring_t *ring)
{
    H5C_t *cache_ptr;                   /* Pointer to cache */
    H5C_cache_entry_t *entry_ptr;       /* Pointer to cache entry at address */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(H5F_addr_defined(addr));

    /* Locate the entry at the address */
    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)
    HDassert(entry_ptr);

    /* Return the ring value */
    *ring = entry_ptr->ring;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_get_entry_ring() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_cork
 *
 * Purpose:     To cork/uncork/get cork status of an object depending on "action":
 *		H5C__SET_CORK: 
 *			To cork the object
 *			Return error if the object is already corked
 *		H5C__UNCORK:
 *			To uncork the obejct
 *			Return error if the object is not corked
 * 		H5C__GET_CORKED:
 *			To retrieve the cork status of an object in
 *			the parameter "corked"
 *		
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Vailin Choi; January 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_cork(H5C_t * cache_ptr, haddr_t obj_addr, unsigned action, hbool_t *corked) 
{
    haddr_t *ptr;		/* Points to an address */
    herr_t ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Assertions */
    HDassert(cache_ptr != NULL);
    HDassert(H5F_addr_defined(obj_addr));
    HDassert(action == H5C__SET_CORK || action == H5C__UNCORK || action == H5C__GET_CORKED);

    /* Search the list of corked object addresses in the cache */
    ptr = (haddr_t *)H5SL_search(cache_ptr->cork_list_ptr, &obj_addr);

    if(H5C__GET_CORKED == action) {
        HDassert(corked);
        if(ptr != NULL && *ptr == obj_addr)
            *corked = TRUE;
        else
            *corked = FALSE;
    } /* end if */
    else {
        hbool_t is_corked;		/* Cork status for an entry */

        /* Sanity check */
        HDassert(H5C__SET_CORK == action || H5C__UNCORK == action);

        /* Perform appropriate action */
        if(H5C__SET_CORK == action) {
            haddr_t *addr_ptr = NULL;	/* Points to an address */

            if(ptr != NULL && *ptr == obj_addr)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't cork an already corked object")

            /* Allocate address */
            if(NULL == (addr_ptr = H5FL_MALLOC(haddr_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

            /* Insert into the list */
            *addr_ptr = obj_addr;
            if(H5SL_insert(cache_ptr->cork_list_ptr, addr_ptr, addr_ptr) < 0) {
                addr_ptr = H5FL_FREE(haddr_t, addr_ptr);
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't insert address into cork list")
            } /* end if */

            /* Set the entry's cork status */
            is_corked = TRUE;
        } /* end if */
        else {
            if(ptr == NULL)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't uncork an object that is not corked ")

            /* Remove the object address from the list */
            ptr = (haddr_t *)H5SL_remove(cache_ptr->cork_list_ptr, &obj_addr);
            if(ptr == NULL || *ptr != obj_addr)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't remove address from list")

            /* Free address */
            ptr = H5FL_FREE(haddr_t, ptr);

            /* Set the entry's cork status */
            is_corked = FALSE;
        } /* end else */

        /* Mark existing cache entries with tag (obj_addr) to the cork status */
        if(H5C_mark_tagged_entries_cork(cache_ptr, obj_addr, is_corked) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "can't mark cork status on entry")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_cork() */


/*-------------------------------------------------------------------------
 * Function:    H5C_mark_tagged_entries_cork
 *		
 *		NEED: work to combine with H5C_mark_tagged_entries()--
 *		      probably an action (FLUSH or CORK) with hbool_t clean_or_cork
 *
 * Purpose:     To set the "is_corked" field to "val" for entries in cache 
 *		with the entry's tag equals to "obj_addr".
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Vailin Choi; January 2014
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5C_mark_tagged_entries_cork(H5C_t *cache_ptr, haddr_t obj_addr, hbool_t val)
{
    /* Variable Declarations */
    int u;                          /* Iterator */
    H5C_cache_entry_t *entry_ptr = NULL; /* entry pointer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Assertions */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Iterate through entries, find each entry with the specified tag */
    /* and set the entry's "corked" field to "val" */
    for(u = 0; u < H5C__HASH_TABLE_LEN; u++) {

	entry_ptr = cache_ptr->index[u];

	while(entry_ptr != NULL) {

	    if(entry_ptr->tag == obj_addr)
		entry_ptr->is_corked = val;

	    entry_ptr = entry_ptr->ht_next;
	} /* end while */
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5C_mark_tagged_entries_cork */


/*-------------------------------------------------------------------------
 * Function:    H5C__mark_flush_dep_dirty()
 *
 * Purpose:     Recursively propagate the flush_dep_ndirty_children flag
 *              up the dependency chain in response to entry either
 *              becoming dirty or having its flush_dep_ndirty_children
 *              increased from 0.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              11/13/12
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__mark_flush_dep_dirty(H5C_cache_entry_t * entry)
{
    unsigned i;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity checks */
    HDassert(entry);
    HDassert((entry->is_dirty && entry->flush_dep_ndirty_children == 0)
	    || (!entry->is_dirty && entry->flush_dep_ndirty_children == 1));

    /* Iterate over the parent entries, if any */
    for(i=0; i<entry->flush_dep_nparents; i++) {
	/* Sanity check */
	HDassert(entry->flush_dep_parent[i]->flush_dep_ndirty_children
		< entry->flush_dep_parent[i]->flush_dep_nchildren);

	/* Adjust the parent's number of dirty children */
	entry->flush_dep_parent[i]->flush_dep_ndirty_children++;

	/* Propagate the flush dep dirty flag up the chain if necessary */
	if(!entry->flush_dep_parent[i]->is_dirty
		&& entry->flush_dep_parent[i]->flush_dep_ndirty_children == 1)
	    if(H5C__mark_flush_dep_dirty(entry->flush_dep_parent[i]) < 0)
		HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "can't propagate flush dep dirty flag")
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__mark_flush_dep_dirty() */


/*-------------------------------------------------------------------------
 * Function:    H5C__mark_flush_dep_clean()
 *
 * Purpose:     Recursively propagate the flush_dep_ndirty_children flag
 *              up the dependency chain in response to entry either
 *              becoming clean or having its flush_dep_ndirty_children
 *              reduced to 0.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              11/13/12
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__mark_flush_dep_clean(H5C_cache_entry_t * entry)
{
    unsigned i;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity checks */
    HDassert(entry);
    HDassert(!entry->is_dirty && entry->flush_dep_ndirty_children == 0);

    /* Iterate over the parent entries, if any */
    for(i=0; i<entry->flush_dep_nparents; i++) {
	/* Sanity check */
	HDassert(entry->flush_dep_parent[i]->flush_dep_ndirty_children > 0);

	/* Adjust the parent's number of dirty children */
	entry->flush_dep_parent[i]->flush_dep_ndirty_children--;

	/* Propagate the flush dep clean flag up the chain if necessary */
	if(!entry->flush_dep_parent[i]->is_dirty
		&& entry->flush_dep_parent[i]->flush_dep_ndirty_children == 0)
	    if(H5C__mark_flush_dep_clean(entry->flush_dep_parent[i]) < 0)
		HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "can't propagate flush dep clean flag")
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__mark_flush_dep_clean() */

#ifndef NDEBUG

/*-------------------------------------------------------------------------
 * Function:    H5C__assert_flush_dep_nocycle()
 *
 * Purpose:     Assert recursively that base_entry is not the same as
 *              entry, and perform the same assertion on all of entry's
 *              flush dependency parents.  This is used to detect cycles
 *              created by flush dependencies.
 *
 * Return:      void
 *
 * Programmer:  Neil Fortner
 *              12/10/12
 *
 *-------------------------------------------------------------------------
 */
static void
H5C__assert_flush_dep_nocycle(H5C_cache_entry_t * entry,
    H5C_cache_entry_t * base_entry)
{
    unsigned i;                         /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity checks */
    HDassert(entry);
    HDassert(base_entry);

    /* Make sure the entries are not the same */
    HDassert(base_entry != entry);

    /* Iterate over entry's parents (if any) */
    for(i=0; i<entry->flush_dep_nparents; i++)
	H5C__assert_flush_dep_nocycle(entry->flush_dep_parent[i], base_entry);

    FUNC_LEAVE_NOAPI_VOID
} /* H5C__assert_flush_dep_nocycle() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 * Function:    H5C__generate_image
 *
 * Purpose:     Serialize an entry and generate its image.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              2/10/16
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__generate_image(const H5F_t *f, H5C_t *cache_ptr, H5C_cache_entry_t *entry_ptr, 
    hid_t dxpl_id, int64_t *entry_size_change_ptr)
{
    haddr_t		new_addr = HADDR_UNDEF;
    haddr_t		old_addr = HADDR_UNDEF;
    size_t		new_len = 0;
    size_t		new_compressed_len = 0;
    unsigned            serialize_flags = H5C__SERIALIZE_NO_FLAGS_SET;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(!entry_ptr->image_up_to_date);

    /* reset cache_ptr->slist_changed so we can detect slist
     * modifications in the pre_serialize call.
     */
    cache_ptr->slist_changed = FALSE;

    /* make note of the entry's current address */
    old_addr = entry_ptr->addr;

    /* Call client's pre-serialize callback, if there's one */
    if(entry_ptr->type->pre_serialize && 
         (entry_ptr->type->pre_serialize)(f, dxpl_id, 
                (void *)entry_ptr, entry_ptr->addr, entry_ptr->size,
                entry_ptr->compressed_size, &new_addr, &new_len, 
                &new_compressed_len, &serialize_flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to pre-serialize entry")

    /* set cache_ptr->slist_change_in_pre_serialize if the 
     * slist was modified.
     */
    if(cache_ptr->slist_changed)
        cache_ptr->slist_change_in_pre_serialize = TRUE;

    /* Check for any flags set in the pre-serialize callback */
    if(serialize_flags != H5C__SERIALIZE_NO_FLAGS_SET) {
        /* Check for unexpected flags from serialize callback */
        if(serialize_flags & ~(H5C__SERIALIZE_RESIZED_FLAG | 
                               H5C__SERIALIZE_MOVED_FLAG |
                               H5C__SERIALIZE_COMPRESSED_FLAG))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unknown serialize flag(s)")

#ifdef H5_HAVE_PARALLEL
        /* In the parallel case, resizes and moves in
         * the serialize operation can cause problems.
         * If they occur, scream and die.
         *
         * At present, in the parallel case, the aux_ptr
         * will only be set if there is more than one
         * process.  Thus we can use this to detect
         * the parallel case.
         *
         * This works for now, but if we start using the
         * aux_ptr for other purposes, we will have to
         * change this test accordingly.
         *
         * NB: While this test detects entryies that attempt
         *     to resize or move themselves during a flush
         *     in the parallel case, it will not detect an
         *     entry that dirties, resizes, and/or moves
         *     other entries during its flush.
         *
         *     From what Quincey tells me, this test is
         *     sufficient for now, as any flush routine that
         *     does the latter will also do the former.
         *
         *     If that ceases to be the case, further
         *     tests will be necessary.
         */
        if(cache_ptr->aux_ptr != NULL)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "resize/move in serialize occured in parallel case.")
#endif

        /* Resize the buffer if required */
        if(((!entry_ptr->compressed) && (serialize_flags & H5C__SERIALIZE_RESIZED_FLAG)) ||
                ((entry_ptr->compressed) && (serialize_flags & H5C__SERIALIZE_COMPRESSED_FLAG))) {
            size_t new_image_size;

            if(entry_ptr->compressed)
                new_image_size = new_compressed_len;
            else
                new_image_size = new_len;
            HDassert(new_image_size > 0);

            /* Release the current image */
            if(entry_ptr->image_ptr)
                entry_ptr->image_ptr = H5MM_xfree(entry_ptr->image_ptr);

            /* Allocate a new image buffer */
            if(NULL == (entry_ptr->image_ptr = H5MM_malloc(new_image_size + H5C_IMAGE_EXTRA_SPACE)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "memory allocation failed for on disk image buffer")

#if H5C_DO_MEMORY_SANITY_CHECKS
            HDmemcpy(((uint8_t *)entry_ptr->image_ptr) + new_image_size, H5C_IMAGE_SANITY_VALUE, H5C_IMAGE_EXTRA_SPACE);
#endif /* H5C_DO_MEMORY_SANITY_CHECKS */
        } /* end if */

        /* If required, update the entry and the cache data structures
         * for a resize.
         */
        if(serialize_flags & H5C__SERIALIZE_RESIZED_FLAG) {
            H5C__UPDATE_STATS_FOR_ENTRY_SIZE_CHANGE(cache_ptr, entry_ptr, new_len);

            /* update the hash table for the size change*/
            H5C__UPDATE_INDEX_FOR_SIZE_CHANGE(cache_ptr, entry_ptr->size, \
                                              new_len, entry_ptr, !(entry_ptr->is_dirty));

            /* The entry can't be protected since we are
             * in the process of flushing it.  Thus we must
             * update the replacement policy data
             * structures for the size change.  The macro
             * deals with the pinned case.
             */
            H5C__UPDATE_RP_FOR_SIZE_CHANGE(cache_ptr, entry_ptr, new_len);

            /* as we haven't updated the cache data structures for 
             * for the flush or flush destroy yet, the entry should
             * be in the slist.  Thus update it for the size change.
             */
            HDassert(entry_ptr->in_slist);
            H5C__UPDATE_SLIST_FOR_SIZE_CHANGE(cache_ptr, entry_ptr->size, new_len);

            /* if defined, update *entry_size_change_ptr for the 
             * change in entry size.
             */
            if(entry_size_change_ptr != NULL)
                *entry_size_change_ptr = (int64_t)new_len - (int64_t)(entry_ptr->size);

            /* finally, update the entry for its new size */
            entry_ptr->size = new_len;
        } /* end if */

        /* If required, udate the entry and the cache data structures 
         * for a move 
         */
        if(serialize_flags & H5C__SERIALIZE_MOVED_FLAG) {
#if H5C_DO_SANITY_CHECKS
            int64_t saved_slist_len_increase;
            int64_t saved_slist_size_increase;
#endif /* H5C_DO_SANITY_CHECKS */

            H5C__UPDATE_STATS_FOR_MOVE(cache_ptr, entry_ptr);

            if(entry_ptr->addr == old_addr) {
                /* we must update cache data structures for the 
                 * change in address.
                 */

                /* delete the entry from the hash table and the slist */
                H5C__DELETE_FROM_INDEX(cache_ptr, entry_ptr);
                H5C__REMOVE_ENTRY_FROM_SLIST(cache_ptr, entry_ptr);

                /* update the entry for its new address */
                entry_ptr->addr = new_addr;

                /* and then reinsert in the index and slist */
                H5C__INSERT_IN_INDEX(cache_ptr, entry_ptr, FAIL);

#if H5C_DO_SANITY_CHECKS
                /* save cache_ptr->slist_len_increase and 
                 * cache_ptr->slist_size_increase before the 
                 * reinsertion into the slist, and restore 
                 * them afterwards to avoid skewing our sanity
                 * checking.
                 */
                saved_slist_len_increase = cache_ptr->slist_len_increase;
                saved_slist_size_increase = cache_ptr->slist_size_increase;
#endif /* H5C_DO_SANITY_CHECKS */

                H5C__INSERT_ENTRY_IN_SLIST(cache_ptr, entry_ptr, FAIL);

#if H5C_DO_SANITY_CHECKS
                cache_ptr->slist_len_increase = saved_slist_len_increase;
                cache_ptr->slist_size_increase = saved_slist_size_increase;
#endif /* H5C_DO_SANITY_CHECKS */
            } /* end if */
            else /* move is already done for us -- just do sanity checks */
                HDassert(entry_ptr->addr == new_addr);
        } /* end if */

        if(serialize_flags & H5C__SERIALIZE_COMPRESSED_FLAG) {
            /* just save the new compressed entry size in 
             * entry_ptr->compressed_size.  We don't need to 
             * do more, as compressed size is only used for I/O.
             */
            HDassert(entry_ptr->compressed);
            entry_ptr->compressed_size = new_compressed_len;
        } /* end if */
    } /* end if(serialize_flags != H5C__SERIALIZE_NO_FLAGS_SET) */

    /* Serialize object into buffer */
    {
        size_t image_len;

        if(entry_ptr->compressed)
            image_len = entry_ptr->compressed_size;
        else
            image_len = entry_ptr->size;

        /* reset cache_ptr->slist_changed so we can detect slist
         * modifications in the serialize call.
         */
        cache_ptr->slist_changed = FALSE;
            
        if(entry_ptr->type->serialize(f, entry_ptr->image_ptr, image_len, (void *)entry_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to serialize entry")

        /* set cache_ptr->slist_change_in_serialize if the 
         * slist was modified.
         */
        if(cache_ptr->slist_changed)
            cache_ptr->slist_change_in_pre_serialize = TRUE;

#if H5C_DO_MEMORY_SANITY_CHECKS
        HDassert(0 == HDmemcmp(((uint8_t *)entry_ptr->image_ptr) + image_len, 
                               H5C_IMAGE_SANITY_VALUE, H5C_IMAGE_EXTRA_SPACE));
#endif /* H5C_DO_MEMORY_SANITY_CHECKS */

        entry_ptr->image_up_to_date = TRUE;
    } /* end block */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__generate_image */

