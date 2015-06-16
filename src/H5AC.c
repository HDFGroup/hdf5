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
 * Created:             H5AC.c
 *                      Jul  9 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Functions in this file implement a cache for
 *                      things which exist on disk.  All "things" associated
 *                      with a particular HDF file share the same cache; each
 *                      HDF file has it's own cache.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5AC_PACKAGE            /*suppress error about including H5ACpkg  */
#define H5C_PACKAGE             /*suppress error about including H5Cpkg   */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5AC_init_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACpkg.h"		/* Metadata cache			*/
#include "H5Cpkg.h"             /* Cache                                */
#include "H5Dprivate.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"        /* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Pprivate.h"         /* Property lists                       */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

#ifdef H5_HAVE_PARALLEL
/****************************************************************************
 *
 * structure H5AC_slist_entry_t
 *
 * The dirty entry list maintained via the d_slist_ptr field of H5AC_aux_t
 * and the cleaned entry list maintained via the c_slist_ptr field of
 * H5AC_aux_t are just lists of the file offsets of the dirty/cleaned
 * entries.  Unfortunately, the slist code makes us define a dynamically
 * allocated structure to store these offsets in.  This structure serves
 * that purpose.  Its fields are as follows:
 *
 * addr:	file offset of a metadata entry.  Entries are added to this
 *		list (if they aren't there already) when they are marked
 *		dirty in an unprotect, inserted, or moved.  They are
 *		removed when they appear in a clean entries broadcast.
 *
 ****************************************************************************/
typedef struct H5AC_slist_entry_t
{
    haddr_t     addr;
} H5AC_slist_entry_t;

/* User data for address list building callbacks */
typedef struct H5AC_addr_list_ud_t
{
    H5AC_aux_t    * aux_ptr;        /* 'Auxiliary' parallel cache info */
    haddr_t       * addr_buf_ptr;   /* Array to store addresses */
    int             i;              /* Counter for position in array */
} H5AC_addr_list_ud_t;
#endif /* H5_HAVE_PARALLEL */


/********************/
/* Local Prototypes */
/********************/

static herr_t H5AC__check_if_write_permitted(const H5F_t *f,
    hbool_t *write_permitted_ptr);
static herr_t H5AC__ext_config_2_int_config(H5AC_cache_config_t *ext_conf_ptr,
    H5C_auto_size_ctl_t *int_conf_ptr);

#ifdef H5_HAVE_PARALLEL
static herr_t H5AC__broadcast_candidate_list(H5AC_t *cache_ptr,
    int *num_entries_ptr, haddr_t **haddr_buf_ptr_ptr);
static herr_t H5AC__broadcast_clean_list(H5AC_t *cache_ptr);
static herr_t H5AC__construct_candidate_list(H5AC_t *cache_ptr,
    H5AC_aux_t *aux_ptr, int sync_point_op);
static herr_t H5AC__copy_candidate_list_to_buffer(const H5AC_t *cache_ptr,
    int *num_entries_ptr, haddr_t **haddr_buf_ptr_ptr);
static herr_t H5AC__flush_entries(H5F_t *f, hid_t dxpl_id);
static herr_t H5AC__log_deleted_entry(const H5AC_info_t *entry_ptr);
static herr_t H5AC__log_dirtied_entry(const H5AC_info_t *entry_ptr);
static herr_t H5AC__log_flushed_entry(H5C_t *cache_ptr, haddr_t addr,
    hbool_t was_dirty, unsigned flags);
static herr_t H5AC__log_inserted_entry(const H5AC_info_t *entry_ptr);
static herr_t H5AC__log_moved_entry(const H5F_t *f, haddr_t old_addr,
    haddr_t new_addr);
static herr_t H5AC__propagate_and_apply_candidate_list(H5F_t  *f, hid_t dxpl_id);
static herr_t H5AC__propagate_flushed_and_still_clean_entries_list(H5F_t  *f,
    hid_t dxpl_id);
static herr_t H5AC__receive_haddr_list(MPI_Comm mpi_comm, int *num_entries_ptr,
    haddr_t **haddr_buf_ptr_ptr);
static herr_t H5AC__receive_candidate_list(const H5AC_t *cache_ptr,
    int *num_entries_ptr, haddr_t **haddr_buf_ptr_ptr);
static herr_t H5AC__receive_and_apply_clean_list(H5F_t *f, hid_t primary_dxpl_id,
    hid_t secondary_dxpl_id);
static herr_t H5AC__tidy_cache_0_lists(H5AC_t *cache_ptr, int num_candidates,
    haddr_t *candidates_list_ptr);
static herr_t H5AC__rsp__dist_md_write__flush(H5F_t *f, hid_t dxpl_id);
static herr_t H5AC__rsp__dist_md_write__flush_to_min_clean(H5F_t *f, hid_t dxpl_id);
static herr_t H5AC__rsp__p0_only__flush(H5F_t *f, hid_t dxpl_id);
static herr_t H5AC__rsp__p0_only__flush_to_min_clean(H5F_t *f, hid_t dxpl_id);
static herr_t H5AC__run_sync_point(H5F_t *f, hid_t dxpl_id, int sync_point_op);
#endif /* H5_HAVE_PARALLEL */


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/

/* Default dataset transfer property list for metadata I/O calls */
/* (Collective set, "block before metadata write" set and "library internal" set) */
/* (Global variable definition, declaration is in H5ACprivate.h also) */
hid_t H5AC_dxpl_id = (-1);

/* Dataset transfer property list for independent metadata I/O calls */
/* (just "library internal" set - i.e. independent transfer mode) */
/* (Global variable definition, declaration is in H5ACprivate.h also) */
H5P_genplist_t *H5AC_ind_dxpl_g = NULL;
hid_t H5AC_ind_dxpl_id = (-1);


/*******************/
/* Local Variables */
/*******************/

#ifdef H5_HAVE_PARALLEL
/* Declare a free list to manage the H5AC_aux_t struct */
H5FL_DEFINE_STATIC(H5AC_aux_t);

/* Declare a free list to manage the H5AC_slist_entry_t struct */
H5FL_DEFINE_STATIC(H5AC_slist_entry_t);
#endif /* H5_HAVE_PARALLEL */

static const char *H5AC_entry_type_names[H5AC_NTYPES] =
{
    "B-tree nodes",
    "symbol table nodes",
    "local heap prefixes",
    "local heap data blocks",
    "global heaps",
    "object headers",
    "object header chunks",
    "v2 B-tree headers",
    "v2 B-tree internal nodes",
    "v2 B-tree leaf nodes",
    "fractal heap headers",
    "fractal heap direct blocks",
    "fractal heap indirect blocks",
    "free space headers",
    "free space sections",
    "shared OH message master table",
    "shared OH message index",
    "extensible array headers",
    "extensible array index blocks",
    "extensible array super blocks",
    "extensible array data blocks",
    "extensible array data block pages",
    "fixed array headers",
    "fixed array data block",
    "fixed array data block pages",
    "superblock",
    "test entry"	/* for testing only -- not used for actual files */
};



/*-------------------------------------------------------------------------
 * Function:	H5AC_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 18, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_init(void)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_init() */


/*-------------------------------------------------------------------------
 * Function:	H5AC_init_interface
 *
 * Purpose:	Initialize interface-specific information
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 18, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC_init_interface(void)
{
#ifdef H5_HAVE_PARALLEL
    H5P_genplist_t  *xfer_plist;    /* Dataset transfer property list object */
    unsigned coll_meta_write;       /* "collective metadata write" property value */
#endif /* H5_HAVE_PARALLEL */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5_HAVE_PARALLEL
    /* Sanity check */
    HDassert(H5P_CLS_DATASET_XFER_g != NULL);

    /* Get an ID for the collective H5AC dxpl */
    if((H5AC_dxpl_id = H5P_create_id(H5P_CLS_DATASET_XFER_g, FALSE)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "unable to register property list")

    /* Get the property list object */
    if (NULL == (xfer_plist = (H5P_genplist_t *)H5I_object(H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADATOM, FAIL, "can't get new property list object")

    /* Insert 'collective metadata write' property */
    coll_meta_write = 1;
    if(H5P_insert(xfer_plist, H5AC_COLLECTIVE_META_WRITE_NAME, H5AC_COLLECTIVE_META_WRITE_SIZE, &coll_meta_write,
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "can't insert metadata cache dxpl property")

    /* Get an ID for the independent H5AC dxpl */
    if((H5AC_ind_dxpl_id = H5P_create_id(H5P_CLS_DATASET_XFER_g, FALSE)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "unable to register property list")

    /* Get the property list object */
    if(NULL == (H5AC_ind_dxpl_g = (H5P_genplist_t *)H5I_object(H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADATOM, FAIL, "can't get new property list object")

    /* Insert 'collective metadata write' property */
    coll_meta_write = 0;
    if(H5P_insert(H5AC_ind_dxpl_g, H5AC_COLLECTIVE_META_WRITE_NAME, H5AC_COLLECTIVE_META_WRITE_SIZE, &coll_meta_write,
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "can't insert metadata cache dxpl property")
#else /* H5_HAVE_PARALLEL */
    /* Sanity check */
    HDassert(H5P_LST_DATASET_XFER_ID_g!=(-1));

    H5AC_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    H5AC_ind_dxpl_id = H5P_DATASET_XFER_DEFAULT;

    /* Get the property list objects for the IDs */
    if(NULL == (H5AC_ind_dxpl_g = (H5P_genplist_t *)H5I_object(H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADATOM, FAIL, "can't get property list object")
#endif /* H5_HAVE_PARALLEL */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5AC_term_interface
 *
 * Purpose:	Terminate this interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				affect other interfaces; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 18, 2002
 *
 *-------------------------------------------------------------------------
 */
int
H5AC_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
#ifdef H5_HAVE_PARALLEL
        if(H5AC_dxpl_id > 0 || H5AC_ind_dxpl_id > 0) {
            /* Indicate more work to do */
            n = 1; /* H5I */

            /* Close H5AC dxpl */
            if(H5I_dec_ref(H5AC_dxpl_id) < 0 || H5I_dec_ref(H5AC_ind_dxpl_id) < 0)
                H5E_clear_stack(NULL); /*ignore error*/
            else {
                /* Reset static IDs */
                H5AC_dxpl_id = (-1);
                H5AC_ind_dxpl_id = (-1);

                /* Reset interface initialization flag */
                H5_interface_initialize_g = 0;
            } /* end else */
        } /* end if */
        else {
#endif /* H5_HAVE_PARALLEL */
            /* Reset static IDs */
            H5AC_dxpl_id = (-1);
            H5AC_ind_dxpl_id = (-1);
#ifdef H5_HAVE_PARALLEL
        } /* end else */
#endif /* H5_HAVE_PARALLEL */

        /* Reset interface initialization flag */
        H5_interface_initialize_g = 0;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5AC_term_interface() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_create
 *
 * Purpose:     Initialize the cache just after a file is opened.  The
 *              SIZE_HINT is the number of cache slots desired.  If you
 *              pass an invalid value then H5AC_NSLOTS is used.  You can
 *              turn off caching by using 1 for the SIZE_HINT value.
 *
 * Return:      Success:        Number of slots actually used.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul  9 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_create(const H5F_t *f, H5AC_cache_config_t *config_ptr)
{
#ifdef H5_HAVE_PARALLEL
    char 	 prefix[H5C__PREFIX_LEN] = "";
    H5AC_aux_t * aux_ptr = NULL;
#endif /* H5_HAVE_PARALLEL */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    HDassert(f);
    HDassert(NULL == f->shared->cache);
    HDassert(config_ptr != NULL) ;
    HDcompile_assert(NELMTS(H5AC_entry_type_names) == H5AC_NTYPES);
    HDcompile_assert(H5C__MAX_NUM_TYPE_IDS == H5AC_NTYPES);

    if(H5AC_validate_config(config_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "Bad cache configuration")

#ifdef H5_HAVE_PARALLEL
    if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI)) {
        MPI_Comm	 mpi_comm;
        int		 mpi_rank;
        int	 	 mpi_size;

        if(MPI_COMM_NULL == (mpi_comm = H5F_mpi_get_comm(f)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI communicator")

        if((mpi_rank = H5F_mpi_get_rank(f)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get mpi rank")

        if((mpi_size = H5F_mpi_get_size(f)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get mpi size")

        if(NULL == (aux_ptr = H5FL_CALLOC(H5AC_aux_t)))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "Can't allocate H5AC auxilary structure.")

        aux_ptr->magic = H5AC__H5AC_AUX_T_MAGIC;
        aux_ptr->mpi_comm = mpi_comm;
        aux_ptr->mpi_rank = mpi_rank;
        aux_ptr->mpi_size = mpi_size;
        aux_ptr->write_permitted = FALSE;
        aux_ptr->dirty_bytes_threshold = H5AC__DEFAULT_DIRTY_BYTES_THRESHOLD;
        aux_ptr->dirty_bytes = 0;
        aux_ptr->metadata_write_strategy = H5AC__DEFAULT_METADATA_WRITE_STRATEGY;
#if H5AC_DEBUG_DIRTY_BYTES_CREATION
        aux_ptr->dirty_bytes_propagations = 0;
        aux_ptr->unprotect_dirty_bytes = 0;
        aux_ptr->unprotect_dirty_bytes_updates = 0;
        aux_ptr->insert_dirty_bytes = 0;
        aux_ptr->insert_dirty_bytes_updates = 0;
        aux_ptr->move_dirty_bytes = 0;
        aux_ptr->move_dirty_bytes_updates = 0;
#endif /* H5AC_DEBUG_DIRTY_BYTES_CREATION */
        aux_ptr->d_slist_ptr = NULL;
        aux_ptr->c_slist_ptr = NULL;
        aux_ptr->candidate_slist_ptr = NULL;
        aux_ptr->write_done = NULL;
        aux_ptr->sync_point_done = NULL;

        sprintf(prefix, "%d:", mpi_rank);

        if(mpi_rank == 0) {
            if(NULL == (aux_ptr->d_slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "can't create dirtied entry list.")

            if(NULL == (aux_ptr->c_slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "can't create cleaned entry list.")
        } /* end if */

        /* construct the candidate slist for all processes.
         * when the distributed strategy is selected as all processes
         * will use it in the case of a flush.
         */
        if(NULL == (aux_ptr->candidate_slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "can't create candidate entry list.")

        if(aux_ptr != NULL)
            if(aux_ptr->mpi_rank == 0)
                f->shared->cache = H5C_create(H5AC__DEFAULT_MAX_CACHE_SIZE,
                        H5AC__DEFAULT_MIN_CLEAN_SIZE, (H5AC_NTYPES - 1),
                        (const char **)H5AC_entry_type_names,
                        H5AC__check_if_write_permitted, TRUE, H5AC__log_flushed_entry,
                        (void *)aux_ptr);
            else
                f->shared->cache = H5C_create(H5AC__DEFAULT_MAX_CACHE_SIZE,
                        H5AC__DEFAULT_MIN_CLEAN_SIZE, (H5AC_NTYPES - 1),
                        (const char **)H5AC_entry_type_names,
                        H5AC__check_if_write_permitted, TRUE, NULL,
                        (void *)aux_ptr);
        else
            f->shared->cache = H5C_create(H5AC__DEFAULT_MAX_CACHE_SIZE,
                    H5AC__DEFAULT_MIN_CLEAN_SIZE, (H5AC_NTYPES - 1),
                    (const char **)H5AC_entry_type_names,
                    H5AC__check_if_write_permitted, TRUE, NULL, NULL);
    } /* end if */
    else {
#endif /* H5_HAVE_PARALLEL */
        /* The default max cache size and min clean size will frequently be
         * overwritten shortly by the subsequent set resize config call.
         *                                             -- JRM
         */
        f->shared->cache = H5C_create(H5AC__DEFAULT_MAX_CACHE_SIZE,
                H5AC__DEFAULT_MIN_CLEAN_SIZE, (H5AC_NTYPES - 1),
                (const char **)H5AC_entry_type_names,
                H5AC__check_if_write_permitted, TRUE, NULL, NULL);
#ifdef H5_HAVE_PARALLEL
    } /* end else */
#endif /* H5_HAVE_PARALLEL */

    if(NULL == f->shared->cache)
	HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "memory allocation failed")

#ifdef H5_HAVE_PARALLEL
    if(aux_ptr != NULL)
        if(H5C_set_prefix(f->shared->cache, prefix) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "H5C_set_prefix() failed")
#endif /* H5_HAVE_PARALLEL */

    if(H5AC_set_cache_auto_resize_config(f->shared->cache, config_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "auto resize configuration failed")

done:
#ifdef H5_HAVE_PARALLEL
    /* if there is a failure, try to tidy up the auxilary structure */
    if(ret_value < 0) {
        if(aux_ptr != NULL) {
            if(aux_ptr->d_slist_ptr != NULL)
                H5SL_close(aux_ptr->d_slist_ptr);
            if(aux_ptr->c_slist_ptr != NULL)
                H5SL_close(aux_ptr->c_slist_ptr);
            if(aux_ptr->candidate_slist_ptr != NULL)
                H5SL_close(aux_ptr->candidate_slist_ptr);
            aux_ptr->magic = 0;
            aux_ptr = H5FL_FREE(H5AC_aux_t, aux_ptr);
        } /* end if */
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_create() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_dest
 *
 * Purpose:     Flushes all data to disk and destroys the cache.
 *              This function fails if any object are protected since the
 *              resulting file might not be consistent.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul  9 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_dest(H5F_t *f, hid_t dxpl_id)
{
#ifdef H5_HAVE_PARALLEL
    H5AC_aux_t * aux_ptr = NULL;
#endif /* H5_HAVE_PARALLEL */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared->cache);

#if H5AC_DUMP_STATS_ON_CLOSE
    /* Dump debugging info */
    H5AC_stats(f);
#endif /* H5AC_DUMP_STATS_ON_CLOSE */

#if H5AC__TRACE_FILE_ENABLED
    if(H5AC_close_trace_file(f->shared->cache) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_close_trace_file() failed.")
#endif /* H5AC__TRACE_FILE_ENABLED */

#ifdef H5_HAVE_PARALLEL
    aux_ptr = (struct H5AC_aux_t *)(f->shared->cache->aux_ptr);
    if(aux_ptr)
        /* Sanity check */
        HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);

    /* Attempt to flush all entries from rank 0 & Bcast clean list to other ranks */
    if(H5AC__flush_entries(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush.")
#endif /* H5_HAVE_PARALLEL */

    /* Destroy the cache */
    if(H5C_dest(f, dxpl_id, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "can't destroy cache")
    f->shared->cache = NULL;

#ifdef H5_HAVE_PARALLEL
    if(aux_ptr != NULL) {
        if(aux_ptr->d_slist_ptr != NULL)
            H5SL_close(aux_ptr->d_slist_ptr);
        if(aux_ptr->c_slist_ptr != NULL)
            H5SL_close(aux_ptr->c_slist_ptr);
        if(aux_ptr->candidate_slist_ptr != NULL)
            H5SL_close(aux_ptr->candidate_slist_ptr);
        aux_ptr->magic = 0;
        aux_ptr = H5FL_FREE(H5AC_aux_t, aux_ptr);
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_expunge_entry
 *
 * Purpose:	Expunge the target entry from the cache without writing it
 * 		to disk even if it is dirty.  The entry must not be either
 * 		pinned or protected.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/30/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_expunge_entry(H5F_t *f, hid_t dxpl_id, const H5AC_class_t *type,
    haddr_t addr, unsigned flags)
{
#if H5AC__TRACE_FILE_ENABLED
    char                trace[128] = "";
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t  ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(type->clear);
    HDassert(type->dest);
    HDassert(H5F_addr_defined(addr));

#if H5AC__TRACE_FILE_ENABLED
{
    H5AC_t * cache_ptr = f->shared->cache;

    /* For the expunge entry call, only the addr, and type id are really
     * necessary in the trace file.  Write the return value to catch occult
     * errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        sprintf(trace, "%s 0x%lx %d", FUNC, (unsigned long)addr, (int)(type->id));
}
#endif /* H5AC__TRACE_FILE_ENABLED */

    if(H5C_expunge_entry(f, dxpl_id, H5AC_dxpl_id, type, addr, flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, FAIL, "H5C_expunge_entry() failed.")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_expunge_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_flush
 *
 * Purpose:	Flush (and possibly destroy) the metadata cache associated
 *		with the specified file.
 *
 *		If the cache contains protected entries, the function will
 *		fail, as protected entries cannot be flushed.  However
 *		all unprotected entries should be flushed before the
 *		function returns failure.
 *
 * Return:      Non-negative on success/Negative on failure if there was a
 *              request to flush all items and something was protected.
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul  9 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_flush(H5F_t *f, hid_t dxpl_id)
{
#if H5AC__TRACE_FILE_ENABLED
    char 	  trace[128] = "";
    FILE *	  trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t	  ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);

#if H5AC__TRACE_FILE_ENABLED
    /* For the flush, only the flags are really necessary in the trace file.
     * Write the result to catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
	sprintf(trace, "%s", FUNC);
#endif /* H5AC__TRACE_FILE_ENABLED */

#ifdef H5_HAVE_PARALLEL
    /* Attempt to flush all entries from rank 0 & Bcast clean list to other ranks */
    if(H5AC__flush_entries(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush.")
#endif /* H5_HAVE_PARALLEL */

    /* Flush the cache */
    /* (Again, in parallel - writes out the superblock) */
    if(H5C_flush_cache(f, dxpl_id, H5AC_dxpl_id, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush cache.")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
        HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_get_entry_status
 *
 * Purpose:     Given a file address, determine whether the metadata
 * 		cache contains an entry at that location.  If it does,
 * 		also determine whether the entry is dirty, protected,
 * 		pinned, etc. and return that information to the caller
 * 		in *status.
 *
 * 		If the specified entry doesn't exist, set *status_ptr
 * 		to zero.
 *
 * 		On error, the value of *status is undefined.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              4/27/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_get_entry_status(const H5F_t *f, haddr_t addr, unsigned *status)
{
    hbool_t	in_cache;               /* Entry @ addr is in the cache */
    hbool_t	is_dirty;               /* Entry @ addr is in the cache and dirty */
    hbool_t	is_protected;           /* Entry @ addr is in the cache and protected */
    hbool_t	is_pinned;              /* Entry @ addr is in the cache and pinned */
    hbool_t	is_flush_dep_child;     /* Entry @ addr is in the cache and is a flush dependency child */
    hbool_t	is_flush_dep_parent;    /* Entry @ addr is in the cache and is a flush dependency parent */
    herr_t      ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if((f == NULL) || (!H5F_addr_defined(addr)) || (status == NULL))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad param(s) on entry.")

    if(H5C_get_entry_status(f, addr, NULL, &in_cache, &is_dirty,
            &is_protected, &is_pinned, &is_flush_dep_parent, &is_flush_dep_child) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_entry_status() failed.")

    if(in_cache) {
	*status |= H5AC_ES__IN_CACHE;
	if(is_dirty)
	    *status |= H5AC_ES__IS_DIRTY;
	if(is_protected)
	    *status |= H5AC_ES__IS_PROTECTED;
	if(is_pinned)
	    *status |= H5AC_ES__IS_PINNED;
	if(is_flush_dep_parent)
	    *status |= H5AC_ES__IS_FLUSH_DEP_PARENT;
	if(is_flush_dep_child)
	    *status |= H5AC_ES__IS_FLUSH_DEP_CHILD;
    } /* end if */
    else
        *status = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_get_entry_status() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_insert_entry
 *
 * Purpose:     Adds the specified thing to the cache.  The thing need not
 *              exist on disk yet, but it must have an address and disk
 *              space reserved.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul  9 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_insert_entry(H5F_t *f, hid_t dxpl_id, const H5AC_class_t *type, haddr_t addr,
    void *thing, unsigned int flags)
{
#if H5AC__TRACE_FILE_ENABLED
    char          	trace[128] = "";
    size_t              trace_entry_size = 0;
    FILE *        	trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(type->flush);
    HDassert(type->size);
    HDassert(H5F_addr_defined(addr));
    HDassert(thing);

    /* Check for invalid access request */
    if(0 == (H5F_INTENT(f) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "no write intent on file")

#if H5AC__TRACE_FILE_ENABLED
    /* For the insert, only the addr, size, type id and flags are really
     * necessary in the trace file.  Write the result to catch occult
     * errors.
     *
     * Note that some data is not available right now -- put what we can
     * in the trace buffer now, and fill in the rest at the end.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        sprintf(trace, "%s 0x%lx %d 0x%x", FUNC, (unsigned long)addr, type->id,
            flags);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* Insert entry into metadata cache */
    if(H5C_insert_entry(f, dxpl_id, H5AC_dxpl_id, type, addr, thing, flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "H5C_insert_entry() failed")

#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
        /* make note of the entry size */
        trace_entry_size = ((H5C_cache_entry_t *)thing)->size;
#endif /* H5AC__TRACE_FILE_ENABLED */

#ifdef H5_HAVE_PARALLEL
{
    H5AC_aux_t *aux_ptr;

    if(NULL != (aux_ptr = (H5AC_aux_t *)f->shared->cache->aux_ptr)) {
        /* Log the new entry */
        if(H5AC__log_inserted_entry((H5AC_info_t *)thing) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "H5AC__log_inserted_entry() failed")

        /* Check if we should try to flush */
        if(aux_ptr->dirty_bytes >= aux_ptr->dirty_bytes_threshold)
            if(H5AC__run_sync_point(f, H5AC_dxpl_id, H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't run sync point.")
    } /* end if */
}
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d %d\n", trace, (int)trace_entry_size, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_insert_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_mark_entry_dirty
 *
 * Purpose:	Mark a pinned or protected entry as dirty.  The target
 * 		entry MUST be either pinned, protected, or both.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              5/16/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_mark_entry_dirty(void *thing)
{
#if H5AC__TRACE_FILE_ENABLED
    char          	trace[128] = "";
    FILE *        	trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(thing);

#if H5AC__TRACE_FILE_ENABLED
    /* For the mark pinned or protected entry dirty call, only the addr
     * is really necessary in the trace file.  Write the result to catch
     * occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr_from_entry(thing)))
        sprintf(trace, "%s 0x%lx", FUNC,
	        (unsigned long)(((H5C_cache_entry_t *)thing)->addr));
#endif /* H5AC__TRACE_FILE_ENABLED */

#ifdef H5_HAVE_PARALLEL
{
    H5AC_info_t *entry_ptr = (H5AC_info_t *)thing;
    H5C_t *cache_ptr = entry_ptr->cache_ptr;

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    if((!entry_ptr->is_dirty) && (!entry_ptr->is_protected) &&
             (entry_ptr->is_pinned) && (NULL != cache_ptr->aux_ptr))
        if(H5AC__log_dirtied_entry(entry_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "can't log dirtied entry")
}
#endif /* H5_HAVE_PARALLEL */

    if(H5C_mark_entry_dirty(thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "can't mark pinned or protected entry dirty")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_mark_entry_dirty() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_move_entry
 *
 * Purpose:     Use this function to notify the cache that an object's
 *              file address changed.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul  9 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_move_entry(H5F_t *f, const H5AC_class_t *type, haddr_t old_addr, haddr_t new_addr)
{
#if H5AC__TRACE_FILE_ENABLED
    char          	trace[128] = "";
    FILE *        	trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
#ifdef H5_HAVE_PARALLEL
    H5AC_aux_t        *aux_ptr;
#endif /* H5_HAVE_PARALLEL */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(H5F_addr_defined(old_addr));
    HDassert(H5F_addr_defined(new_addr));
    HDassert(H5F_addr_ne(old_addr, new_addr));

#if H5AC__TRACE_FILE_ENABLED
    /* For the move call, only the old addr and new addr are really
     * necessary in the trace file.  Include the type id so we don't have to
     * look it up.  Also write the result to catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        sprintf(trace, "%s 0x%lx 0x%lx %d", FUNC, (unsigned long)old_addr,
		(unsigned long)new_addr, (int)(type->id));
#endif /* H5AC__TRACE_FILE_ENABLED */

#ifdef H5_HAVE_PARALLEL
    /* Log moving the entry */
    if(NULL != (aux_ptr = (H5AC_aux_t *)f->shared->cache->aux_ptr))
        if(H5AC__log_moved_entry(f, old_addr, new_addr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "can't log moved entry")
#endif /* H5_HAVE_PARALLEL */

    if(H5C_move_entry(f->shared->cache, type, old_addr, new_addr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTMOVE, FAIL, "H5C_move_entry() failed.")

#ifdef H5_HAVE_PARALLEL
    /* Check if we should try to flush */
    if(NULL != aux_ptr && aux_ptr->dirty_bytes >= aux_ptr->dirty_bytes_threshold)
        if(H5AC__run_sync_point(f, H5AC_dxpl_id, H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't run sync point.")
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_move_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_pin_protected_entry()
 *
 * Purpose:	Pin a protected cache entry.  The entry must be protected
 *              at the time of call, and must be unpinned.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              4/27/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_pin_protected_entry(void *thing)
{
#if H5AC__TRACE_FILE_ENABLED
    char        trace[128] = "";
    FILE *      trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(thing);

#if H5AC__TRACE_FILE_ENABLED
    /* For the pin protected entry call, only the addr is really necessary
     * in the trace file.  Also write the result to catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr_from_entry(thing)))
        sprintf(trace, "%s 0x%lx", FUNC,
	        (unsigned long)(((H5C_cache_entry_t *)thing)->addr));
#endif /* H5AC__TRACE_FILE_ENABLED */

    if(H5C_pin_protected_entry(thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTPIN, FAIL, "can't pin entry")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_pin_protected_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_create_flush_dependency()
 *
 * Purpose:	Create a flush dependency between two entries in the metadata
 *              cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              3/24/09
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_create_flush_dependency(void * parent_thing, void * child_thing)
{
#if H5AC__TRACE_FILE_ENABLED
    char        trace[128] = "";
    FILE *      trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(parent_thing);
    HDassert(child_thing);

#if H5AC__TRACE_FILE_ENABLED
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr_from_entry(parent_thing)))
        sprintf(trace, "%s %lx %lx", FUNC,
	        (unsigned long)(((H5C_cache_entry_t *)parent_thing)->addr),
	        (unsigned long)(((H5C_cache_entry_t *)child_thing)->addr));
#endif /* H5AC__TRACE_FILE_ENABLED */

    if(H5C_create_flush_dependency(parent_thing, child_thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTDEPEND, FAIL, "H5C_create_flush_dependency() failed.")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_create_flush_dependency() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_protect
 *
 * Purpose:     If the target entry is not in the cache, load it.  If
 *		necessary, attempt to evict one or more entries to keep
 *		the cache within its maximum size.
 *
 *		Mark the target entry as protected, and return its address
 *		to the caller.  The caller must call H5AC_unprotect() when
 *		finished with the entry.
 *
 *		While it is protected, the entry may not be either evicted
 *		or flushed -- nor may it be accessed by another call to
 *		H5AC_protect.  Any attempt to do so will result in a failure.
 *
 * Return:      Success:        Ptr to the object.
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Sep  2 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5AC_protect(H5F_t *f, hid_t dxpl_id, const H5AC_class_t *type, haddr_t addr,
    void *udata, H5AC_protect_t rw)
{
    unsigned		protect_flags = H5C__NO_FLAGS_SET;
    void *		thing;          /* Pointer to native data structure for entry */
#if H5AC__TRACE_FILE_ENABLED
    char                trace[128] = "";
    size_t		trace_entry_size = 0;
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    void *		ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(type->flush);
    HDassert(type->load);
    HDassert(H5F_addr_defined(addr));

    /* Check for invalid access request */
    if(0 == (H5F_INTENT(f) & H5F_ACC_RDWR) && rw == H5AC_WRITE)
	HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, NULL, "no write intent on file")

#if H5AC__TRACE_FILE_ENABLED
    /* For the protect call, only the addr and type id is really necessary
     * in the trace file.  Include the size of the entry protected as a
     * sanity check.  Also indicate whether the call was successful to
     * catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr))) {

	const char * rw_string;

        if ( rw == H5AC_WRITE ) {

	    rw_string = "H5AC_WRITE";

	} else if ( rw == H5AC_READ ) {

	    rw_string = "H5AC_READ";

	} else {

	    rw_string = "???";
	}

        sprintf(trace, "%s 0x%lx %d %s", FUNC, (unsigned long)addr,
		(int)(type->id), rw_string);
    }
#endif /* H5AC__TRACE_FILE_ENABLED */

    if ( rw == H5AC_READ )
	protect_flags |= H5C__READ_ONLY_FLAG;

    if(NULL == (thing = H5C_protect(f, dxpl_id, H5AC_dxpl_id, type, addr, udata, protect_flags)))

        HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, "H5C_protect() failed.")

#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
        /* Make note of the entry size */
        trace_entry_size = ((H5C_cache_entry_t *)thing)->size;
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* Set return value */
    ret_value = thing;

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d %d\n", trace, (int)trace_entry_size, (int)(ret_value != NULL));
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_protect() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_resize_entry
 *
 * Purpose:	Resize a pinned or protected entry.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              7/5/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_resize_entry(void *thing, size_t new_size)
{
#if H5AC__TRACE_FILE_ENABLED
    char          	trace[128] = "";
    FILE *        	trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(thing);

#if H5AC__TRACE_FILE_ENABLED
    /* For the resize pinned entry call, only the addr, and new_size are
     * really necessary in the trace file. Write the result to catch
     * occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr_from_entry(thing)))
        sprintf(trace, "%s 0x%lx %d", FUNC,
	        (unsigned long)(((H5C_cache_entry_t *)thing)->addr),
		(int)new_size);
#endif /* H5AC__TRACE_FILE_ENABLED */

    if(H5C_resize_entry(thing, new_size) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTRESIZE, FAIL, "can't resize entry")

#ifdef H5_HAVE_PARALLEL
{
    H5AC_info_t * entry_ptr = (H5AC_info_t *)thing;
    H5C_t *cache_ptr = entry_ptr->cache_ptr;

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    if((!entry_ptr->is_dirty) && (NULL != cache_ptr->aux_ptr))
        if(H5AC__log_dirtied_entry(entry_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "can't log dirtied entry")
}
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_resize_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_unpin_entry()
 *
 * Purpose:	Unpin a cache entry.  The entry must be unprotected at
 * 		the time of call, and must be pinned.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              4/11/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_unpin_entry(void *thing)
{
#if H5AC__TRACE_FILE_ENABLED
    char                trace[128] = "";
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(thing);

#if H5AC__TRACE_FILE_ENABLED
    /* For the unpin entry call, only the addr is really necessary
     * in the trace file.  Also write the result to catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr_from_entry(thing)))
        sprintf(trace, "%s 0x%lx", FUNC,
	        (unsigned long)(((H5C_cache_entry_t *)thing)->addr));
#endif /* H5AC__TRACE_FILE_ENABLED */

    if(H5C_unpin_entry(thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "can't unpin entry")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_unpin_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_destroy_flush_dependency()
 *
 * Purpose:	Destroy a flush dependency between two entries.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              3/24/09
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_destroy_flush_dependency(void * parent_thing, void * child_thing)
{
#if H5AC__TRACE_FILE_ENABLED
    char                trace[128] = "";
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(parent_thing);
    HDassert(child_thing);

#if H5AC__TRACE_FILE_ENABLED
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr_from_entry(parent_thing)))
        sprintf(trace, "%s %llx %llx", FUNC,
	        (unsigned long long)(((H5C_cache_entry_t *)parent_thing)->addr),
	        (unsigned long long)(((H5C_cache_entry_t *)child_thing)->addr));
#endif /* H5AC__TRACE_FILE_ENABLED */

    if(H5C_destroy_flush_dependency(parent_thing, child_thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "H5C_destroy_flush_dependency() failed.")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_destroy_flush_dependency() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_unprotect
 *
 * Purpose:	Undo an H5AC_protect() call -- specifically, mark the
 *		entry as unprotected, remove it from the protected list,
 *		and give it back to the replacement policy.
 *
 *		The TYPE and ADDR arguments must be the same as those in
 *		the corresponding call to H5AC_protect() and the THING
 *		argument must be the value returned by that call to
 *		H5AC_protect().
 *
 *		If the deleted flag is TRUE, simply remove the target entry
 *		from the cache, clear it, and free it without writing it to
 *		disk.
 *
 *		This verion of the function is a complete re-write to
 *		use the new metadata cache.  While there isn't all that
 *		much difference between the old and new Purpose sections,
 *		the original version is given below.
 *
 *		Original purpose section:
 *
 *		This function should be called to undo the effect of
 *              H5AC_protect().  The TYPE and ADDR arguments should be the
 *              same as the corresponding call to H5AC_protect() and the
 *              THING argument should be the value returned by H5AC_protect().
 *              If the DELETED flag is set, then this object has been deleted
 *              from the file and should not be returned to the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Sep  2 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_unprotect(H5F_t *f, hid_t dxpl_id, const H5AC_class_t *type, haddr_t addr,
    void *thing, unsigned flags)
{
    hbool_t		dirtied;
    hbool_t		deleted;
#ifdef H5_HAVE_PARALLEL
    H5AC_aux_t        * aux_ptr = NULL;
#endif /* H5_HAVE_PARALLEL */
#if H5AC__TRACE_FILE_ENABLED
    char                trace[128] = "";
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t              ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(type->clear);
    HDassert(type->flush);
    HDassert(H5F_addr_defined(addr));
    HDassert(thing);
    HDassert( ((H5AC_info_t *)thing)->addr == addr );
    HDassert( ((H5AC_info_t *)thing)->type == type );

#if H5AC__TRACE_FILE_ENABLED
    /* For the unprotect call, only the addr, type id, flags, and possible
     * new size are really necessary in the trace file.  Write the return
     * value to catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        sprintf(trace, "%s 0x%lx %d", FUNC, (unsigned long)addr, (int)(type->id));
#endif /* H5AC__TRACE_FILE_ENABLED */

    dirtied = (hbool_t)(((flags & H5AC__DIRTIED_FLAG) == H5AC__DIRTIED_FLAG) ||
		(((H5AC_info_t *)thing)->dirtied));
    deleted = (hbool_t)((flags & H5C__DELETED_FLAG) == H5C__DELETED_FLAG);

    /* Check if the size changed out from underneath us, if we're not deleting
     *  the entry.
     */
    if(dirtied && !deleted) {
        size_t		curr_size = 0;

        if((type->size)(f, thing, &curr_size) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTGETSIZE, FAIL, "Can't get size of thing")

        if(((H5AC_info_t *)thing)->size != curr_size)
            HGOTO_ERROR(H5E_CACHE, H5E_BADSIZE, FAIL, "size of entry changed")
    } /* end if */

#ifdef H5_HAVE_PARALLEL
    if(NULL != (aux_ptr = (H5AC_aux_t *)f->shared->cache->aux_ptr)) {
        if(dirtied && ((H5AC_info_t *)thing)->is_dirty == FALSE)
            if(H5AC__log_dirtied_entry((H5AC_info_t *)thing) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "can't log dirtied entry")

        if(deleted && aux_ptr->mpi_rank == 0)
            if(H5AC__log_deleted_entry((H5AC_info_t *)thing) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "H5AC__log_deleted_entry() failed.")
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    if(H5C_unprotect(f, dxpl_id, H5AC_dxpl_id, type, addr, thing, flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "H5C_unprotect() failed.")

#ifdef H5_HAVE_PARALLEL
    /* Check if we should try to flush */
    if((aux_ptr != NULL) && (aux_ptr->dirty_bytes >= aux_ptr->dirty_bytes_threshold))
        if(H5AC__run_sync_point(f, H5AC_dxpl_id, H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't run sync point.")
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s 0x%x %d\n", trace, (unsigned)flags, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_unprotect() */


/*-------------------------------------------------------------------------
 * Function:    HA5C_set_sync_point_done_callback
 *
 * Purpose:     Set the value of the sync_point_done callback.  This 
 *		callback is used by the parallel test code to verify
 *		that the expected writes and only the expected writes
 *		take place during a sync point.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              5/9/10
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
herr_t
H5AC_set_sync_point_done_callback(H5C_t * cache_ptr,
    void (* sync_point_done)(int num_writes, haddr_t * written_entries_tbl))
{
    H5AC_aux_t * aux_ptr;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity checks */
    HDassert(cache_ptr && (cache_ptr->magic == H5C__H5C_T_MAGIC));
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);

    aux_ptr->sync_point_done = sync_point_done;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC_set_sync_point_done_callback() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    HA5C_set_write_done_callback
 *
 * Purpose:     Set the value of the write_done callback.  This callback
 *              is used to improve performance of the parallel test bed
 *              for the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              5/11/06
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
herr_t
H5AC_set_write_done_callback(H5C_t * cache_ptr, void (* write_done)(void))
{
    H5AC_aux_t * aux_ptr;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity checks */
    HDassert(cache_ptr && (cache_ptr->magic == H5C__H5C_T_MAGIC));
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert( aux_ptr != NULL );
    HDassert( aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC );

    aux_ptr->write_done = write_done;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC_set_write_done_callback() */
#endif /* H5_HAVE_PARALLEL */

#ifndef NDEBUG  /* debugging functions */

/*-------------------------------------------------------------------------
 * Function:    H5AC_stats
 *
 * Purpose:     Prints statistics about the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Thursday, October 30, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_stats(const H5F_t *f)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);

    /* at present, this can't fail */
    (void)H5C_stats(f->shared->cache, H5F_OPEN_NAME(f), FALSE);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_dump_cache
 *
 * Purpose:     Dumps a summary of the contents of the metadata cache
 *              to stdout.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              Sunday, October 10, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_dump_cache(const H5F_t *f)
{
    herr_t              ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);

    if(H5C_dump_cache(f->shared->cache, H5F_OPEN_NAME(f)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_dump_cache() failed.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_dump_cache() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 * Function:    H5AC_get_cache_auto_resize_config
 *
 * Purpose:     Wrapper function for H5C_get_cache_auto_resize_config().
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *              3/10/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_get_cache_auto_resize_config(const H5AC_t *cache_ptr,
    H5AC_cache_config_t *config_ptr)
{
    H5C_auto_size_ctl_t internal_config;
    hbool_t evictions_enabled;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    if((cache_ptr == NULL) ||
#ifdef H5_HAVE_PARALLEL
             ((cache_ptr->aux_ptr != NULL) &&
               (((H5AC_aux_t *)(cache_ptr->aux_ptr))->magic != H5AC__H5AC_AUX_T_MAGIC))
             ||
#endif /* H5_HAVE_PARALLEL */
             (config_ptr == NULL) || (config_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr or config_ptr on entry.")

    /* Retrieve the configuration */
    if(H5C_get_cache_auto_resize_config((const H5C_t *)cache_ptr, &internal_config) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_cache_auto_resize_config() failed.")
    if(H5C_get_evictions_enabled((const H5C_t *)cache_ptr, &evictions_enabled) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_resize_enabled() failed.")

    /* Set the information to return */
    if(internal_config.rpt_fcn == NULL)
        config_ptr->rpt_fcn_enabled = FALSE;
    else
	config_ptr->rpt_fcn_enabled = TRUE;
    config_ptr->open_trace_file        = FALSE;
    config_ptr->close_trace_file       = FALSE;
    config_ptr->trace_file_name[0]     = '\0';
    config_ptr->evictions_enabled      = evictions_enabled;
    config_ptr->set_initial_size       = internal_config.set_initial_size;
    config_ptr->initial_size           = internal_config.initial_size;
    config_ptr->min_clean_fraction     = internal_config.min_clean_fraction;
    config_ptr->max_size               = internal_config.max_size;
    config_ptr->min_size               = internal_config.min_size;
    config_ptr->epoch_length           = (long)(internal_config.epoch_length);
    config_ptr->incr_mode              = internal_config.incr_mode;
    config_ptr->lower_hr_threshold     = internal_config.lower_hr_threshold;
    config_ptr->increment              = internal_config.increment;
    config_ptr->apply_max_increment    = internal_config.apply_max_increment;
    config_ptr->max_increment          = internal_config.max_increment;
    config_ptr->decr_mode              = internal_config.decr_mode;
    config_ptr->upper_hr_threshold     = internal_config.upper_hr_threshold;
    config_ptr->flash_incr_mode        = internal_config.flash_incr_mode;
    config_ptr->flash_multiple         = internal_config.flash_multiple;
    config_ptr->flash_threshold        = internal_config.flash_threshold;
    config_ptr->decrement              = internal_config.decrement;
    config_ptr->apply_max_decrement    = internal_config.apply_max_decrement;
    config_ptr->max_decrement          = internal_config.max_decrement;
    config_ptr->epochs_before_eviction = (int)(internal_config.epochs_before_eviction);
    config_ptr->apply_empty_reserve    = internal_config.apply_empty_reserve;
    config_ptr->empty_reserve          = internal_config.empty_reserve;
#ifdef H5_HAVE_PARALLEL
    if(cache_ptr->aux_ptr != NULL) {
        config_ptr->dirty_bytes_threshold = ((H5AC_aux_t *)(cache_ptr->aux_ptr))->dirty_bytes_threshold;
	config_ptr->metadata_write_strategy = ((H5AC_aux_t *)(cache_ptr->aux_ptr))->metadata_write_strategy;
    } /* end if */
    else {
#endif /* H5_HAVE_PARALLEL */
        config_ptr->dirty_bytes_threshold = H5AC__DEFAULT_DIRTY_BYTES_THRESHOLD;
	config_ptr->metadata_write_strategy = H5AC__DEFAULT_METADATA_WRITE_STRATEGY;
#ifdef H5_HAVE_PARALLEL
    } /* end else */
#endif /* H5_HAVE_PARALLEL */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_get_cache_auto_resize_config() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_get_cache_size
 *
 * Purpose:     Wrapper function for H5C_get_cache_size().
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *              3/11/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_get_cache_size(H5AC_t *cache_ptr, size_t *max_size_ptr, size_t *min_clean_size_ptr,
    size_t *cur_size_ptr, int32_t *cur_num_entries_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5C_get_cache_size((H5C_t *)cache_ptr, max_size_ptr, min_clean_size_ptr,
            cur_size_ptr, cur_num_entries_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_cache_size() failed.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_get_cache_size() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_get_cache_hit_rate
 *
 * Purpose:     Wrapper function for H5C_get_cache_hit_rate().
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *              3/10/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_get_cache_hit_rate(H5AC_t *cache_ptr, double *hit_rate_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5C_get_cache_hit_rate((H5C_t *)cache_ptr, hit_rate_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_cache_hit_rate() failed.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_get_cache_hit_rate() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC_reset_cache_hit_rate_stats()
 *
 * Purpose:     Wrapper function for H5C_reset_cache_hit_rate_stats().
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer, 3/10/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_reset_cache_hit_rate_stats(H5AC_t * cache_ptr)
{
    herr_t      ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5C_reset_cache_hit_rate_stats((H5C_t *)cache_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_reset_cache_hit_rate_stats() failed.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_reset_cache_hit_rate_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_set_cache_auto_resize_config
 *
 * Purpose:     Wrapper function for H5C_set_cache_auto_resize_config().
 *
 * Return:      SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer
 *              3/10/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_set_cache_auto_resize_config(H5AC_t *cache_ptr, H5AC_cache_config_t *config_ptr)
{
    H5C_auto_size_ctl_t internal_config;
#if H5AC__TRACE_FILE_ENABLED
    H5AC_cache_config_t trace_config = H5AC__DEFAULT_CACHE_CONFIG;
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache_ptr);

#if H5AC__TRACE_FILE_ENABLED
    /* Make note of the new configuration.  Don't look up the trace file
     * pointer, as that may change before we use it.
     */
    if(config_ptr != NULL)
        trace_config = *config_ptr;
#endif /* H5AC__TRACE_FILE_ENABLED */

    if((cache_ptr == NULL)
#ifdef H5_HAVE_PARALLEL
            || ((cache_ptr->aux_ptr != NULL) &&
                (((H5AC_aux_t *)(cache_ptr->aux_ptr))->magic != H5AC__H5AC_AUX_T_MAGIC))
#endif /* H5_HAVE_PARALLEL */
            )
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad cache_ptr on entry.")

    /* Validate external configuration */
    if(H5AC_validate_config(config_ptr) != SUCCEED)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "Bad cache configuration");

    if(config_ptr->open_trace_file) {
	FILE * file_ptr;

	if(NULL == (file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
	    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_trace_file_ptr() failed.")

	if((!(config_ptr->close_trace_file)) && (file_ptr != NULL))
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "Trace file already open.")
    } /* end if */

    /* Close & reopen trace file, if requested */
    if(config_ptr->close_trace_file)
	if(H5AC_close_trace_file(cache_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_close_trace_file() failed.")
    if(config_ptr->open_trace_file)
        if(H5AC_open_trace_file(cache_ptr, config_ptr->trace_file_name) < 0)
	    HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "H5AC_open_trace_file() failed.")

    /* Convert external configuration to internal representation */
    if(H5AC__ext_config_2_int_config(config_ptr, &internal_config) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC__ext_config_2_int_config() failed.")

    /* Set configuration */
    if(H5C_set_cache_auto_resize_config(cache_ptr, &internal_config) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_set_cache_auto_resize_config() failed.")
    if(H5C_set_evictions_enabled(cache_ptr, config_ptr->evictions_enabled) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_set_evictions_enabled() failed.")

#ifdef H5_HAVE_PARALLEL
    /* Set parallel configuration values */
    /* (Which are only held in the H5AC layer -QAK) */
    if(cache_ptr->aux_ptr != NULL) {
        ((H5AC_aux_t *)(cache_ptr->aux_ptr))->dirty_bytes_threshold = config_ptr->dirty_bytes_threshold;
        ((H5AC_aux_t *)(cache_ptr->aux_ptr))->metadata_write_strategy = config_ptr->metadata_write_strategy;
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    /* For the set cache auto resize config call, only the contents
     * of the config is necessary in the trace file. Write the return
     * value to catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
	HDfprintf(trace_file_ptr,
                  "%s %d %d %d %d \"%s\" %d %d %d %f %d %d %ld %d %f %f %d %f %f %d %d %d %f %f %d %d %d %d %f %zu %d %d\n",
		  "H5AC_set_cache_auto_resize_config",
		  trace_config.version,
		  (int)(trace_config.rpt_fcn_enabled),
		  (int)(trace_config.open_trace_file),
		  (int)(trace_config.close_trace_file),
		  trace_config.trace_file_name,
		  (int)(trace_config.evictions_enabled),
		  (int)(trace_config.set_initial_size),
		  (int)(trace_config.initial_size),
		  trace_config.min_clean_fraction,
		  (int)(trace_config.max_size),
		  (int)(trace_config.min_size),
		  trace_config.epoch_length,
		  (int)(trace_config.incr_mode),
		  trace_config.lower_hr_threshold,
		  trace_config.increment,
		  (int)(trace_config.flash_incr_mode),
		  trace_config.flash_multiple,
		  trace_config.flash_threshold,
		  (int)(trace_config.apply_max_increment),
		  (int)(trace_config.max_increment),
		  (int)(trace_config.decr_mode),
		  trace_config.upper_hr_threshold,
		  trace_config.decrement,
		  (int)(trace_config.apply_max_decrement),
		  (int)(trace_config.max_decrement),
		  trace_config.epochs_before_eviction,
		  (int)(trace_config.apply_empty_reserve),
		  trace_config.empty_reserve,
		  trace_config.dirty_bytes_threshold,
		  trace_config.metadata_write_strategy,
		  (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_set_cache_auto_resize_config() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_validate_config()
 *
 * Purpose:     Run a sanity check on the contents of the supplied
 *		instance of H5AC_cache_config_t.
 *
 *              Do nothing and return SUCCEED if no errors are detected,
 *              and flag an error and return FAIL otherwise.
 *
 *		At present, this function operates by packing the data
 *		from the instance of H5AC_cache_config_t into an instance
 *		of H5C_auto_size_ctl_t, and then calling
 *		H5C_validate_resize_config().  As H5AC_cache_config_t and
 *		H5C_auto_size_ctl_t diverge, we may have to change this.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              4/6/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_validate_config(H5AC_cache_config_t *config_ptr)
{
    H5C_auto_size_ctl_t internal_config;
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    if(config_ptr == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "NULL config_ptr on entry.")
    if(config_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "Unknown config version.")

    /* don't bother to test trace_file_name unless open_trace_file is TRUE */
    if(config_ptr->open_trace_file) {
        size_t	        name_len;

	/* Can't really test the trace_file_name field without trying to
	 * open the file, so we will content ourselves with a couple of
	 * sanity checks on the length of the file name.
	 */
	name_len = HDstrlen(config_ptr->trace_file_name);
	if(name_len == 0)
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "config_ptr->trace_file_name is empty.")
        else if(name_len > H5AC__MAX_TRACE_FILE_NAME_LEN)
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "config_ptr->trace_file_name too long.")
    } /* end if */

    if((config_ptr->evictions_enabled == FALSE) &&
            ((config_ptr->incr_mode != H5C_incr__off) ||
                (config_ptr->flash_incr_mode != H5C_flash_incr__off) ||
                (config_ptr->decr_mode != H5C_decr__off)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "Can't disable evictions while auto-resize is enabled.")

    if(config_ptr->dirty_bytes_threshold < H5AC__MIN_DIRTY_BYTES_THRESHOLD)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "dirty_bytes_threshold too small.")
    else if(config_ptr->dirty_bytes_threshold > H5AC__MAX_DIRTY_BYTES_THRESHOLD)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "dirty_bytes_threshold too big.")

    if((config_ptr->metadata_write_strategy != H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY) &&
         (config_ptr->metadata_write_strategy != H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED))
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "config_ptr->metadata_write_strategy out of range.")

    if(H5AC__ext_config_2_int_config(config_ptr, &internal_config) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC__ext_config_2_int_config() failed.")

    if(H5C_validate_resize_config(&internal_config, H5C_RESIZE_CFG__VALIDATE_ALL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "error(s) in new config.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_validate_config() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_close_trace_file()
 *
 * Purpose:     If a trace file is open, stop logging calls to the cache,
 *              and close the file.
 *
 *              Note that the function does nothing if there is no trace
 *              file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_close_trace_file(H5AC_t *cache_ptr)
{
    FILE *   trace_file_ptr = NULL;
    herr_t   ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(cache_ptr == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "NULL cache_ptr on entry.")

    if(NULL == (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_trace_file_ptr() failed.")

    if(trace_file_ptr != NULL) {
        if(H5C_set_trace_file_ptr(cache_ptr, NULL) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_set_trace_file_ptr() failed.")

        if(HDfclose(trace_file_ptr) != 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close metadata cache trace file")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_close_trace_file() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_open_trace_file()
 *
 * Purpose:     Open a trace file, and start logging calls to the cache.
 *
 * 		This logging is done at the H5C level, and will only take
 * 		place if H5C_TRACE_FILE_ENABLED (defined in H5Cprivate.h)
 * 		is TRUE.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/1/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_open_trace_file(H5AC_t *cache_ptr, const char *trace_file_name)
{
    char     file_name[H5AC__MAX_TRACE_FILE_NAME_LEN + H5C__PREFIX_LEN + 2];
    FILE *   file_ptr = NULL;
#ifdef H5_HAVE_PARALLEL
    H5AC_aux_t * aux_ptr = NULL;
#endif /* H5_HAVE_PARALLEL */
    herr_t   ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(cache_ptr);

    /* Check args */
    if(cache_ptr == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "cache_ptr NULL on entry.")
    if(trace_file_name == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "NULL trace_file_name on entry.")
    if(HDstrlen(trace_file_name) > H5AC__MAX_TRACE_FILE_NAME_LEN)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "trace file name too long.")
    if(NULL != (file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        HGOTO_ERROR(H5E_CACHE, H5E_FILEOPEN, FAIL, "trace file already open.")

#ifdef H5_HAVE_PARALLEL
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    if(cache_ptr->aux_ptr == NULL)
        sprintf(file_name, "%s", trace_file_name);
    else {
	if(aux_ptr->magic != H5AC__H5AC_AUX_T_MAGIC)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad aux_ptr->magic.")

        sprintf(file_name, "%s.%d", trace_file_name, aux_ptr->mpi_rank);
    } /* end else */

    if(HDstrlen(file_name) > H5AC__MAX_TRACE_FILE_NAME_LEN + H5C__PREFIX_LEN + 1)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "cooked trace file name too long.")
#else /* H5_HAVE_PARALLEL */
    HDsnprintf(file_name, (size_t)(H5AC__MAX_TRACE_FILE_NAME_LEN + H5C__PREFIX_LEN + 1), 
               "%s", trace_file_name);
#endif /* H5_HAVE_PARALLEL */

    if((file_ptr = HDfopen(file_name, "w")) == NULL)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "trace file open failed.")

    HDfprintf(file_ptr, "### HDF5 metadata cache trace file version 1 ###\n");

    if(H5C_set_trace_file_ptr(cache_ptr, file_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_set_trace_file_ptr() failed.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_open_trace_file() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_add_candidate()
 *
 * Purpose:     Add the supplied metadata entry address to the candidate
 *		list.  Verify that each entry added does not appear in 
 *		the list prior to its insertion.
 *
 *		This function is intended for used in constructing list
 *		of entried to be flushed during sync points.  It shouldn't
 *		be called anywhere else.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              3/17/10
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
herr_t
H5AC_add_candidate(H5AC_t * cache_ptr, haddr_t addr)
{
    H5AC_aux_t         * aux_ptr;
    H5AC_slist_entry_t * slist_entry_ptr = NULL;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);
    HDassert(aux_ptr->candidate_slist_ptr != NULL);

    /* Construct an entry for the supplied address, and insert
     * it into the candidate slist.
     */
    if(NULL == (slist_entry_ptr = H5FL_MALLOC(H5AC_slist_entry_t)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "Can't allocate candidate slist entry")
    slist_entry_ptr->addr  = addr;

    if(H5SL_insert(aux_ptr->candidate_slist_ptr, slist_entry_ptr, &(slist_entry_ptr->addr)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "can't insert entry into dirty entry slist")

done:
    /* Clean up on error */
    if(ret_value < 0)
        if(slist_entry_ptr)
            slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_add_candidate() */
#endif /* H5_HAVE_PARALLEL */


/*************************************************************************/
/*************************** Debugging Functions: ************************/
/*************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Function:    H5AC_get_entry_ptr_from_addr()
 *
 * Purpose:     Debugging function that attempts to look up an entry in the
 *              cache by its file address, and if found, returns a pointer
 *              to the entry in *entry_ptr_ptr.  If the entry is not in the
 *              cache, *entry_ptr_ptr is set to NULL.
 *
 *              WARNING: This call should be used only in debugging
 *                       routines, and it should be avoided when
 *                       possible.
 *
 *                       Further, if we ever multi-thread the cache,
 *                       this routine will have to be either discarded
 *                       or heavily re-worked.
 *
 *                       Finally, keep in mind that the entry whose
 *                       pointer is obtained in this fashion may not
 *                       be in a stable state.
 *
 *              Note that this function is only defined if NDEBUG
 *              is not defined.
 *
 *              As heavy use of this function is almost certainly a
 *              bad idea, the metadata cache tracks the number of
 *              successful calls to this function, and (if 
 *              H5C_DO_SANITY_CHECKS is defined) displays any
 *              non-zero count on cache shutdown.
 *
 *		This function is just a wrapper that calls the H5C 
 *		version of the function.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 5/30/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5AC_get_entry_ptr_from_addr(const H5F_t *f, haddr_t addr, void **entry_ptr_ptr)
{
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5C_get_entry_ptr_from_addr(f, addr, entry_ptr_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_entry_ptr_from_addr() failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_get_entry_ptr_from_addr() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC_verify_entry_type()
 *
 * Purpose:     Debugging function that attempts to look up an entry in the
 *              cache by its file address, and if found, test to see if its
 *              type field contains the expected value.
 *
 *              If the specified entry is in cache, *in_cache_ptr is set
 *              to TRUE, and *type_ok_ptr is set to TRUE or FALSE
 *              depending on whether the entries type field matches the
 *              expected_type parameter
 *
 *              If the target entry is not in cache, *in_cache_ptr is
 *              set to FALSE, and *type_ok_ptr is undefined.
 *
 *              Note that this function is only defined if NDEBUG
 *              is not defined.
 *
 *		This function is just a wrapper that calls the H5C 
 *		version of the function.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 5/30/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5AC_verify_entry_type(const H5F_t *f, haddr_t addr, const H5AC_class_t *expected_type,
    hbool_t *in_cache_ptr, hbool_t *type_ok_ptr)
{
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5C_verify_entry_type(f, addr, expected_type, in_cache_ptr, type_ok_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_verify_entry_type() failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_verify_entry_type() */
#endif /* NDEBUG */



/*************************************************************************/
/**************************** Private Functions: *************************/
/*************************************************************************/

/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__broadcast_candidate_list()
 *
 * Purpose:     Broadcast the contents of the process 0 candidate entry
 *		slist.  In passing, also remove all entries from said
 *		list.  As the application of this will be handled by 
 *		the same functions on all processes, construct and 
 *		return a copy of the list in the same format as that
 *		received by the other processes.  Note that if this
 *		copy is returned in *haddr_buf_ptr_ptr, the caller 
 *		must free it.
 *
 *		This function must only be called by the process with
 *		MPI_rank 0.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 7/1/05
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__broadcast_candidate_list(H5AC_t *cache_ptr, int *num_entries_ptr,
    haddr_t **haddr_buf_ptr_ptr)
{
    H5AC_aux_t         * aux_ptr = NULL;
    haddr_t            * haddr_buf_ptr = NULL;
    int                  mpi_result;
    int			 num_entries;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->mpi_rank == 0);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);
    HDassert(aux_ptr->candidate_slist_ptr != NULL);
    HDassert(num_entries_ptr != NULL);
    HDassert(*num_entries_ptr == 0);
    HDassert(haddr_buf_ptr_ptr != NULL);
    HDassert(*haddr_buf_ptr_ptr == NULL);

    /* First broadcast the number of entries in the list so that the
     * receivers can set up buffers to receive them.  If there aren't
     * any, we are done.
     */
    num_entries = (int)H5SL_count(aux_ptr->candidate_slist_ptr);
    if(MPI_SUCCESS != (mpi_result = MPI_Bcast(&num_entries, 1, MPI_INT, 0, aux_ptr->mpi_comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_result)

    if(num_entries > 0) {
        size_t		 buf_size = 0;
        int		 chk_num_entries = 0;

        /* convert the candidate list into the format we
         * are used to receiving from process 0, and also load it 
         * into a buffer for transmission.
         */
        if(H5AC__copy_candidate_list_to_buffer(cache_ptr, &chk_num_entries, &haddr_buf_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't construct candidate buffer.")
        HDassert(chk_num_entries == num_entries);
        HDassert(haddr_buf_ptr != NULL);

        /* Now broadcast the list of candidate entries */
        buf_size = sizeof(haddr_t) * (size_t)num_entries;
        if(MPI_SUCCESS != (mpi_result = MPI_Bcast((void *)haddr_buf_ptr, (int)buf_size, MPI_BYTE, 0, aux_ptr->mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_result)
    } /* end if */

    /* Pass the number of entries and the buffer pointer 
     * back to the caller.  Do this so that we can use the same code
     * to apply the candidate list to all the processes.
     */
    *num_entries_ptr = num_entries;
    *haddr_buf_ptr_ptr = haddr_buf_ptr;

done:
    if(ret_value < 0)
        if(haddr_buf_ptr)
            haddr_buf_ptr = (haddr_t *)H5MM_xfree((void *)haddr_buf_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__broadcast_candidate_list() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__broadcast_clean_list_cb()
 *
 * Purpose:     Skip list callback for building array of addresses for
 *              broadcasting the clean list.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  Quincey Koziol, 6/12/15
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__broadcast_clean_list_cb(void *_item, void H5_ATTR_UNUSED *_key,
    void *_udata)
{
    H5AC_slist_entry_t    * slist_entry_ptr = (H5AC_slist_entry_t *)_item;  /* Address of item */
    H5AC_addr_list_ud_t   * udata = (H5AC_addr_list_ud_t *)_udata;      /* Context for callback */
    haddr_t		    addr;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(slist_entry_ptr);
    HDassert(udata);

    /* Store the entry's address in the buffer */
    addr = slist_entry_ptr->addr;
    udata->addr_buf_ptr[udata->i] = addr;
    udata->i++;

    /* now release the entry */
    slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);

    /* and also remove the matching entry from the dirtied list
     * if it exists.
     */
    if(NULL != (slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(udata->aux_ptr->d_slist_ptr, (void *)(&addr))))
        slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC__broadcast_clean_list_cb() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__broadcast_clean_list()
 *
 * Purpose:     Broadcast the contents of the process 0 cleaned entry
 *		slist.  In passing, also remove all entries from said
 *		list, and also remove any matching entries from the dirtied
 *		slist.
 *
 *		This function must only be called by the process with
 *		MPI_rank 0.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 7/1/05
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__broadcast_clean_list(H5AC_t * cache_ptr)
{
    haddr_t	       * addr_buf_ptr = NULL;
    H5AC_aux_t         * aux_ptr;
    int                  mpi_result;
    int			 num_entries = 0;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)cache_ptr->aux_ptr;
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->mpi_rank == 0);
    HDassert(aux_ptr->c_slist_ptr != NULL);

    /* First broadcast the number of entries in the list so that the
     * receives can set up a buffer to receive them.  If there aren't
     * any, we are done.
     */
    num_entries = (int)H5SL_count(aux_ptr->c_slist_ptr);
    if(MPI_SUCCESS != (mpi_result = MPI_Bcast(&num_entries, 1, MPI_INT, 0, aux_ptr->mpi_comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_result)

    if(num_entries > 0) {
        H5AC_addr_list_ud_t udata;
        size_t		 buf_size;

        /* allocate a buffer to store the list of entry base addresses in */
        buf_size = sizeof(haddr_t) * (size_t)num_entries;
        if(NULL == (addr_buf_ptr = (haddr_t *)H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "memory allocation failed for addr buffer")

        /* Set up user data for callback */
        udata.aux_ptr = aux_ptr;
        udata.addr_buf_ptr = addr_buf_ptr;
        udata.i = 0;

        /* Free all the clean list entries, building the address list in the callback */
        /* (Callback also removes the matching entries from the dirtied list) */
        if(H5SL_free(aux_ptr->c_slist_ptr, H5AC__broadcast_clean_list_cb, &udata) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "Can't build address list for clean entries")

        /* Now broadcast the list of cleaned entries */
        if(MPI_SUCCESS != (mpi_result = MPI_Bcast((void *)addr_buf_ptr, (int)buf_size, MPI_BYTE, 0, aux_ptr->mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_result)
    } /* end if */

    /* if it is defined, call the sync point done callback.  Note
     * that this callback is defined purely for testing purposes,
     * and should be undefined under normal operating circumstances.
     */
    if(aux_ptr->sync_point_done)
        (aux_ptr->sync_point_done)(num_entries, addr_buf_ptr);

done:
    if(addr_buf_ptr)
        addr_buf_ptr = (haddr_t *)H5MM_xfree((void *)addr_buf_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__broadcast_clean_list() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__check_if_write_permitted
 *
 * Purpose:     Determine if a write is permitted under the current
 *		circumstances, and set *write_permitted_ptr accordingly.
 *		As a general rule it is, but when we are running in parallel
 *		mode with collective I/O, we must ensure that a read cannot
 *		cause a write.
 *
 *		In the event of failure, the value of *write_permitted_ptr
 *		is undefined.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/15/04
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__check_if_write_permitted(const H5F_t
#ifndef H5_HAVE_PARALLEL
H5_ATTR_UNUSED
#endif /* H5_HAVE_PARALLEL */
    *f, hbool_t *write_permitted_ptr)
{
#ifdef H5_HAVE_PARALLEL
    H5AC_aux_t *	aux_ptr = NULL;
#endif /* H5_HAVE_PARALLEL */
    hbool_t		write_permitted = TRUE;

    FUNC_ENTER_STATIC_NOERR

#ifdef H5_HAVE_PARALLEL
    /* Sanity checks */
    HDassert(f != NULL);
    HDassert(f->shared != NULL);
    HDassert(f->shared->cache != NULL);
    aux_ptr = (H5AC_aux_t *)(f->shared->cache->aux_ptr);
    if(aux_ptr != NULL) {
        HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);

        if((aux_ptr->mpi_rank == 0) || (aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED))
	    write_permitted = aux_ptr->write_permitted;
        else
	    write_permitted = FALSE;
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    *write_permitted_ptr = write_permitted;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC__check_if_write_permitted() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__construct_candidate_list()
 *
 * Purpose:     In the parallel case when the metadata_write_strategy is 
 *		H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED, process 0 uses
 *		this function to construct the list of cache entries to 
 *		be flushed.  This list is then propagated to the other 
 *		caches, and then flushed in a distributed fashion.
 *
 *		The sync_point_op parameter is used to determine the extent
 *		of the flush.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              3/17/10
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__construct_candidate_list(H5AC_t *cache_ptr, H5AC_aux_t *aux_ptr,
    int sync_point_op)
{
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);
    HDassert((sync_point_op == H5AC_SYNC_POINT_OP__FLUSH_CACHE) || (aux_ptr->mpi_rank == 0));
    HDassert(aux_ptr->d_slist_ptr != NULL);
    HDassert(aux_ptr->c_slist_ptr != NULL);
    HDassert(H5SL_count(aux_ptr->c_slist_ptr) == 0);
    HDassert(aux_ptr->candidate_slist_ptr != NULL);
    HDassert(H5SL_count(aux_ptr->candidate_slist_ptr) == 0);
    HDassert((sync_point_op == H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) || (sync_point_op == H5AC_SYNC_POINT_OP__FLUSH_CACHE));

    switch(sync_point_op) {
	case H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN:
            if(H5C_construct_candidate_list__min_clean((H5C_t *)cache_ptr) < 0)
		HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_construct_candidate_list__min_clean() failed.")
	    break;

	case H5AC_SYNC_POINT_OP__FLUSH_CACHE:
            if(H5C_construct_candidate_list__clean_cache((H5C_t *)cache_ptr) < 0)
		HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_construct_candidate_list__clean_cache() failed.")
	    break;

        default:
	    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unknown sync point operation.")
	    break;
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__construct_candidate_list() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__copy_candidate_list_to_buffer_cb
 *
 * Purpose:     Skip list callback for building array of addresses for
 *              broadcasting the candidate list.
 *
 * Return:	Return SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  Quincey Koziol, 6/12/15
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__copy_candidate_list_to_buffer_cb(void *_item, void H5_ATTR_UNUSED *_key,
    void *_udata)
{
    H5AC_slist_entry_t    * slist_entry_ptr = (H5AC_slist_entry_t *)_item;  /* Address of item */
    H5AC_addr_list_ud_t   * udata = (H5AC_addr_list_ud_t *)_udata;      /* Context for callback */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(slist_entry_ptr);
    HDassert(udata);

    /* Store the entry's address in the buffer */
    udata->addr_buf_ptr[udata->i] = slist_entry_ptr->addr;
    udata->i++;

    /* now release the entry */
    slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC__copy_candidate_list_to_buffer_cb() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__copy_candidate_list_to_buffer
 *
 * Purpose:     Allocate buffer(s) and copy the contents of the candidate
 *		entry slist into it (them).  In passing, remove all 
 *		entries from the candidate slist.  Note that the 
 *		candidate slist must not be empty.
 *
 *		If MPI_Offset_buf_ptr_ptr is not NULL, allocate a buffer
 *		of MPI_Offset, copy the contents of the candidate
 *		entry list into it with the appropriate conversions, 
 *		and return the base address of the buffer in 
 *		*MPI_Offset_buf_ptr.  Note that this is the buffer
 *		used by process 0 to transmit the list of entries to 
 *		be flushed to all other processes (in this file group).
 *
 *		Similarly, allocate a buffer of haddr_t, load the contents
 *		of the candidate list into this buffer, and return its 
 *		base address in *haddr_buf_ptr_ptr.  Note that this 
 *		latter buffer is constructed unconditionally.  
 *
 *		In passing, also remove all entries from the candidate
 *		entry slist.
 *
 * Return:	Return SUCCEED on success, and FAIL on failure.
 *
 * Programmer:  John Mainzer, 4/19/10
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__copy_candidate_list_to_buffer(const H5AC_t *cache_ptr, int *num_entries_ptr,
    haddr_t **haddr_buf_ptr_ptr)
{
    H5AC_aux_t         * aux_ptr = NULL;
    H5AC_addr_list_ud_t  udata;
    haddr_t            * haddr_buf_ptr = NULL;
    size_t		 buf_size;
    int			 num_entries = 0;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);
    HDassert(aux_ptr->candidate_slist_ptr != NULL);
    HDassert(H5SL_count(aux_ptr->candidate_slist_ptr) > 0);
    HDassert(num_entries_ptr != NULL);
    HDassert(*num_entries_ptr == 0);
    HDassert(haddr_buf_ptr_ptr != NULL);
    HDassert(*haddr_buf_ptr_ptr == NULL);

    num_entries = (int)H5SL_count(aux_ptr->candidate_slist_ptr);

    /* allocate a buffer(s) to store the list of candidate entry 
     * base addresses in 
     */
    buf_size = sizeof(haddr_t) * (size_t)num_entries;
    if(NULL == (haddr_buf_ptr = (haddr_t *)H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "memory allocation failed for haddr buffer")

    /* Set up user data for callback */
    udata.aux_ptr = aux_ptr;
    udata.addr_buf_ptr = haddr_buf_ptr;
    udata.i = 0;

    /* Free all the candidate list entries, building the address list in the callback */
    if(H5SL_free(aux_ptr->candidate_slist_ptr, H5AC__copy_candidate_list_to_buffer_cb, &udata) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "Can't build address list for candidate entries")

    /* Pass the number of entries and the buffer pointer 
     * back to the caller.
     */
    *num_entries_ptr = num_entries;
    *haddr_buf_ptr_ptr = haddr_buf_ptr;

done:
    if(ret_value < 0)
        if(haddr_buf_ptr)
            haddr_buf_ptr = (haddr_t *)H5MM_xfree((void *)haddr_buf_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__copy_candidate_list_to_buffer() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__ext_config_2_int_config()
 *
 * Purpose:     Utility function to translate an instance of
 *		H5AC_cache_config_t to an instance of H5C_auto_size_ctl_t.
 *
 *		Places translation in *int_conf_ptr and returns SUCCEED
 *		if successful.  Returns FAIL on failure.
 *
 *		Does only minimal sanity checking.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              1/26/06
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__ext_config_2_int_config(H5AC_cache_config_t *ext_conf_ptr,
    H5C_auto_size_ctl_t *int_conf_ptr)
{
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    if((ext_conf_ptr == NULL) || (ext_conf_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION) ||
            (int_conf_ptr == NULL))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad ext_conf_ptr or inf_conf_ptr on entry.")

    int_conf_ptr->version                = H5C__CURR_AUTO_SIZE_CTL_VER;
    if(ext_conf_ptr->rpt_fcn_enabled)
        int_conf_ptr->rpt_fcn            = H5C_def_auto_resize_rpt_fcn;
    else
        int_conf_ptr->rpt_fcn            = NULL;

    int_conf_ptr->set_initial_size       = ext_conf_ptr->set_initial_size;
    int_conf_ptr->initial_size           = ext_conf_ptr->initial_size;
    int_conf_ptr->min_clean_fraction     = ext_conf_ptr->min_clean_fraction;
    int_conf_ptr->max_size               = ext_conf_ptr->max_size;
    int_conf_ptr->min_size               = ext_conf_ptr->min_size;
    int_conf_ptr->epoch_length           = (int64_t)(ext_conf_ptr->epoch_length);

    int_conf_ptr->incr_mode              = ext_conf_ptr->incr_mode;
    int_conf_ptr->lower_hr_threshold     = ext_conf_ptr->lower_hr_threshold;
    int_conf_ptr->increment              = ext_conf_ptr->increment;
    int_conf_ptr->apply_max_increment    = ext_conf_ptr->apply_max_increment;
    int_conf_ptr->max_increment          = ext_conf_ptr->max_increment;
    int_conf_ptr->flash_incr_mode        = ext_conf_ptr->flash_incr_mode;
    int_conf_ptr->flash_multiple         = ext_conf_ptr->flash_multiple;
    int_conf_ptr->flash_threshold        = ext_conf_ptr->flash_threshold;

    int_conf_ptr->decr_mode              = ext_conf_ptr->decr_mode;
    int_conf_ptr->upper_hr_threshold     = ext_conf_ptr->upper_hr_threshold;
    int_conf_ptr->decrement              = ext_conf_ptr->decrement;
    int_conf_ptr->apply_max_decrement    = ext_conf_ptr->apply_max_decrement;
    int_conf_ptr->max_decrement          = ext_conf_ptr->max_decrement;
    int_conf_ptr->epochs_before_eviction = (int32_t)(ext_conf_ptr->epochs_before_eviction);
    int_conf_ptr->apply_empty_reserve    = ext_conf_ptr->apply_empty_reserve;
    int_conf_ptr->empty_reserve          = ext_conf_ptr->empty_reserve;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__ext_config_2_int_config() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__log_deleted_entry()
 *
 * Purpose:     Log an entry which has been deleted.
 *
 *		Only called for mpi_rank 0. We must make sure that the entry
 *              doesn't appear in the cleaned or dirty entry lists.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 6/29/05
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__log_deleted_entry(const H5AC_info_t *entry_ptr)
{
    H5AC_t             * cache_ptr;
    H5AC_aux_t         * aux_ptr;
    H5AC_slist_entry_t * slist_entry_ptr = NULL;
    haddr_t              addr;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(entry_ptr);
    addr = entry_ptr->addr;
    cache_ptr = entry_ptr->cache_ptr;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->mpi_rank == 0);
    HDassert(aux_ptr->d_slist_ptr != NULL);
    HDassert(aux_ptr->c_slist_ptr != NULL);

    /* if the entry appears in the dirtied entry slist, remove it. */
    if(NULL != (slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->d_slist_ptr, (void *)(&addr))))
        slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);

    /* if the entry appears in the cleaned entry slist, remove it. */
    if(NULL != (slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->c_slist_ptr, (void *)(&addr))))
        slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC__log_deleted_entry() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__log_dirtied_entry()
 *
 * Purpose:     Update the dirty_bytes count for a newly dirtied entry.
 *
 *		If mpi_rank isn't 0, this simply means adding the size
 *		of the entries to the dirty_bytes count.
 *
 *		If mpi_rank is 0, we must first check to see if the entry
 *		appears in the dirty entries slist.  If it is, do nothing.
 *		If it isn't, add the size to th dirty_bytes count, add the
 *		entry to the dirty entries slist, and remove it from the
 *		cleaned list (if it is present there).
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 6/29/05
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__log_dirtied_entry(const H5AC_info_t *entry_ptr)
{
    H5AC_t             * cache_ptr;
    H5AC_aux_t         * aux_ptr;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(entry_ptr);
    HDassert(entry_ptr->is_dirty == FALSE);
    cache_ptr = entry_ptr->cache_ptr;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);

    if(aux_ptr->mpi_rank == 0) {
        H5AC_slist_entry_t *slist_entry_ptr;
        haddr_t addr = entry_ptr->addr;

        /* Sanity checks */
        HDassert(aux_ptr->d_slist_ptr != NULL);
        HDassert(aux_ptr->c_slist_ptr != NULL);

        if(NULL == H5SL_search(aux_ptr->d_slist_ptr, (void *)(&addr))) {
            /* insert the address of the entry in the dirty entry list, and
             * add its size to the dirty_bytes count.
             */
            if(NULL == (slist_entry_ptr = H5FL_MALLOC(H5AC_slist_entry_t)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "Can't allocate dirty slist entry .")
            slist_entry_ptr->addr  = addr;

            if(H5SL_insert(aux_ptr->d_slist_ptr, slist_entry_ptr, &(slist_entry_ptr->addr)) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "can't insert entry into dirty entry slist.")

            aux_ptr->dirty_bytes += entry_ptr->size;
#if H5AC_DEBUG_DIRTY_BYTES_CREATION
	    aux_ptr->unprotect_dirty_bytes += entry_ptr->size;
	    aux_ptr->unprotect_dirty_bytes_updates += 1;
#endif /* H5AC_DEBUG_DIRTY_BYTES_CREATION */
        } /* end if */

        /* the entry is dirty.  If it exists on the cleaned entries list,
         * remove it.
         */
        if(NULL != (slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->c_slist_ptr, (void *)(&addr))))
            slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);
    } /* end if */
    else {
        aux_ptr->dirty_bytes += entry_ptr->size;
#if H5AC_DEBUG_DIRTY_BYTES_CREATION
        aux_ptr->unprotect_dirty_bytes += entry_size;
        aux_ptr->unprotect_dirty_bytes_updates += 1;
#endif /* H5AC_DEBUG_DIRTY_BYTES_CREATION */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__log_dirtied_entry() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__log_flushed_entry()
 *
 * Purpose:     Update the clean entry slist for the flush of an entry --
 *		specifically, if the entry has been cleared, remove it
 * 		from both the cleaned and dirtied lists if it is present.
 *		Otherwise, if the entry was dirty, insert the indicated
 *		entry address in the clean slist if it isn't there already.
 *
 *		This function is only used in PHDF5, and should only
 *		be called for the process with mpi rank 0.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 6/29/05
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__log_flushed_entry(H5C_t *cache_ptr, haddr_t addr, hbool_t was_dirty,
    unsigned flags)
{
    hbool_t		 cleared;
    H5AC_aux_t         * aux_ptr;
    H5AC_slist_entry_t * slist_entry_ptr = NULL;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->mpi_rank == 0);
    HDassert(aux_ptr->c_slist_ptr != NULL);

    /* Set local flags */
    cleared = ((flags & H5C__FLUSH_CLEAR_ONLY_FLAG) != 0);

    if(cleared) {
        /* If the entry has been cleared, must remove it from both the
         * cleaned list and the dirtied list.
         */
        if(NULL != (slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->c_slist_ptr, (void *)(&addr))))
            slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);
        if(NULL != (slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->d_slist_ptr, (void *)(&addr))))
            slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);
    } /* end if */
    else if(was_dirty) {
        if(NULL == H5SL_search(aux_ptr->c_slist_ptr, (void *)(&addr))) {
            /* insert the address of the entry in the clean entry list. */
            if(NULL == (slist_entry_ptr = H5FL_MALLOC(H5AC_slist_entry_t)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "Can't allocate clean slist entry .")
            slist_entry_ptr->addr = addr;

            if(H5SL_insert(aux_ptr->c_slist_ptr, slist_entry_ptr, &(slist_entry_ptr->addr)) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "can't insert entry into clean entry slist.")
        } /* end if */
    } /* end else-if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__log_flushed_entry() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__log_inserted_entry()
 *
 * Purpose:     Update the dirty_bytes count for a newly inserted entry.
 *
 *		If mpi_rank isnt 0, this simply means adding the size
 *		of the entry to the dirty_bytes count.
 *
 *		If mpi_rank is 0, we must also add the entry to the
 *		dirty entries slist.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 6/30/05
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__log_inserted_entry(const H5AC_info_t *entry_ptr)
{
    H5AC_t             * cache_ptr;
    H5AC_aux_t         * aux_ptr;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(entry_ptr);
    cache_ptr = entry_ptr->cache_ptr;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);

    if(aux_ptr->mpi_rank == 0) {
        H5AC_slist_entry_t *slist_entry_ptr;

        HDassert(aux_ptr->d_slist_ptr != NULL);
        HDassert(aux_ptr->c_slist_ptr != NULL);

        /* Entry to insert should not be in dirty list currently */
        if(NULL != H5SL_search(aux_ptr->d_slist_ptr, (const void *)(&entry_ptr->addr)))
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Inserted entry already in dirty slist.")

        /* insert the address of the entry in the dirty entry list, and
         * add its size to the dirty_bytes count.
         */
        if(NULL == (slist_entry_ptr = H5FL_MALLOC(H5AC_slist_entry_t)))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "Can't allocate dirty slist entry .")
        slist_entry_ptr->addr  = entry_ptr->addr;
        if(H5SL_insert(aux_ptr->d_slist_ptr, slist_entry_ptr, &(slist_entry_ptr->addr)) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "can't insert entry into dirty entry slist.")

        /* Entry to insert should not be in clean list either */
        if(NULL != H5SL_search(aux_ptr->c_slist_ptr, (const void *)(&entry_ptr->addr)))
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Inserted entry in clean slist.")
    } /* end if */

    aux_ptr->dirty_bytes += entry_ptr->size;

#if H5AC_DEBUG_DIRTY_BYTES_CREATION
    aux_ptr->insert_dirty_bytes += size;
    aux_ptr->insert_dirty_bytes_updates += 1;
#endif /* H5AC_DEBUG_DIRTY_BYTES_CREATION */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__log_inserted_entry() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__log_moved_entry()
 *
 * Purpose:     Update the dirty_bytes count for a moved entry.
 *
 *		WARNING
 *
 *		At present, the way that the move call is used ensures
 *		that the moved entry is present in all caches by
 *		moving in a collective operation and immediately after
 *		unprotecting the target entry.
 *
 *		This function uses this invariant, and will cause arcane
 *		failures if it is not met.  If maintaining this invariant
 *		becomes impossible, we will have to rework this function
 *		extensively, and likely include a bit of IPC for
 *		synchronization.  A better option might be to subsume
 *		move in the unprotect operation.
 *
 *		Given that the target entry is in all caches, the function
 *		proceeds as follows:
 *
 *		For processes with mpi rank other 0, it simply checks to
 *		see if the entry was dirty prior to the move, and adds
 *		the entries size to the dirty bytes count.
 *
 *		In the process with mpi rank 0, the function first checks
 *		to see if the entry was dirty prior to the move.  If it
 *		was, and if the entry doesn't appear in the dirtied list
 *		under its old address, it adds the entry's size to the
 *		dirty bytes count.
 *
 *		The rank 0 process then removes any references to the
 *		entry under its old address from the cleands and dirtied
 *		lists, and inserts an entry in the dirtied list under the
 *		new address.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 6/30/05
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__log_moved_entry(const H5F_t *f, haddr_t old_addr, haddr_t new_addr)
{
    H5AC_t             * cache_ptr;
    H5AC_aux_t         * aux_ptr;
    hbool_t		 entry_in_cache;
    hbool_t		 entry_dirty;
    size_t               entry_size;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    cache_ptr = (H5AC_t *)f->shared->cache;
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);

    /* get entry status, size, etc here */
    if(H5C_get_entry_status(f, old_addr, &entry_size, &entry_in_cache,
            &entry_dirty, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't get entry status.")
    if(!entry_in_cache)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "entry not in cache.")

    if(aux_ptr->mpi_rank == 0) {
        H5AC_slist_entry_t * slist_entry_ptr;

        HDassert(aux_ptr->d_slist_ptr != NULL);
        HDassert(aux_ptr->c_slist_ptr != NULL);

        /* if the entry appears in the cleaned entry slist, under its old
         * address, remove it.
         */
        if(NULL != (slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->c_slist_ptr, (void *)(&old_addr))))
            slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, slist_entry_ptr);

        /* if the entry appears in the dirtied entry slist under its old
         * address, remove it, but don't free it. Set addr to new_addr.
         */
        if(NULL != (slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->d_slist_ptr, (void *)(&old_addr))))
            slist_entry_ptr->addr = new_addr;
        else {
             /* otherwise, allocate a new entry that is ready
              * for insertion, and increment dirty_bytes.
              *
              * Note that the fact that the entry wasn't in the dirtied
              * list under its old address implies that it must have
              * been clean to start with.
              */
            HDassert(!entry_dirty);
            if(NULL == (slist_entry_ptr = H5FL_MALLOC(H5AC_slist_entry_t)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "Can't allocate dirty slist entry .")
            slist_entry_ptr->addr = new_addr;

            aux_ptr->dirty_bytes += entry_size;

#if H5AC_DEBUG_DIRTY_BYTES_CREATION
            aux_ptr->move_dirty_bytes += entry_size;
            aux_ptr->move_dirty_bytes_updates += 1;
#endif /* H5AC_DEBUG_DIRTY_BYTES_CREATION */
        } /* end else */

        /* insert / reinsert the entry in the dirty slist */
        if(H5SL_insert(aux_ptr->d_slist_ptr, slist_entry_ptr, &(slist_entry_ptr->addr)) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "can't insert entry into dirty entry slist.")
    } /* end if */
    else if(!entry_dirty) {
        aux_ptr->dirty_bytes += entry_size;

#if H5AC_DEBUG_DIRTY_BYTES_CREATION
        aux_ptr->move_dirty_bytes += entry_size;
        aux_ptr->move_dirty_bytes_updates += 1;
#endif /* H5AC_DEBUG_DIRTY_BYTES_CREATION */
    } /* end else-if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__log_moved_entry() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__propagate_and_apply_candidate_list
 *
 * Purpose:     Prior to the addition of support for multiple metadata 
 *		write strategies, in PHDF5, only the metadata cache with 
 *		mpi rank 0 was allowed to write to file.  All other 
 *		metadata caches on processes with rank greater than 0 
 *		were required to retain dirty entries until they were 
 *		notified that the entry was clean.
 *
 *		This constraint is relaxed with the distributed 
 *		metadata write strategy, in which a list of candidate
 *		metadata cache entries is constructed by the process 0
 *		cache and then distributed to the caches of all the other
 *		processes.  Once the listed is distributed, many (if not 
 *		all) processes writing writing a unique subset of the 
 *		entries, and marking the remainder clean.  The subsets 
 *		are chosen so that each entry in the list of candidates 
 *		is written by exactly one cache, and all entries are 
 *		marked as being clean in all caches.
 *
 *		While the list of candidate cache entries is prepared 
 *		elsewhere, this function is the main routine for distributing
 *		and applying the list.  It must be run simultaniously on 
 *		all processes that have the relevant file open.  To ensure
 *		proper synchronization, there is a barrier at the beginning 
 *		of this function.
 *
 *		At present, this function is called under one of two 
 *		circumstances:
 *
 *		1) Dirty byte creation exceeds some user specified value.
 *
 *		   While metadata reads may occur independently, all
 *		   operations writing metadata must be collective.  Thus
 *		   all metadata caches see the same sequence of operations,
 *                 and therefore the same dirty data creation.
 *
 *		   This fact is used to synchronize the caches for purposes
 *                 of propagating the list of candidate entries, by simply 
 *		   calling this function from all caches whenever some user 
 *		   specified threshold on dirty data is exceeded.  (the 
 *		   process 0 cache creates the candidate list just before 
 *		   calling this function).
 *
 *		2) Under direct user control -- this operation must be
 *		   collective.
 *
 *              The operations to be managed by this function are as
 * 		follows:
 *
 *		All processes:
 *
 *		1) Participate in an opening barrier.
 *
 *		For the process with mpi rank 0:
 *
 *		1) Load the contents of the candidate list 
 *		   (candidate_slist_ptr) into a buffer, and broadcast that
 *		   buffer to all the other caches.  Clear the candidate
 *		   list in passing.
 *
 *		If there is a positive number of candidates, proceed with 
 *		the following:
 *
 *		2) Apply the candidate entry list.
 *
 *		3) Particpate in a closing barrier.
 *
 *		4) Remove from the dirty list (d_slist_ptr) and from the 
 *		   flushed and still clean entries list (c_slist_ptr),  
 *                 all addresses that appeared in the candidate list, as
 *		   these entries are now clean.
 *
 *
 *		For all processes with mpi rank greater than 0:
 *
 *		1) Receive the candidate entry list broadcast
 *
 *		If there is a positive number of candidates, proceed with 
 *		the following:
 *
 *		2) Apply the candidate entry list.
 *
 *		3) Particpate in a closing barrier.
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              3/17/10
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__propagate_and_apply_candidate_list(H5F_t  *f, hid_t dxpl_id)
{
    H5AC_t             * cache_ptr;
    H5AC_aux_t         * aux_ptr;
    haddr_t            * candidates_list_ptr = NULL;
    int		         mpi_result;
    int	                 num_candidates = 0;
    herr_t               ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);

    /* to prevent "messages from the future" we must synchronize all
     * processes before we write any entries.
     */
    if(MPI_SUCCESS != (mpi_result = MPI_Barrier(aux_ptr->mpi_comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_result)

    if(aux_ptr->mpi_rank == 0) {
        if(H5AC__broadcast_candidate_list(cache_ptr, &num_candidates, &candidates_list_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't broadcast candidate slist.")

        HDassert(H5SL_count(aux_ptr->candidate_slist_ptr) == 0);
    } /* end if */
    else {
        if(H5AC__receive_candidate_list(cache_ptr, &num_candidates, &candidates_list_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't receive candidate broadcast.")
    } /* end else */

    if(num_candidates > 0) {
        herr_t	         result;

        /* all processes apply the candidate list.  
         * H5C_apply_candidate_list() handles the details of 
         * distributing the writes across the processes.
         */

        /* Enable writes during this operation */
        aux_ptr->write_permitted = TRUE;

        /* Apply the candidate list */
        result = H5C_apply_candidate_list(f, dxpl_id, dxpl_id, cache_ptr, num_candidates,
            candidates_list_ptr, aux_ptr->mpi_rank, aux_ptr->mpi_size);

        /* Disable writes again */
        aux_ptr->write_permitted = FALSE;

        /* Check for error on the write operation */
        if(result < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't apply candidate list.")

        /* this code exists primarily for the test bed -- it allows us to
         * enforce posix semantics on the server that pretends to be a 
         * file system in our parallel tests.
         */
	if(aux_ptr->write_done)
	    (aux_ptr->write_done)();

        /* to prevent "messages from the past" we must synchronize all
         * processes again before we go on.
         */
        if(MPI_SUCCESS != (mpi_result = MPI_Barrier(aux_ptr->mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_result)

	/* if this is process zero, tidy up the dirtied,
         * and flushed and still clean lists.
         */
        if(aux_ptr->mpi_rank == 0)
            if(H5AC__tidy_cache_0_lists(cache_ptr, num_candidates, candidates_list_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't tidy up process 0 lists.")
    } /* end if */

    /* if it is defined, call the sync point done callback.  Note
     * that this callback is defined purely for testing purposes,
     * and should be undefined under normal operating circumstances.
     */
    if(aux_ptr->sync_point_done)
        (aux_ptr->sync_point_done)(num_candidates, candidates_list_ptr);

done:
    if(candidates_list_ptr)
        candidates_list_ptr = (haddr_t *)H5MM_xfree((void *)candidates_list_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__propagate_and_apply_candidate_list() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__propagate_flushed_and_still_clean_entries_list
 *
 * Purpose:     In PHDF5, if the process 0 only metadata write strategy
 *		is selected, only the metadata cache with mpi rank 0 is 
 *		allowed to write to file.  All other metadata caches on 
 *		processes with rank greater than 0 must retain dirty 
 *		entries until they are notified that the entry is now 
 *		clean.
 *
 *		This function is the main routine for handling this 
 *		notification proceedure.  It must be called 
 *		simultaniously on all processes that have the relevant 
 *		file open.  To this end, it is called only during a 
 *		sync point, with a barrier prior to the call.
 *
 *		Note that any metadata entry writes by process 0 will
 *		occur after the barrier and just before this call.
 *
 *		Typicaly, calls to this function will be triggered in
 *		one of two ways:
 *
 *		1) Dirty byte creation exceeds some user specified value.
 *
 *		   While metadata reads may occur independently, all
 *		   operations writing metadata must be collective.  Thus
 *		   all metadata caches see the same sequence of operations,
 *                 and therefore the same dirty data creation.
 *
 *		   This fact is used to synchronize the caches for purposes
 *                 of propagating the list of flushed and still clean
 *		   entries, by simply calling this function from all
 *		   caches whenever some user specified threshold on dirty
 *		   data is exceeded.
 *
 *		2) Under direct user control -- this operation must be
 *		   collective.
 *
 *              The operations to be managed by this function are as
 * 		follows:
 *
 *		For the process with mpi rank 0:
 *
 *		1) Load the contents of the flushed and still clean entries
 *		   list (c_slist_ptr) into a buffer, and broadcast that
 *		   buffer to all the other caches.
 *
 *		2) Clear the flushed and still clean entries list
 *                 (c_slist_ptr).
 *
 *
 *		For all processes with mpi rank greater than 0:
 *
 *		1) Receive the flushed and still clean entries list broadcast
 *
 *		2) Mark the specified entries as clean.
 *
 *
 *		For all processes:
 *
 *		1) Reset the dirtied bytes count to 0.
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              July 5, 2005
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__propagate_flushed_and_still_clean_entries_list(H5F_t  *f, hid_t dxpl_id)
{
    H5AC_t     * cache_ptr;
    H5AC_aux_t * aux_ptr;
    herr_t	 ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY);

    if(aux_ptr->mpi_rank == 0) {
        if(H5AC__broadcast_clean_list(cache_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't broadcast clean slist.")
        HDassert(H5SL_count(aux_ptr->c_slist_ptr) == 0);
    } /* end if */
    else {
        if(H5AC__receive_and_apply_clean_list(f, dxpl_id, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't receive and/or process clean slist broadcast.")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__propagate_flushed_and_still_clean_entries_list() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC_receive_haddr_list()
 *
 * Purpose:     Receive the list of entry addresses from process 0,
 *		and return it in a buffer pointed to by *haddr_buf_ptr_ptr.
 *		Note that the caller must free this buffer if it is 
 *		returned.
 *
 *		This function must only be called by the process with
 *		MPI_rank greater than 0.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  Quincey Koziol, 6/11/2015
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__receive_haddr_list(MPI_Comm mpi_comm, int *num_entries_ptr,
    haddr_t **haddr_buf_ptr_ptr)
{
    haddr_t	       * haddr_buf_ptr = NULL;
    int                  mpi_result;
    int			 num_entries;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(num_entries_ptr != NULL);
    HDassert(*num_entries_ptr == 0);
    HDassert(haddr_buf_ptr_ptr != NULL);
    HDassert(*haddr_buf_ptr_ptr == NULL);

    /* First receive the number of entries in the list so that we
     * can set up a buffer to receive them.  If there aren't
     * any, we are done.
     */
    if(MPI_SUCCESS != (mpi_result = MPI_Bcast(&num_entries, 1, MPI_INT, 0, mpi_comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_result)

    if(num_entries > 0) {
        size_t buf_size;

        /* allocate buffers to store the list of entry base addresses in */
        buf_size = sizeof(haddr_t) * (size_t)num_entries;
        if(NULL == (haddr_buf_ptr = (haddr_t *)H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "memory allocation failed for haddr buffer")

        /* Now receive the list of candidate entries */
        if(MPI_SUCCESS != (mpi_result = MPI_Bcast((void *)haddr_buf_ptr, (int)buf_size, MPI_BYTE, 0, mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_result)
    } /* end if */

    /* finally, pass the number of entries and the buffer pointer 
     * back to the caller.
     */
    *num_entries_ptr = num_entries;
    *haddr_buf_ptr_ptr = haddr_buf_ptr;

done:
    if(ret_value < 0)
        if(haddr_buf_ptr)
            haddr_buf_ptr = (haddr_t *)H5MM_xfree((void *)haddr_buf_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_receive_haddr_list() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__receive_and_apply_clean_list()
 *
 * Purpose:     Receive the list of cleaned entries from process 0,
 *		and mark the specified entries as clean.
 *
 *		This function must only be called by the process with
 *		MPI_rank greater than 0.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 7/4/05
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__receive_and_apply_clean_list(H5F_t *f, hid_t primary_dxpl_id, hid_t secondary_dxpl_id)
{
    H5AC_t             * cache_ptr;
    H5AC_aux_t         * aux_ptr;
    haddr_t	       * haddr_buf_ptr = NULL;
    int			 num_entries = 0;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->mpi_rank != 0);

    /* Retrieve the clean list from process 0 */
    if(H5AC__receive_haddr_list(aux_ptr->mpi_comm, &num_entries, &haddr_buf_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't receive clean list")

    if(num_entries > 0)
        /* mark the indicated entries as clean */
        if(H5C_mark_entries_as_clean(f, primary_dxpl_id, secondary_dxpl_id, (int32_t)num_entries, haddr_buf_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't mark entries clean.")

    /* if it is defined, call the sync point done callback.  Note
     * that this callback is defined purely for testing purposes,
     * and should be undefined under normal operating circumstances.
     */
    if(aux_ptr->sync_point_done)
        (aux_ptr->sync_point_done)(num_entries, haddr_buf_ptr);

done:
    if(haddr_buf_ptr)
        haddr_buf_ptr = (haddr_t *)H5MM_xfree((void *)haddr_buf_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__receive_and_apply_clean_list() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__receive_candidate_list()
 *
 * Purpose:     Receive the list of candidate entries from process 0,
 *		and return it in a buffer pointed to by *haddr_buf_ptr_ptr.
 *		Note that the caller must free this buffer if it is 
 *		returned.
 *
 *		This function must only be called by the process with
 *		MPI_rank greater than 0.
 *
 *		Return SUCCEED on success, and FAIL on failure.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 3/17/10
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__receive_candidate_list(const H5AC_t *cache_ptr, int *num_entries_ptr,
    haddr_t **haddr_buf_ptr_ptr)
{
    H5AC_aux_t         * aux_ptr;
    herr_t               ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->mpi_rank != 0);
    HDassert(aux_ptr-> metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);
    HDassert(num_entries_ptr != NULL);
    HDassert(*num_entries_ptr == 0);
    HDassert(haddr_buf_ptr_ptr != NULL);
    HDassert(*haddr_buf_ptr_ptr == NULL);

    /* Retrieve the candidate list from process 0 */
    if(H5AC__receive_haddr_list(aux_ptr->mpi_comm, num_entries_ptr, haddr_buf_ptr_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't receive clean list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__receive_candidate_list() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__rsp__dist_md_write__flush
 *
 * Purpose:     Routine for handling the details of running a sync point
 *		that is triggered by a flush -- which in turn must have been
 *		triggered by either a flush API call or a file close --
 *		when the distributed metadata write strategy is selected.  
 *
 *		Upon entry, each process generates it own candidate list, 
 *              being a sorted list of all dirty metadata entries currently 
 *		in the metadata cache.  Note that this list must be idendical 
 *		across all processes, as all processes see the same stream 
 *		of dirty metadata coming in, and use the same lists of 
 *		candidate entries at each sync point.  (At first glance, this 
 *		argument sounds circular, but think of it in the sense of
 *		a recursive proof).
 *
 *		If this this list is empty, we are done, and the function 
 *		returns
 *
 *		Otherwise, after the sorted list dirty metadata entries is 
 *		constructed, each process uses the same algorithm to assign 
 *		each entry on the candidate list to exactly one process for 
 *		flushing.
 *
 *		At this point, all processes participate in a barrier to
 *		avoid messages from the past/future bugs.
 *
 *		Each process then flushes the entries assigned to it, and 
 *		marks all other entries on the candidate list as clean.
 *
 *		Finally, all processes participate in a second barrier to 
 *		avoid messages from the past/future bugs.
 *
 *		At the end of this process, process 0 and only process 0
 *		must tidy up its lists of dirtied and cleaned entries.   
 *		These lists are not used in the distributed metadata write
 *		strategy, but they must be maintained should we shift 
 *		to a strategy that uses them.
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              April 28, 2010
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__rsp__dist_md_write__flush(H5F_t *f, hid_t dxpl_id)
{
    H5AC_t     * cache_ptr;
    H5AC_aux_t * aux_ptr;
    haddr_t    * haddr_buf_ptr = NULL;
    int		 mpi_result;
    int          num_entries = 0;
    herr_t	 ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);

    /* first construct the candidate list -- initially, this will be in the 
     * form of a skip list.  We will convert it later.
     */
    if(H5C_construct_candidate_list__clean_cache(cache_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't construct candidate list.")

    if(H5SL_count(aux_ptr->candidate_slist_ptr) > 0) {
        herr_t	 result;

        /* convert the candidate list into the format we
         * are used to receiving from process 0.
         */
        if(H5AC__copy_candidate_list_to_buffer(cache_ptr, &num_entries, &haddr_buf_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't construct candidate buffer.")

        /* initial sync point barrier */
        if(MPI_SUCCESS != (mpi_result = MPI_Barrier(aux_ptr->mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_result)

        /* Enable writes during this operation */
        aux_ptr->write_permitted = TRUE;

        /* Apply the candidate list */
        result = H5C_apply_candidate_list(f, dxpl_id, dxpl_id, cache_ptr, num_entries,
            haddr_buf_ptr, aux_ptr->mpi_rank, aux_ptr->mpi_size);

        /* Disable writes again */
        aux_ptr->write_permitted = FALSE;

        /* Check for error on the write operation */
        if(result < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't apply candidate list.")

        /* this code exists primarily for the test bed -- it allows us to
         * enforce posix semantics on the server that pretends to be a 
         * file system in our parallel tests.
         */
        if(aux_ptr->write_done)
            (aux_ptr->write_done)();

        /* final sync point barrier */
        if(MPI_SUCCESS != (mpi_result = MPI_Barrier(aux_ptr->mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_result)

	/* if this is process zero, tidy up the dirtied,
         * and flushed and still clean lists.
         */
        if(aux_ptr->mpi_rank == 0)
            if(H5AC__tidy_cache_0_lists(cache_ptr, num_entries, haddr_buf_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Can't tidy up process 0 lists.")
    } /* end if */

    /* if it is defined, call the sync point done callback.  Note
     * that this callback is defined purely for testing purposes,
     * and should be undefined under normal operating circumstances.
     */
    if(aux_ptr->sync_point_done)
        (aux_ptr->sync_point_done)(num_entries, haddr_buf_ptr);

done:
    if(haddr_buf_ptr)
        haddr_buf_ptr = (haddr_t *)H5MM_xfree((void *)haddr_buf_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__rsp__dist_md_write__flush() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__rsp__dist_md_write__flush_to_min_clean
 *
 * Purpose:     Routine for handling the details of running a sync point
 *		triggered by the accumulation of dirty metadata (as 
 *		opposed to a flush call to the API) when the distributed
 *		metadata write strategy is selected.
 *
 *		After invocation and initial sanity checking this function
 *		first checks to see if evictions are enabled -- if they 
 *		are not, the function does nothing and returns.
 *
 *		Otherwise, process zero constructs a list of entries to 
 *		be flushed in order to bring the process zero cache back
 *		within its min clean requirement.  Note that this list 
 *		(the candidate list) may be empty.
 *
 *              Then, all processes participate in a barrier.
 *
 *		After the barrier, process 0 broadcasts the number of 
 *		entries in the candidate list prepared above, and all 
 *		other processes receive this number.
 *
 *		If this number is zero, we are done, and the function
 *		returns without further action.
 *
 *		Otherwise, process 0 broadcasts the sorted list of 
 *		candidate entries, and all other processes receive it.
 *
 *		Then, each process uses the same algorithm to assign 
 *		each entry on the candidate list to exactly one process 
 *		for flushing.
 *
 *		Each process then flushes the entries assigned to it, and 
 *		marks all other entries on the candidate list as clean.
 *
 *		Finally, all processes participate in a second barrier to 
 *		avoid messages from the past/future bugs.
 *
 *		At the end of this process, process 0 and only process 0
 *		must tidy up its lists of dirtied and cleaned entries.   
 *		These lists are not used in the distributed metadata write
 *		strategy, but they must be maintained should we shift 
 *		to a strategy that uses them.
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              April 28, 2010
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__rsp__dist_md_write__flush_to_min_clean(H5F_t *f, hid_t dxpl_id)
{
    H5AC_t     * cache_ptr;
    H5AC_aux_t * aux_ptr;
    hbool_t 	 evictions_enabled;
    herr_t	 ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);

    /* Query if evictions are allowed */
    if(H5C_get_evictions_enabled((const H5C_t *)cache_ptr, &evictions_enabled) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "H5C_get_evictions_enabled() failed.")

    if(evictions_enabled) {
        /* construct candidate list -- process 0 only */
        if(aux_ptr->mpi_rank == 0)
            if(H5AC__construct_candidate_list(cache_ptr, aux_ptr, H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't construct candidate list.")

        /* propagate and apply candidate list -- all processes */
        if(H5AC__propagate_and_apply_candidate_list(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't propagate and apply candidate list.")
    } /* evictions enabled */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__rsp__dist_md_write__flush_to_min_clean() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__rsp__p0_only__flush
 *
 * Purpose:     Routine for handling the details of running a sync point
 *		that is triggered a flush -- which in turn must have been
 *		triggered by either a flush API call or a file close -- 
 *		when the process 0 only metadata write strategy is selected.  
 *
 *              First, all processes participate in a barrier.
 *
 *		Then process zero flushes all dirty entries, and broadcasts
 *		they number of clean entries (if any) to all the other 
 *		caches.
 *
 *		If this number is zero, we are done.
 *
 *		Otherwise, process 0 broadcasts the list of cleaned 
 *		entries, and all other processes which are part of this
 *		file group receive it, and mark the listed entries as
 *		clean in their caches.
 *
 *		Since all processes have the same set of dirty 
 *		entries at the beginning of the sync point, and all
 *		entries that will be written are written before 
 *		process zero broadcasts the number of cleaned entries,
 *		there is no need for a closing barrier.
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              April 28, 2010
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__rsp__p0_only__flush(H5F_t *f, hid_t dxpl_id)
{
    H5AC_t     * cache_ptr;
    H5AC_aux_t * aux_ptr;
    int		 mpi_result;
    herr_t	 ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY);

    /* to prevent "messages from the future" we must 
     * synchronize all processes before we start the flush.  
     * Hence the following barrier.
     */
    if(MPI_SUCCESS != (mpi_result = MPI_Barrier(aux_ptr->mpi_comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_result)

    /* Flush data to disk, from rank 0 process */
    if(aux_ptr->mpi_rank == 0) {
        herr_t        result;

        /* Enable writes during this operation */
        aux_ptr->write_permitted = TRUE;

        /* Flush the cache */
        result = H5C_flush_cache(f, dxpl_id, dxpl_id, H5AC__NO_FLAGS_SET);

        /* Disable writes again */
        aux_ptr->write_permitted = FALSE;

        /* Check for error on the write operation */
        if(result < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush.")

        /* this code exists primarily for the test bed -- it allows us to
         * enforce posix semantics on the server that pretends to be a 
         * file system in our parallel tests.
         */
        if(aux_ptr->write_done)
            (aux_ptr->write_done)();
    } /* end if */

    /* Propagate cleaned entries to other ranks. */
    if(H5AC__propagate_flushed_and_still_clean_entries_list(f, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't propagate clean entries list.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__rsp__p0_only__flush() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__rsp__p0_only__flush_to_min_clean
 *
 * Purpose:     Routine for handling the details of running a sync point
 *		triggered by the accumulation of dirty metadata (as 
 *		opposed to a flush call to the API) when the process 0
 *		only metadata write strategy is selected.
 *
 *		After invocation and initial sanity checking this function
 *		first checks to see if evictions are enabled -- if they 
 *		are not, the function does nothing and returns.
 *
 *              Otherwise, all processes participate in a barrier.
 *
 *		After the barrier, if this is process 0, the function 
 *		causes the cache to flush sufficient entries to get the 
 *		cache back within its minimum clean fraction, and broadcast 
 *		the number of entries which have been flushed since 
 *		the last sync point, and are still clean.
 *
 *		If this number is zero, we are done.
 *
 *		Otherwise, process 0 broadcasts the list of cleaned 
 *		entries, and all other processes which are part of this
 *		file group receive it, and mark the listed entries as
 *		clean in their caches.
 *
 *		Since all processes have the same set of dirty 
 *		entries at the beginning of the sync point, and all
 *		entries that will be written are written before 
 *		process zero broadcasts the number of cleaned entries,
 *		there is no need for a closing barrier.
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              April 28, 2010
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__rsp__p0_only__flush_to_min_clean(H5F_t *f, hid_t dxpl_id)
{
    H5AC_t     * cache_ptr;
    H5AC_aux_t * aux_ptr;
    hbool_t 	 evictions_enabled;
    herr_t	 ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY);

    /* Query if evictions are allowed */
    if(H5C_get_evictions_enabled((const H5C_t *)cache_ptr, &evictions_enabled) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "H5C_get_evictions_enabled() failed.")

    /* Flush if evictions are allowed -- following call
     * will cause process 0 to flush to min clean size,
     * and then propagate the newly clean entries to the
     * other processes.
     *
     * Otherwise, do nothing.
     */
    if(evictions_enabled) {
        int          mpi_result;

        /* to prevent "messages from the future" we must synchronize all
         * processes before we start the flush.  This synchronization may
         * already be done -- hence the do_barrier parameter.
         */
        if(MPI_SUCCESS != (mpi_result = MPI_Barrier(aux_ptr->mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_result)

        if(0 == aux_ptr->mpi_rank) {
            herr_t	 result;

            /* here, process 0 flushes as many entries as necessary to 
             * comply with the currently specified min clean size.
             * Note that it is quite possible that no entries will be 
             * flushed.
             */

            /* Enable writes during this operation */
            aux_ptr->write_permitted = TRUE;

            /* Flush the cache */
            result = H5C_flush_to_min_clean(f, dxpl_id, H5AC_dxpl_id);

            /* Disable writes again */
            aux_ptr->write_permitted = FALSE;

            /* Check for error on the write operation */
            if(result < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_flush_to_min_clean() failed.")

            /* this call exists primarily for the test code -- it is used
 	     * to enforce POSIX semantics on the process used to simulate
 	     * reads and writes in t_cache.c.
             */
            if(aux_ptr->write_done)
                (aux_ptr->write_done)();
        } /* end if */

        if(H5AC__propagate_flushed_and_still_clean_entries_list(f, dxpl_id) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't propagate clean entries list.")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__rsp__p0_only__flush_to_min_clean() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__run_sync_point
 *
 * Purpose:     Top level routine for managing a sync point between all
 *		meta data caches in the parallel case.  Since all caches 
 *		see the same sequence of dirty metadata, we simply count
 *		bytes of dirty metadata, and run a sync point whenever the
 *		number of dirty bytes of metadata seen since the last
 *		sync point exceeds a threshold that is common across all
 *		processes.  We also run sync points in response to 
 *		HDF5 API calls triggering either a flush or a file close.
 *
 *		In earlier versions of PHDF5, only the metadata cache with 
 *		mpi rank 0 was allowed to write to file.  All other 
 *		metadata caches on processes with rank greater than 0 were
 *		required to retain dirty entries until they were notified 
 *		that the entry is was clean.
 *
 *		This function was created to make it easier for us to 
 *		experiment with other options, as it is a single point 
 *		for the execution of sync points.  
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              March 11, 2010
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__run_sync_point(H5F_t *f, hid_t dxpl_id, int sync_point_op)
{
    H5AC_t     * cache_ptr;
    H5AC_aux_t * aux_ptr;
    herr_t	 ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f != NULL);
    cache_ptr = f->shared->cache;
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert((sync_point_op == H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) ||
        (sync_point_op == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED));

#if H5AC_DEBUG_DIRTY_BYTES_CREATION
HDfprintf(stdout, "%d:H5AC_propagate...:%u: (u/uu/i/iu/r/ru) = %zu/%u/%zu/%u/%zu/%u\n",
    aux_ptr->mpi_rank,
    aux_ptr->dirty_bytes_propagations,
    aux_ptr->unprotect_dirty_bytes,
    aux_ptr->unprotect_dirty_bytes_updates,
    aux_ptr->insert_dirty_bytes,
    aux_ptr->insert_dirty_bytes_updates,
    aux_ptr->rename_dirty_bytes,
    aux_ptr->rename_dirty_bytes_updates);
#endif /* H5AC_DEBUG_DIRTY_BYTES_CREATION */

    switch(aux_ptr->metadata_write_strategy) {
        case H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY:
	    switch(sync_point_op) {
                case H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN:
	            if(H5AC__rsp__p0_only__flush_to_min_clean(f, dxpl_id) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "H5AC__rsp__p0_only__flush_to_min_clean() failed.")
		    break;

		case H5AC_SYNC_POINT_OP__FLUSH_CACHE:
	            if(H5AC__rsp__p0_only__flush(f, dxpl_id) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "H5AC__rsp__p0_only__flush() failed.")
		    break;

		default:
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unknown flush op");
		    break;
	    } /* end switch */
	    break;

	case H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED:
	    switch(sync_point_op) {
                case H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN:
	            if(H5AC__rsp__dist_md_write__flush_to_min_clean(f, dxpl_id) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "H5AC__rsp__dist_md_write__flush_to_min_clean() failed.")
		    break;

		case H5AC_SYNC_POINT_OP__FLUSH_CACHE:
	            if(H5AC__rsp__dist_md_write__flush(f, dxpl_id) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "H5AC__rsp__dist_md_write__flush() failed.")
		    break;

		default:
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unknown flush op");
		    break;
	    } /* end switch */
	    break;

	default:
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Unknown metadata write strategy.")
	    break;
    } /* end switch */

    /* reset the dirty bytes count */
    aux_ptr->dirty_bytes = 0;

#if H5AC_DEBUG_DIRTY_BYTES_CREATION
    aux_ptr->dirty_bytes_propagations     += 1;
    aux_ptr->unprotect_dirty_bytes         = 0;
    aux_ptr->unprotect_dirty_bytes_updates = 0;
    aux_ptr->insert_dirty_bytes            = 0;
    aux_ptr->insert_dirty_bytes_updates    = 0;
    aux_ptr->rename_dirty_bytes            = 0;
    aux_ptr->rename_dirty_bytes_updates    = 0;
#endif /* H5AC_DEBUG_DIRTY_BYTES_CREATION */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__run_sync_point() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__tidy_cache_0_lists()
 *
 * Purpose:     In the distributed metadata write strategy, not all dirty
 *		entries are written by process 0 -- thus we must tidy
 *		up the dirtied, and flushed and still clean lists 
 *		maintained by process zero after each sync point.
 *
 *		This procedure exists to tend to this issue.
 *
 *		At this point, all entries that process 0 cleared should
 *		have been removed from both the dirty and flushed and 
 *		still clean lists, and entries that process 0 has flushed
 *		should have been removed from the dirtied list and added
 *		to the flushed and still clean list.
 *
 *		However, since the distributed metadata write strategy
 *		doesn't make use of these lists, the objective is simply
 *		to maintain these lists in consistent state that allows
 *		them to be used should the metadata write strategy change
 *		to one that uses these lists.
 *
 *		Thus for our purposes, all we need to do is remove from 
 *		the dirtied and flushed and still clean lists all
 *		references to entries that appear in the candidate list.
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              4/20/10
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__tidy_cache_0_lists(H5AC_t *cache_ptr, int num_candidates,
    haddr_t *candidates_list_ptr)
{
    H5AC_aux_t         * aux_ptr;
    int                  i;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    aux_ptr = (H5AC_aux_t *)(cache_ptr->aux_ptr);
    HDassert(aux_ptr != NULL);
    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);
    HDassert(aux_ptr->metadata_write_strategy == H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED);
    HDassert(aux_ptr->mpi_rank == 0);
    HDassert(num_candidates > 0);
    HDassert(candidates_list_ptr != NULL);

    /* clean up dirtied and flushed and still clean lists by removing 
     * all entries on the candidate list.  Cleared entries should 
     * have been removed from both the dirty and cleaned lists at 
     * this point, flushed entries should have been added to the 
     * cleaned list.  However, for this metadata write strategy, 
     * we just want to remove all references to the candidate entries.
     */
    for(i = 0; i < num_candidates; i++) {
        H5AC_slist_entry_t * d_slist_entry_ptr;
        H5AC_slist_entry_t * c_slist_entry_ptr;
        haddr_t              addr;

        addr = candidates_list_ptr[i];

        /* addr may be either on the dirtied list, or on the flushed 
         * and still clean list.  Remove it.
         */
        if(NULL != (d_slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->d_slist_ptr, (void *)&addr)))
            d_slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, d_slist_entry_ptr);
        if(NULL != (c_slist_entry_ptr = (H5AC_slist_entry_t *)H5SL_remove(aux_ptr->c_slist_ptr, (void *)&addr)))
            c_slist_entry_ptr = H5FL_FREE(H5AC_slist_entry_t, c_slist_entry_ptr);
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC__tidy_cache_0_lists() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5AC__flush_entries
 *
 * Purpose:     Flush the metadata cache associated with the specified file,
 *              only writing from rank 0, but propagating the cleaned entries
 *              to all ranks.
 *
 * Return:      Non-negative on success/Negative on failure if there was a
 *              request to flush all items and something was protected.
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Aug 22 2009
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
static herr_t
H5AC__flush_entries(H5F_t *f, hid_t dxpl_id)
{
    herr_t        ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared->cache);

    /* Check if we have >1 ranks */
    if(f->shared->cache->aux_ptr)
        if(H5AC__run_sync_point(f, dxpl_id, H5AC_SYNC_POINT_OP__FLUSH_CACHE) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't run sync point.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__flush_entries() */
#endif /* H5_HAVE_PARALLEL */


/*------------------------------------------------------------------------------
 * Function:    H5AC_ignore_tags()
 *
 * Purpose:     Override all assertion frameworks and force application of 
 *              global tag everywhere. This should really only be used in the
 *              tests that need to access functions without going through 
 *              API paths.
 * 
 * Return:      SUCCEED on success, FAIL otherwise.
 *
 * Programmer:  Mike McGreevy
 *              December 1, 2009
 *
 *------------------------------------------------------------------------------
 */
herr_t
H5AC_ignore_tags(const H5F_t *f)
{
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);

    /* Set up a new metadata tag */
    if(H5C_ignore_tags(f->shared->cache) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "H5C_ignore_tags() failed.")
            
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_ignore_tags() */


/*------------------------------------------------------------------------------
 * Function:    H5AC_tag()
 *
 * Purpose:     Sets the metadata tag property in the provided property list.
 * 
 * Return:      SUCCEED on success, FAIL otherwise.
 *
 * Programmer:  Mike McGreevy
 *              December 1, 2009
 *
 *------------------------------------------------------------------------------
 */
herr_t
H5AC_tag(hid_t dxpl_id, haddr_t metadata_tag, haddr_t *prev_tag)
{
    H5P_genplist_t *dxpl;       /* dataset transfer property list */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check Arguments */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADTYPE, FAIL, "not a property list")

    /* Get the current tag value and return that (if prev_tag is NOT null) */
    if(prev_tag)
        if((H5P_get(dxpl, "H5AC_metadata_tag", prev_tag)) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "unable to query dxpl")

    /* Set the provided tag value in the dxpl_id. */
    if(H5P_set(dxpl, "H5AC_metadata_tag", &metadata_tag) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "can't set property in dxpl")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_tag */


/*------------------------------------------------------------------------------
 * Function:    H5AC_retag_copied_metadata()
 *
 * Purpose:     Searches through cache index for all entries with the
 *              H5AC__COPIED_TAG, indicating that it was created as a 
 *              result of an object copy, and applies the provided tag.
 * 
 * Return:      SUCCEED on success, FAIL otherwise.
 *
 * Programmer:  Mike McGreevy
 *              March 17, 2010
 *
 *------------------------------------------------------------------------------
 */
herr_t
H5AC_retag_copied_metadata(const H5F_t *f, haddr_t metadata_tag) 
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
     
    /* Call cache-level function to retag entries */
    H5C_retag_copied_metadata(f->shared->cache, metadata_tag);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC_retag_copied_metadata */

