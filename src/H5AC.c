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

#include "H5ACmodule.h"         /* This source code file is part of the H5AC module */
#define H5C_FRIEND		        /* Suppress error about including H5Cpkg	        */
#define H5F_FRIEND		        /* Suppress error about including H5Fpkg	        */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACpkg.h"		/* Metadata cache			*/
#include "H5Cpkg.h"		        /* Cache                                */
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5SLprivate.h"        /* Skip Lists                           */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5AC__check_if_write_permitted(const H5F_t *f,
    hbool_t *write_permitted_ptr);
static herr_t H5AC__ext_config_2_int_config(H5AC_cache_config_t *ext_conf_ptr,
    H5C_auto_size_ctl_t *int_conf_ptr);
#if H5AC_DO_TAGGING_SANITY_CHECKS
static herr_t H5AC__verify_tag(hid_t dxpl_id, const H5AC_class_t * type);
#endif /* H5AC_DO_TAGGING_SANITY_CHECKS */


/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;

/*****************************/
/* Library Private Variables */
/*****************************/

/* Default dataset transfer property list for metadata I/O calls (coll write, ind read) */
hid_t H5AC_ind_read_dxpl_id = (-1);
#ifdef H5_HAVE_PARALLEL
/* collective metadata read property */
hid_t H5AC_coll_read_dxpl_id = (-1);
#endif /* H5_HAVE_PARALLEL */

/* DXPL to be used in operations that will not result in I/O calls */
hid_t H5AC_noio_dxpl_id = (-1);

/* Default DXPL to be used for raw data I/O operations when one is not
   provided by the user (fill values in H5Dcreate) */
hid_t H5AC_rawdata_dxpl_id = (-1);

#ifdef H5_HAVE_PARALLEL
/* Environment variable for collective API sanity checks */
hbool_t H5_coll_api_sanity_check_g = false;
#endif /* H5_HAVE_PARALLEL */

/*******************/
/* Local Variables */
/*******************/

static const char *H5AC_entry_type_names[H5AC_NTYPES] =
{
    "B-tree nodes",
    "symbol table nodes",
    "local heap prefixes",
    "local heap data blocks",
    "global heaps",
    "object headers",
    "object header chunks",
    "object header proxies",
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
    "driver info",
    "test entry"	/* for testing only -- not used for actual files */
};



/*-------------------------------------------------------------------------
 * Function:	H5AC_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
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
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_init() */


/*-------------------------------------------------------------------------
 * Function:	H5AC__init_package
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
herr_t
H5AC__init_package(void)
{
    H5P_genplist_t  *xfer_plist;    /* Dataset transfer property list object */
#ifdef H5_HAVE_PARALLEL
    H5P_coll_md_read_flag_t coll_meta_read;
#endif /* H5_HAVE_PARALLEL */
#ifdef H5_DEBUG_BUILD
    H5FD_dxpl_type_t  dxpl_type;    /* Property indicating the type of the internal dxpl */
#endif /* H5_DEBUG_BUILD */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

#ifdef H5_HAVE_PARALLEL
    /* check whether to enable strict collective function calling
       sanity checks using MPI barriers */
    {
        const char *s;  /* String for environment variables */

        s = HDgetenv("H5_COLL_API_SANITY_CHECK");
        if(s && HDisdigit(*s)) {
            H5_coll_api_sanity_check_g = (hbool_t)HDstrtol(s, NULL, 0);
        }
    }
#endif /* H5_HAVE_PARALLEL */

#if defined(H5_HAVE_PARALLEL) || defined(H5_DEBUG_BUILD)
    /* Get an ID for the internal independent metadata dxpl */
    if((H5AC_ind_read_dxpl_id = H5P_create_id(H5P_CLS_DATASET_XFER_g, FALSE)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "unable to register property list")

    /* Get an ID for the no I/O internal dxpl */
    if((H5AC_noio_dxpl_id = H5P_create_id(H5P_CLS_DATASET_XFER_g, FALSE)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "unable to register property list")

    /* Get an ID for the raw data (H5AC) dxpl */
    if((H5AC_rawdata_dxpl_id = H5P_create_id(H5P_CLS_DATASET_XFER_g, FALSE)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "unable to register property list")

    /* if this is a debug build, set the dxpl type flag on the
       independent metadata dxpl and create the noio and raw data internal dxpls */
#ifdef H5_DEBUG_BUILD
    /* Get the property list object */
    if (NULL == (xfer_plist = (H5P_genplist_t *)H5I_object(H5AC_ind_read_dxpl_id)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADATOM, FAIL, "can't get new property list object")
    /* Insert the dxpl type property  */
    dxpl_type = H5FD_METADATA_DXPL;
    if(H5P_set(xfer_plist, H5FD_DXPL_TYPE_NAME, &dxpl_type) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "can't set dxpl type property")

    /* Get the property list object */
    if (NULL == (xfer_plist = (H5P_genplist_t *)H5I_object(H5AC_noio_dxpl_id)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADATOM, FAIL, "can't get new property list object")
    /* Insert the dxpl type property  */
    dxpl_type = H5FD_NOIO_DXPL;
    if(H5P_set(xfer_plist, H5FD_DXPL_TYPE_NAME, &dxpl_type) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "can't set dxpl type property")

    /* Get the property list object */
    if (NULL == (xfer_plist = (H5P_genplist_t *)H5I_object(H5AC_rawdata_dxpl_id)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADATOM, FAIL, "can't get new property list object")
    /* Insert the dxpl type property  */
    dxpl_type = H5FD_RAWDATA_DXPL;
    if(H5P_set(xfer_plist, H5FD_DXPL_TYPE_NAME, &dxpl_type) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "can't set dxpl type property")
#endif /* H5_DEBUG_BUILD */

    /* if this is a parallel build, create an internal dxpl for
       collective metadata reads */
#ifdef H5_HAVE_PARALLEL
    /* Get an ID for H5AC_coll_read_dxpl_id */
    if((H5AC_coll_read_dxpl_id = H5P_create_id(H5P_CLS_DATASET_XFER_g, FALSE)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "unable to register property list")
    /* Get the property list object */
    if (NULL == (xfer_plist = (H5P_genplist_t *)H5I_object(H5AC_coll_read_dxpl_id)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADATOM, FAIL, "can't get new property list object")
    /* set 'collective metadata read' property */
    coll_meta_read = H5P_USER_TRUE;
    if(H5P_set(xfer_plist, H5_COLL_MD_READ_FLAG_NAME, &coll_meta_read) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set collective metadata read flag")

    /* if we have a debug build, set the dxpl type to metadata on the
       collective metadata dxpl */
#ifdef H5_DEBUG_BUILD
    /* set metadata dxpl type */
    dxpl_type = H5FD_METADATA_DXPL;
    if(H5P_set(xfer_plist, H5FD_DXPL_TYPE_NAME, &dxpl_type) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "can't set dxpl type property")
#endif /* H5_DEBUG_BUILD */
#endif /* H5_HAVE_PARALLEL */

#else /* defined(H5_HAVE_PARALLEL) || defined(H5_DEBUG_BUILD) */
    H5AC_ind_read_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    H5AC_noio_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    H5AC_rawdata_dxpl_id = H5P_DATASET_XFER_DEFAULT;
#endif /* defined(H5_HAVE_PARALLEL) || defined(H5_DEBUG_BUILD) */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC__init_package() */


/*-------------------------------------------------------------------------
 * Function:	H5AC_term_package
 *
 * Purpose:	Terminate this interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				affect other interfaces; zero otherwise.
 * 		Failure:	Negative.
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 18, 2002
 *
 *-------------------------------------------------------------------------
 */
int
H5AC_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
        if(H5AC_ind_read_dxpl_id > 0 || H5AC_noio_dxpl_id > 0 || H5AC_rawdata_dxpl_id > 0
#ifdef H5_HAVE_PARALLEL
           || H5AC_coll_read_dxpl_id > 0
#endif /* H5_HAVE_PARALLEL */
           ) {
#if defined(H5_HAVE_PARALLEL) || defined(H5_DEBUG_BUILD)
            /* Indicate more work to do */
            n = 1; /* H5I */

            /* Close H5AC dxpls */
            if(H5I_dec_ref(H5AC_ind_read_dxpl_id) < 0 ||
               H5I_dec_ref(H5AC_noio_dxpl_id) < 0 ||
               H5I_dec_ref(H5AC_rawdata_dxpl_id) < 0
#ifdef H5_HAVE_PARALLEL
               || H5I_dec_ref(H5AC_coll_read_dxpl_id) < 0
#endif /* H5_HAVE_PARALLEL */
               )
                H5E_clear_stack(NULL); /*ignore error*/
#endif /* defined(H5_HAVE_PARALLEL) || defined(H5_DEBUG_BUILD) */

            /* Reset static IDs */
            H5AC_ind_read_dxpl_id = (-1);
            H5AC_noio_dxpl_id = (-1);
            H5AC_rawdata_dxpl_id = (-1);
#ifdef H5_HAVE_PARALLEL
            H5AC_coll_read_dxpl_id = (-1);
#endif /* H5_HAVE_PARALLEL */
        } /* end if */

        /* Reset interface initialization flag */
        if(0 == n)
            H5_PKG_INIT_VAR = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5AC_term_package() */


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

    /* Turn on metadata cache logging, if being used */
    if(f->shared->use_mdc_logging) {
        if(H5C_set_up_logging(f->shared->cache, f->shared->mdc_log_location, f->shared->start_mdc_log_on_access) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "mdc logging setup failed")

        /* Write the log header regardless of current logging status */
        if(H5AC__write_create_cache_log_msg(f->shared->cache) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")
    } /* end if */

    /* Set the cache parameters */
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

    if(f->shared->use_mdc_logging) {
        /* Write the log footer regardless of current logging status */
        if(H5AC__write_destroy_cache_log_msg(f->shared->cache) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")
        if(H5C_tear_down_logging(f->shared->cache) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdc logging tear-down failed")
    } /* end if */

#ifdef H5_HAVE_PARALLEL
    /* destroying the cache, so clear all collective entries */
    if(H5C_clear_coll_entries(f->shared->cache, 0) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "H5C_clear_coll_entries() failed.")

    aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(f->shared->cache);
    if(aux_ptr)
        /* Sanity check */
        HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);

    /* Attempt to flush all entries from rank 0 & Bcast clean list to other ranks */
    if(H5AC__flush_entries(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush.")
#endif /* H5_HAVE_PARALLEL */

    /* Destroy the cache */
    if(H5C_dest(f, dxpl_id) < 0)
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
 * Function:    H5AC_evict
 *
 * Purpose:     Evict all entries except the pinned entries
 *		in the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Dec 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_evict(H5F_t *f, hid_t dxpl_id)
{
    hbool_t log_enabled;             /* TRUE if logging was set up */
    hbool_t curr_logging;            /* TRUE if currently logging */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(f->shared->cache, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

    /* Evict all entries in the cache except the pinned superblock entry */
    if(H5C_evict(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "can't evict cache")

done:

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_evict_cache_log_msg(f->shared->cache, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_evict() */


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
    hbool_t log_enabled;              /* TRUE if logging was set up */
    hbool_t curr_logging;             /* TRUE if currently logging */
    herr_t  ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(type->serialize);
    HDassert(H5F_addr_defined(addr));

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(f->shared->cache, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

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

    if(H5C_expunge_entry(f, dxpl_id, type, addr, flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, FAIL, "H5C_expunge_entry() failed.")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_expunge_entry_log_msg(f->shared->cache, addr, type->id, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
    hbool_t log_enabled;              /* TRUE if logging was set up */
    hbool_t curr_logging;             /* TRUE if currently logging */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(f->shared->cache, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

#if H5AC__TRACE_FILE_ENABLED
    /* For the flush, only the flags are really necessary in the trace file.
     * Write the result to catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
	sprintf(trace, "%s", FUNC);
#endif /* H5AC__TRACE_FILE_ENABLED */

#ifdef H5_HAVE_PARALLEL
    /* flushing the cache, so clear all collective entries */
    if(H5C_clear_coll_entries(f->shared->cache, 0) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "H5C_clear_coll_entries() failed.")

    /* Attempt to flush all entries from rank 0 & Bcast clean list to other ranks */
    if(H5AC__flush_entries(f, dxpl_id) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush.")
#endif /* H5_HAVE_PARALLEL */

    /* Flush the cache */
    /* (Again, in parallel - writes out the superblock) */
    if(H5C_flush_cache(f, dxpl_id, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush cache.")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
        HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_flush_cache_log_msg(f->shared->cache, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
    hbool_t	is_corked;
    hbool_t	is_flush_dep_child;     /* Entry @ addr is in the cache and is a flush dependency child */
    hbool_t	is_flush_dep_parent;    /* Entry @ addr is in the cache and is a flush dependency parent */
    herr_t      ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if((f == NULL) || (!H5F_addr_defined(addr)) || (status == NULL))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad param(s) on entry.")

    if(H5C_get_entry_status(f, addr, NULL, &in_cache, &is_dirty,
            &is_protected, &is_pinned, &is_corked, &is_flush_dep_parent, &is_flush_dep_child) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_entry_status() failed.")

    if(in_cache) {
	*status |= H5AC_ES__IN_CACHE;
	if(is_dirty)
	    *status |= H5AC_ES__IS_DIRTY;
	if(is_protected)
	    *status |= H5AC_ES__IS_PROTECTED;
	if(is_pinned)
	    *status |= H5AC_ES__IS_PINNED;
	if(is_corked)
	    *status |= H5AC_ES__IS_CORKED;
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
    hbool_t log_enabled;             /* TRUE if logging was set up */
    hbool_t curr_logging;            /* TRUE if currently logging */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(type->serialize);
    HDassert(H5F_addr_defined(addr));
    HDassert(thing);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(f->shared->cache, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

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

#if H5AC_DO_TAGGING_SANITY_CHECKS
    if (!f->shared->cache->ignore_tags && (H5AC__verify_tag(dxpl_id, type) < 0))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "Bad tag value")
#endif /* H5AC_DO_TAGGING_SANITY_CHECKS */

    /* Insert entry into metadata cache */
    if(H5C_insert_entry(f, dxpl_id, type, addr, thing, flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "H5C_insert_entry() failed")

#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
        /* make note of the entry size */
        trace_entry_size = ((H5C_cache_entry_t *)thing)->size;
#endif /* H5AC__TRACE_FILE_ENABLED */

#ifdef H5_HAVE_PARALLEL
{
    H5AC_aux_t *aux_ptr;

    if(NULL != (aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(f->shared->cache))) {
        /* Log the new entry */
        if(H5AC__log_inserted_entry((H5AC_info_t *)thing) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINS, FAIL, "H5AC__log_inserted_entry() failed")

        /* Check if we should try to flush */
        if(aux_ptr->dirty_bytes >= aux_ptr->dirty_bytes_threshold)
            if(H5AC__run_sync_point(f, dxpl_id, H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't run sync point.")
    } /* end if */
}
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d %d\n", trace, (int)trace_entry_size, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */
    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_insert_entry_log_msg(f->shared->cache, addr, type->id, flags, ((H5C_cache_entry_t *)thing)->size, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
    hbool_t log_enabled;              /* TRUE if logging was set up */
    hbool_t curr_logging;             /* TRUE if currently logging */
    H5AC_info_t *entry_ptr = NULL;    /* Pointer to the cache entry */
    H5C_t *cache_ptr = NULL;          /* Pointer to the entry's associated metadata cache */
    herr_t ret_value = SUCCEED;       /* Return value */

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

    entry_ptr = (H5AC_info_t *)thing;
    cache_ptr = entry_ptr->cache_ptr;

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(cache_ptr, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

#ifdef H5_HAVE_PARALLEL
{
    H5AC_aux_t *aux_ptr;

    aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(cache_ptr);
    if((!entry_ptr->is_dirty) && (!entry_ptr->is_protected) &&
             (entry_ptr->is_pinned) && (NULL != aux_ptr))
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

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_mark_dirty_entry_log_msg(cache_ptr, entry_ptr, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
H5AC_move_entry(H5F_t *f, const H5AC_class_t *type, haddr_t old_addr, 
                haddr_t new_addr, hid_t dxpl_id)
{
#if H5AC__TRACE_FILE_ENABLED
    char          	trace[128] = "";
    FILE *        	trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
#ifdef H5_HAVE_PARALLEL
    H5AC_aux_t        *aux_ptr;
#endif /* H5_HAVE_PARALLEL */
    hbool_t log_enabled;           /* TRUE if logging was set up */
    hbool_t curr_logging;          /* TRUE if currently logging */
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(H5F_addr_defined(old_addr));
    HDassert(H5F_addr_defined(new_addr));
    HDassert(H5F_addr_ne(old_addr, new_addr));

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(f->shared->cache, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

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
    if(NULL != (aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(f->shared->cache)))
        if(H5AC__log_moved_entry(f, old_addr, new_addr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "can't log moved entry")
#endif /* H5_HAVE_PARALLEL */

    if(H5C_move_entry(f->shared->cache, type, old_addr, new_addr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTMOVE, FAIL, "H5C_move_entry() failed.")

#ifdef H5_HAVE_PARALLEL
    /* Check if we should try to flush */
    if(NULL != aux_ptr && aux_ptr->dirty_bytes >= aux_ptr->dirty_bytes_threshold)
        if(H5AC__run_sync_point(f, dxpl_id, H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't run sync point.")
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_move_entry_log_msg(f->shared->cache, old_addr, new_addr, type->id, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
    hbool_t log_enabled;                /* TRUE if logging was set up */
    hbool_t curr_logging;               /* TRUE if currently logging */
    H5AC_info_t *entry_ptr = NULL;      /* Pointer to the cache entry */
    H5C_t *cache_ptr = NULL;            /* Pointer to the entry's associated metadata cache */
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

    entry_ptr = (H5AC_info_t *)thing;
    cache_ptr = entry_ptr->cache_ptr;

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(cache_ptr, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

    /* Pin entry */
    if(H5C_pin_protected_entry(thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTPIN, FAIL, "can't pin entry")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_pin_entry_log_msg(cache_ptr, entry_ptr, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
    hbool_t log_enabled;                /* TRUE if logging was set up */
    hbool_t curr_logging;               /* TRUE if currently logging */
    H5AC_info_t *entry_ptr = NULL;      /* Pointer to the cache entry */
    H5C_t *cache_ptr = NULL;            /* Pointer to the entry's associated metadata cache */
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

    entry_ptr = (H5AC_info_t *)parent_thing;
    cache_ptr = entry_ptr->cache_ptr;

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(cache_ptr, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

    /* Create the flush dependency */
    if(H5C_create_flush_dependency(parent_thing, child_thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTDEPEND, FAIL, "H5C_create_flush_dependency() failed.")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_create_fd_log_msg(cache_ptr, (H5AC_info_t *)parent_thing, (H5AC_info_t *)child_thing, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
    void *udata, unsigned flags)
{
#if H5AC__TRACE_FILE_ENABLED
    char                trace[128] = "";
    size_t		trace_entry_size = 0;
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    void *thing = NULL;                 /* Pointer to native data structure for entry */
    hbool_t log_enabled;                /* TRUE if logging was set up */
    hbool_t curr_logging;               /* TRUE if currently logging */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(type->serialize);
    HDassert(H5F_addr_defined(addr));

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(f->shared->cache, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, "unable to get logging status")

    /* Check for unexpected flags -- H5C__FLUSH_COLLECTIVELY_FLAG
     * only permitted in the parallel case.
     */
#ifdef H5_HAVE_PARALLEL
    HDassert(0 == (flags & (unsigned)(~(H5C__READ_ONLY_FLAG | \
                                        H5C__FLUSH_LAST_FLAG | \
                                        H5C__FLUSH_COLLECTIVELY_FLAG))));
#else /* H5_HAVE_PARALLEL */
    HDassert(0 == (flags & (unsigned)(~(H5C__READ_ONLY_FLAG | \
                                        H5C__FLUSH_LAST_FLAG))));
#endif /* H5_HAVE_PARALLEL */

    /* Check for invalid access request */
    if((0 == (H5F_INTENT(f) & H5F_ACC_RDWR)) &&  (0 == (flags & H5C__READ_ONLY_FLAG)))
	HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, NULL, "no write intent on file")

#if H5AC__TRACE_FILE_ENABLED
    /* For the protect call, only the addr, size, type id, and flags are 
     * necessary in the trace file.  Also indicate whether the call was 
     * successful to catch occult errors.
     */
    if(NULL != (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        sprintf(trace, "%s 0x%lx %d 0x%x", FUNC, (unsigned long)addr,
		(int)(type->id), flags);
#endif /* H5AC__TRACE_FILE_ENABLED */

#if H5AC_DO_TAGGING_SANITY_CHECKS
    if (!f->shared->cache->ignore_tags && (H5AC__verify_tag(dxpl_id, type) < 0))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, NULL, "Bad tag value")
#endif /* H5AC_DO_TAGGING_SANITY_CHECKS */

    if(NULL == (thing = H5C_protect(f, dxpl_id, type, addr, udata, flags)))
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

    /* If currently logging, generate a message */
    if(curr_logging) {
        herr_t fake_ret_value = (NULL == ret_value) ? FAIL : SUCCEED;

        if(H5AC__write_protect_entry_log_msg(f->shared->cache, (H5AC_info_t *)thing, flags, fake_ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, NULL, "unable to emit log message")
    } /* end if */

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
    hbool_t log_enabled;              /* TRUE if logging was set up */
    hbool_t curr_logging;             /* TRUE if currently logging */
    H5AC_info_t *entry_ptr = NULL;    /* Pointer to the cache entry */
    H5C_t *cache_ptr = NULL;          /* Pointer to the entry's associated metadata cache */
    herr_t ret_value = SUCCEED;       /* Return value */

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

    entry_ptr = (H5AC_info_t *)thing;
    cache_ptr = entry_ptr->cache_ptr;

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(cache_ptr, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

    /* Resize the entry */
    if(H5C_resize_entry(thing, new_size) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTRESIZE, FAIL, "can't resize entry")

#ifdef H5_HAVE_PARALLEL
{
    H5AC_aux_t *aux_ptr;

    aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(cache_ptr);
    if((!entry_ptr->is_dirty) && (NULL != aux_ptr))
        if(H5AC__log_dirtied_entry(entry_ptr) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKDIRTY, FAIL, "can't log dirtied entry")
}
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_resize_entry_log_msg(cache_ptr, entry_ptr, new_size, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
    hbool_t log_enabled;              /* TRUE if logging was set up */
    hbool_t curr_logging;             /* TRUE if currently logging */
    H5AC_info_t *entry_ptr = NULL;    /* Pointer to the cache entry */
    H5C_t *cache_ptr = NULL;          /* Pointer to the entry's associated metadata cache */
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

    entry_ptr = (H5AC_info_t *)thing;
    cache_ptr = entry_ptr->cache_ptr;

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(cache_ptr, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

    /* Unpin the entry */
    if(H5C_unpin_entry(thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "can't unpin entry")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_unpin_entry_log_msg(cache_ptr, entry_ptr, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
    hbool_t log_enabled;                /* TRUE if logging was set up */
    hbool_t curr_logging;               /* TRUE if currently logging */
    H5AC_info_t *entry_ptr = NULL;      /* Pointer to the cache entry */
    H5C_t *cache_ptr = NULL;            /* Pointer to the entry's associated metadata cache */
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

    entry_ptr = (H5AC_info_t *)parent_thing;
    cache_ptr = entry_ptr->cache_ptr;

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(cache_ptr, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

    /* Destroy the flush dependency */
    if(H5C_destroy_flush_dependency(parent_thing, child_thing) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "H5C_destroy_flush_dependency() failed.")

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s %d\n", trace, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_destroy_fd_log_msg(cache_ptr, (H5AC_info_t *)parent_thing, (H5AC_info_t *)child_thing, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
#if H5AC__TRACE_FILE_ENABLED
    char                trace[128] = "";
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    hbool_t		dirtied;
    hbool_t		deleted;
#ifdef H5_HAVE_PARALLEL
    H5AC_aux_t        * aux_ptr = NULL;
#endif /* H5_HAVE_PARALLEL */
    hbool_t log_enabled;              /* TRUE if logging was set up */
    hbool_t curr_logging;             /* TRUE if currently logging */
    herr_t              ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(type);
    HDassert(type->deserialize);
    HDassert(type->image_len);
    HDassert(H5F_addr_defined(addr));
    HDassert(thing);
    HDassert( ((H5AC_info_t *)thing)->addr == addr );
    HDassert( ((H5AC_info_t *)thing)->type == type );

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(f->shared->cache, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

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
        hbool_t		curr_compressed = FALSE; /* dummy for call */
        size_t		curr_size = 0;
        size_t		curr_compressed_size = 0; /* dummy for call */

        if((type->image_len)(thing, &curr_size, &curr_compressed, &curr_compressed_size) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTGETSIZE, FAIL, "Can't get size of thing")

        if(((H5AC_info_t *)thing)->size != curr_size)
            HGOTO_ERROR(H5E_CACHE, H5E_BADSIZE, FAIL, "size of entry changed")
    } /* end if */

#ifdef H5_HAVE_PARALLEL
    if(NULL != (aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(f->shared->cache))) {
        if(dirtied && ((H5AC_info_t *)thing)->is_dirty == FALSE)
            if(H5AC__log_dirtied_entry((H5AC_info_t *)thing) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "can't log dirtied entry")

        if(deleted && aux_ptr->mpi_rank == 0)
            if(H5AC__log_deleted_entry((H5AC_info_t *)thing) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "H5AC__log_deleted_entry() failed.")
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    if(H5C_unprotect(f, dxpl_id, addr, thing, flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "H5C_unprotect() failed.")

#ifdef H5_HAVE_PARALLEL
    /* Check if we should try to flush */
    if((aux_ptr != NULL) && (aux_ptr->dirty_bytes >= aux_ptr->dirty_bytes_threshold))
        if(H5AC__run_sync_point(f, dxpl_id, H5AC_SYNC_POINT_OP__FLUSH_TO_MIN_CLEAN) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't run sync point.")
#endif /* H5_HAVE_PARALLEL */

done:
#if H5AC__TRACE_FILE_ENABLED
    if(trace_file_ptr != NULL)
	HDfprintf(trace_file_ptr, "%s 0x%x %d\n", trace, (unsigned)flags, (int)ret_value);
#endif /* H5AC__TRACE_FILE_ENABLED */

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_unprotect_entry_log_msg(f->shared->cache, (H5AC_info_t *)thing, type->id, flags, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_unprotect() */

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
    if((cache_ptr == NULL) || (config_ptr == NULL) ||
            (config_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr or config_ptr on entry.")
#ifdef H5_HAVE_PARALLEL
{
    H5AC_aux_t *aux_ptr;

    aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(cache_ptr);
    if((aux_ptr != NULL) && (aux_ptr->magic != H5AC__H5AC_AUX_T_MAGIC))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad aux_ptr on entry.")
}
#endif /* H5_HAVE_PARALLEL */

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
{
    H5AC_aux_t *aux_ptr;

    if(NULL != (aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(cache_ptr))) {
        config_ptr->dirty_bytes_threshold = aux_ptr->dirty_bytes_threshold;
	config_ptr->metadata_write_strategy = aux_ptr->metadata_write_strategy;
    } /* end if */
    else {
#endif /* H5_HAVE_PARALLEL */
        config_ptr->dirty_bytes_threshold = H5AC__DEFAULT_DIRTY_BYTES_THRESHOLD;
	config_ptr->metadata_write_strategy = H5AC__DEFAULT_METADATA_WRITE_STRATEGY;
#ifdef H5_HAVE_PARALLEL
    } /* end else */
}
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
#if H5AC__TRACE_FILE_ENABLED
    H5AC_cache_config_t trace_config = H5AC__DEFAULT_CACHE_CONFIG;
    FILE *              trace_file_ptr = NULL;
#endif /* H5AC__TRACE_FILE_ENABLED */
    hbool_t log_enabled;                /* TRUE if logging was set up */
    hbool_t curr_logging;               /* TRUE if currently logging */
    H5C_auto_size_ctl_t internal_config;
    herr_t  ret_value = SUCCEED;      	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache_ptr);

    /* Check if log messages are being emitted */
    if(H5C_get_logging_status(cache_ptr, &log_enabled, &curr_logging) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to get logging status")

#if H5AC__TRACE_FILE_ENABLED
    /* Make note of the new configuration.  Don't look up the trace file
     * pointer, as that may change before we use it.
     */
    if(config_ptr != NULL)
        trace_config = *config_ptr;
#endif /* H5AC__TRACE_FILE_ENABLED */

    if(cache_ptr == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad cache_ptr on entry.")
#ifdef H5_HAVE_PARALLEL
{
    H5AC_aux_t *aux_ptr;

    aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(cache_ptr);
    if((aux_ptr != NULL) && (aux_ptr->magic != H5AC__H5AC_AUX_T_MAGIC))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad aux_ptr on entry.")
}
#endif /* H5_HAVE_PARALLEL */

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
{
    H5AC_aux_t *aux_ptr;

    /* Set parallel configuration values */
    /* (Which are only held in the H5AC layer -QAK) */
    if(NULL != (aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(cache_ptr))) {
        aux_ptr->dirty_bytes_threshold = config_ptr->dirty_bytes_threshold;
        aux_ptr->metadata_write_strategy = config_ptr->metadata_write_strategy;
    } /* end if */
}
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

    /* If currently logging, generate a message */
    if(curr_logging)
        if(H5AC__write_set_cache_config_log_msg(cache_ptr, config_ptr, ret_value) < 0)
            HDONE_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

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
{
    H5AC_aux_t * aux_ptr;

    aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(cache_ptr);
    if(aux_ptr == NULL)
        sprintf(file_name, "%s", trace_file_name);
    else {
	if(aux_ptr->magic != H5AC__H5AC_AUX_T_MAGIC)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad aux_ptr->magic.")

        sprintf(file_name, "%s.%d", trace_file_name, aux_ptr->mpi_rank);
    } /* end else */

    if(HDstrlen(file_name) > H5AC__MAX_TRACE_FILE_NAME_LEN + H5C__PREFIX_LEN + 1)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "cooked trace file name too long.")
}
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
    aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(f->shared->cache);
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
    H5C_tag_t tag;                  /* Tag structure */
    H5P_genplist_t *dxpl;           /* Dataset transfer property list */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check Arguments */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_CACHE, H5E_BADTYPE, FAIL, "not a property list")

    /* Get the current tag value and return that (if prev_tag is NOT null) */
    if(prev_tag) {
        if((H5P_get(dxpl, "H5C_tag", &tag)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to query dxpl")
        *prev_tag = tag.value;
    } /* end if */

    /* Add metadata_tag to tag structure */
    tag.value = metadata_tag;

    /* Determine globality of tag */
    switch(metadata_tag) {
        case H5AC__SUPERBLOCK_TAG:
        case H5AC__SOHM_TAG:
        case H5AC__GLOBALHEAP_TAG:
            tag.globality = H5C_GLOBALITY_MAJOR;
            break;
        case H5AC__FREESPACE_TAG:
            tag.globality = H5C_GLOBALITY_MINOR;
            break;
        default:
            tag.globality = H5C_GLOBALITY_NONE;
            break;
    } /* end switch */

    /* Set the provided tag in the dxpl_id. */
    if(H5P_set(dxpl, "H5C_tag", &tag) < 0)
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
     
    /* Call cache-level function to re-tag entries with the COPIED tag */
    H5C_retag_entries(f->shared->cache, H5AC__COPIED_TAG, metadata_tag);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC_retag_copied_metadata */


/*------------------------------------------------------------------------------
 * Function:    H5AC_flush_tagged_metadata()
 *
 * Purpose:     Wrapper for cache level function which flushes all metadata
 *              that contains the specific tag. 
 * 
 * Return:      SUCCEED on success, FAIL otherwise.
 *
 * Programmer:  Mike McGreevy
 *              May 19, 2010
 *
 *------------------------------------------------------------------------------
 */
herr_t
H5AC_flush_tagged_metadata(H5F_t * f, haddr_t metadata_tag, hid_t dxpl_id)
{
    /* Variable Declarations */
    herr_t ret_value = SUCCEED;
 
    /* Function Enter Macro */   
    FUNC_ENTER_NOAPI(FAIL)

    /* Assertions */
    HDassert(f);
    HDassert(f->shared);

    /* Call cache level function to flush metadata entries with specified tag */
    if(H5C_flush_tagged_entries(f, dxpl_id, metadata_tag) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Cannot flush metadata")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_flush_tagged_metadata */


/*------------------------------------------------------------------------------
 * Function:    H5AC_evict_tagged_metadata()
 *
 * Purpose:     Wrapper for cache level function which flushes all metadata
 *              that contains the specific tag. 
 * 
 * Return:      SUCCEED on success, FAIL otherwise.
 *
 * Programmer:  Mike McGreevy
 *              May 19, 2010
 *
 *------------------------------------------------------------------------------
 */
herr_t
H5AC_evict_tagged_metadata(H5F_t * f, haddr_t metadata_tag, hid_t dxpl_id)
{
    /* Variable Declarations */
    herr_t ret_value = SUCCEED;
 
    /* Function Enter Macro */   
    FUNC_ENTER_NOAPI(FAIL)

    /* Assertions */
    HDassert(f);
    HDassert(f->shared);

    /* Call cache level function to evict metadata entries with specified tag */
    if(H5C_evict_tagged_entries(f, dxpl_id, metadata_tag)<0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Cannot evict metadata")

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5AC_evict_tagged_metadata */


/*-------------------------------------------------------------------------
 * Function:    H5AC_cork
 *
 * Purpose:     To cork/uncork/get cork status for an object
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_cork(H5F_t *f, haddr_t obj_addr, unsigned action, hbool_t *corked)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);
    HDassert(H5F_addr_defined(obj_addr));
    HDassert(action == H5AC__SET_CORK || action == H5AC__UNCORK || action == H5AC__GET_CORKED);

    if(action == H5AC__GET_CORKED)
	HDassert(corked);

    if(H5C_cork(f->shared->cache, obj_addr, action, corked) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Cannot perform the cork action")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_cork() */

#if H5AC_DO_TAGGING_SANITY_CHECKS

/*-------------------------------------------------------------------------
 *
 * Function:    H5AC__verify_tag
 *
 * Purpose:     Performs sanity checking on an entry type and tag value
 *              stored in a supplied dxpl_id.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              October 20, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__verify_tag(hid_t dxpl_id, const H5AC_class_t * type)
{
    H5C_tag_t tag;
    H5P_genplist_t * dxpl;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Get the dataset transfer property list */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Get the tag from the DXPL */
    if( (H5P_get(dxpl, "H5C_tag", &tag)) < 0 )
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to query property value");

    /* Perform some sanity checks on tag value. Certain entry
     * types require certain tag values, so check that these
     * constraints are met. */
    if(tag.value == H5AC__IGNORE_TAG)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "cannot ignore a tag while doing verification.")
    else if(tag.value == H5AC__INVALID_TAG)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "no metadata tag provided")
    else {

        /* Perform some sanity checks on tag value. Certain entry
         * types require certain tag values, so check that these
         * constraints are met. */

        /* Superblock */
        if(type->id == H5AC_SUPERBLOCK_ID || type->id == H5AC_DRVRINFO_ID) {
            if(tag.value != H5AC__SUPERBLOCK_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "superblock/driver-info not tagged with H5AC__SUPERBLOCK_TAG")
            if(tag.globality != H5C_GLOBALITY_MAJOR)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "superblock/driver-info globality not marked with H5C_GLOBALITY_MAJOR")
        }
        else {
            if(tag.value == H5AC__SUPERBLOCK_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__SUPERBLOCK_TAG applied to non-superblock entry")
        }
    
        /* Free Space Manager */
        if((type->id == H5AC_FSPACE_HDR_ID) || (type->id == H5AC_FSPACE_SINFO_ID)) {
            if(tag.value != H5AC__FREESPACE_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "freespace entry not tagged with H5AC__FREESPACE_TAG")
            if(tag.globality != H5C_GLOBALITY_MINOR)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "freespace entry globality not marked with H5C_GLOBALITY_MINOR")
        }
        else {
            if(tag.value == H5AC__FREESPACE_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__FREESPACE_TAG applied to non-freespace entry")
        }
    
        /* SOHM */
        if((type->id == H5AC_SOHM_TABLE_ID) || (type->id == H5AC_SOHM_LIST_ID)) { 
            if(tag.value != H5AC__SOHM_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "sohm entry not tagged with H5AC__SOHM_TAG")
            if(tag.globality != H5C_GLOBALITY_MAJOR)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "sohm entry globality not marked with H5C_GLOBALITY_MAJOR")
        }
    
        /* Global Heap */
        if(type->id == H5AC_GHEAP_ID) {
            if(tag.value != H5AC__GLOBALHEAP_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "global heap not tagged with H5AC__GLOBALHEAP_TAG")
            if(tag.globality != H5C_GLOBALITY_MAJOR)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "global heap entry globality not marked with H5C_GLOBALITY_MAJOR")
        }
        else {
            if(tag.value == H5AC__GLOBALHEAP_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__GLOBALHEAP_TAG applied to non-globalheap entry")
        }
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__verify_tag */
#endif /* H5AC_DO_TAGGING_SANITY_CHECKS */


/*-------------------------------------------------------------------------
 * Function:    H5AC_get_entry_ring
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
H5AC_get_entry_ring(const H5F_t *f, haddr_t addr, H5AC_ring_t *ring)
{
    herr_t      ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(ring);

    /* Retrieve the ring value for the entry at address */
    if(H5C_get_entry_ring(f, addr, ring) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "Can't retrieve ring for entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_get_entry_ring() */


/*-------------------------------------------------------------------------
 * Function:       H5AC_set_ring
 *
 * Purpose:        Routine to set the ring on a DXPL (for passing through
 *                 to the metadata cache).
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Quincey Koziol
 *                 Tuesday, September 8, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_set_ring(hid_t dxpl_id, H5AC_ring_t ring, H5P_genplist_t **dxpl,
    H5AC_ring_t *orig_ring)
{
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(dxpl);
    HDassert(orig_ring);

    /* Set the ring type in the DXPL */
    if(NULL == ((*dxpl) = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")
    if((H5P_get((*dxpl), H5AC_RING_NAME, orig_ring)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get original ring value")
    if((H5P_set((*dxpl), H5AC_RING_NAME, &ring)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set ring value")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_set_ring() */


/*-------------------------------------------------------------------------
 * Function:       H5AC_reset_ring
 *
 * Purpose:        Routine to reset the original ring on a DXPL (after passing
 *                 through to the metadata cache).
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Quincey Koziol
 *                 Tuesday, September 8, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_reset_ring(H5P_genplist_t *dxpl, H5AC_ring_t orig_ring)
{
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Reset the ring in the DXPL, if it's been changed */
    if(orig_ring) {
        /* Sanity check */
        HDassert(dxpl);

        if((H5P_set(dxpl, H5AC_RING_NAME, &orig_ring)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set property value")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_reset_ring() */

