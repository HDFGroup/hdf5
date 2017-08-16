/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright the Regents of the University of California, through            *
 *  Lawrence Berkeley National Laboratory                                    *
 *  (subject to receipt of any required approvals from U.S. Dept. of Energy).*
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5MFfreedspace.c
 *
 * Purpose:             Functions for cache freed space management.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#include "H5MFmodule.h"         /* This source code file is part of the H5MF module */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5MFpkg.h"		/* File memory management		*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Context for iterator callback */
typedef struct H5MF_freedspace_ctx_t {
    /* Down */
    H5F_t      *f;              /* File where space is being freed */
    hid_t      dxpl_id;         /* DXPL for free operation */
    int        ring;            /* Metadata cache ring for object who's space is freed */
    H5FD_mem_t alloc_type;      /* File space type for freed space */
    haddr_t    addr;            /* Address for freed space */
    hsize_t    size;            /* Size for freed space */

    /* Up */
    H5MF_freedspace_t *fs;      /* Freespace object created */
} H5MF_freedspace_ctx_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static int H5MF__freedspace_create_cb(H5AC_info_t *entry, void *_ctx);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage H5MF_freedspace_t objects */
H5FL_DEFINE_STATIC(H5MF_freedspace_t);



/*-------------------------------------------------------------------------
 * Function:    H5MF__freedspace_create_cb()
 *
 * Purpose:     Cache iteration callback, creates flush dep
 *
 * Return:      0 on success, non-zero on failure
 *
 * Programmer:  Houjun Tang
 *              June 8, 2017
 *
 *-------------------------------------------------------------------------
 */
static int
H5MF__freedspace_create_cb(H5AC_info_t *entry, void *_ctx)
{
    H5MF_freedspace_ctx_t *ctx = (H5MF_freedspace_ctx_t*)_ctx;   /* Callback context */
    H5P_genplist_t *dxpl = NULL;            /* DXPL for setting ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;  /* Original ring value */
    hbool_t reset_ring = FALSE;             /* Whether the ring was set */
    int type_id;                            /* Cache client for entry */
    int ret_value = H5_ITER_CONT;           /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(entry);
    HDassert(ctx);

    /* Retrieve type for entry */
    if((type_id = H5AC_get_entry_type(entry)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, H5_ITER_ERROR, "unable to get entry type")

    /* Don't create flush dependency on clean entries or cache-internal ones */
    if(type_id != H5AC_FREEDSPACE_ID && type_id != H5AC_PROXY_ENTRY_ID
            && type_id != H5AC_EPOCH_MARKER_ID && type_id != H5AC_PREFETCHED_ENTRY_ID
            && entry->is_dirty) {

        /* Only create flush dependencies on entries in rings which will be flushed same / earlier */
        if(entry->ring <= ctx->ring) {
            if(ctx->fs == NULL) {
                haddr_t fs_addr;        /* Address of freedspace entry */

                /* Allocate new freedspace object */
                if(NULL == (ctx->fs = H5FL_CALLOC(H5MF_freedspace_t)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, H5_ITER_ERROR, "can't allocate freed space entry")

                /* Initialize new freedspace object */
                ctx->fs->f           = ctx->f;
                ctx->fs->dxpl_id     = ctx->dxpl_id;
                ctx->fs->alloc_type  = ctx->alloc_type;
                ctx->fs->addr        = ctx->addr;
                ctx->fs->size        = ctx->size;

                /* Allocate a temporary address for the freedspace entry */
                if(HADDR_UNDEF == (fs_addr = H5MF_alloc_tmp(ctx->f, 1)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, H5_ITER_ERROR, "can't allocate temporary space for freed space entry")

                /* Set the ring for the new freedspace entry in the cache */
                if(H5AC_set_ring(ctx->dxpl_id, ctx->ring, &dxpl, &orig_ring) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, H5_ITER_ERROR, "unable to set ring value")
                reset_ring = TRUE;

                /* Insert freedspace entry into the cache */
                if(H5AC_insert_entry(ctx->f, ctx->dxpl_id, H5AC_FREEDSPACE, fs_addr, ctx->fs, H5AC__PIN_ENTRY_FLAG) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINSERT, H5_ITER_ERROR, "unable to insert freedspace")
            } /* if ctx->fs == NULL */

            /* Create flush dependency between the freedspace entry and the dirty entry */
            if(H5AC_create_flush_dependency(ctx->fs, entry) < 0)
               HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCREATE, H5_ITER_ERROR, "can't create flush dependency")
        } /* end if */
    } /* end if */

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5_ITER_ERROR, FAIL, "unable to reset ring value")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MF__freedspace_create_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5MF__freedspace_create
 *
 * Purpose:     Create a new freedspace entry
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Houjun Tang
 *              June 8, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF__freedspace_create(H5F_t *f, hid_t dxpl_id, H5FD_mem_t alloc_type,
    haddr_t addr, hsize_t size, H5MF_freedspace_t** fs)
{
    htri_t cache_clean;         /* Whether the cache has any dirty entries */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(fs);
    HDassert(NULL == *fs);

    /* Check if there's any dirty entries in the cache currently */
    if((cache_clean = H5AC_cache_is_clean(f, H5AC_RING_SB)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "unable to check for dirty entries in cache")

    /* Check if there's any dirty entries in the cache currently */
    if(!cache_clean) {
        unsigned status = 0;    /* Cache entry status for address being freed */

        /* Check cache status of address being freed */
        if(H5AC_get_entry_status(f, addr, &status) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "unable to get entry status")

        /* If the entry is in the cache, attempt to create a freedspace entry for it */
        if((status & H5AC_ES__IN_CACHE) != 0) {
            H5MF_freedspace_ctx_t fs_ctx;       /* Context for cache iteration */

            /* Create a freed space ctx */
            fs_ctx.f           = f;
            fs_ctx.dxpl_id     = dxpl_id;
            if(H5AC_get_entry_ring(f, addr, &fs_ctx.ring) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't get ring of entry")
            fs_ctx.alloc_type  = alloc_type;
            fs_ctx.addr        = addr;
            fs_ctx.size        = size;
            fs_ctx.fs          = NULL;

            /* Iterate through cache entries, setting up flush dependencies */
            if(H5AC_iterate(f, H5MF__freedspace_create_cb, &fs_ctx) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_BADITER, FAIL, "unable to iterate cache entries")

            /* If there were entries in the cache that set up flush dependencies, finish setting up freedspace object */
            if(NULL != fs_ctx.fs) {
#if defined H5_DEBUG_BUILD
{
    unsigned nchildren;         /* # of flush dependency children for freedspace object */

    /* Get # of flush dependency children now */
    if(H5AC_get_flush_dep_nchildren((H5AC_info_t *)fs_ctx.fs, &nchildren) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't get cache entry nchildren")

    /* Sanity check to make certain that freedspace entry has at least one flush dependency child entry */
    if(0 == nchildren)
        HGOTO_ERROR(H5E_RESOURCE, H5E_BADVALUE, FAIL, "no flush dependency children for new freedspace object")
}
#endif /* H5_DEBUG_BUILD */

                /* Set the pointer to the freedspace entry, for the calling routine */
                *fs = fs_ctx.fs;
            } /* end if */
        } /* end if */
    } /* end if */

done:
    /* Release resources on error */
    if(ret_value < 0)
        if(*fs)
            *fs = H5FL_FREE(H5MF_freedspace_t, *fs);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF__freedspace_create() */


/*-------------------------------------------------------------------------
 * Function:    H5MF__freedspace_dest
 *
 * Purpose:     Destroys a freedspace entry in memory.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Houjun Tang
 *              June 8, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF__freedspace_dest(H5MF_freedspace_t *freedspace)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity checks */
    HDassert(freedspace);

    /* Free the freedspace entry object */
    freedspace = H5FL_FREE(H5MF_freedspace_t, freedspace);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5MF__freedspace_dest() */

