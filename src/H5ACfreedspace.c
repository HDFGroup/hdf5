/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Created:             H5ACfreedspace.c
 *
 * Purpose:             Functions for cache freed space management.
 *                      Reused code from H5ACproxy_entry.c
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#include "H5ACmodule.h"         /* This source code file is part of the H5AC module */

/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5ACpkg.h"            /* Metadata cache                       */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5ACprivate.h"


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

typedef struct H5AC_freedspace_ctx_t {
    H5F_t      *f;
    hid_t      dxpl_id;
    int        ring;
    H5AC_freedspace_t *fs;
} H5AC_freedspace_ctx_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static int H5AC__freedspace_create_cb(H5C_cache_entry_t *entry, void *_ctx);

/* Metadata cache (H5AC) callbacks */
static herr_t H5AC__freedspace_notify(H5AC_notify_action_t action, void *_thing, ...);
static herr_t H5AC__freedspace_image_len(const void *thing, size_t *image_len);
static herr_t H5AC__freedspace_serialize(const H5F_t *f, void *image_ptr,
            size_t len, void *thing);
static herr_t H5AC__freedspace_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/

/* H5AC freedspace entries inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_FREEDSPACE[1] = {{
    H5AC_FREEDSPACE_ID,               	/* Metadata client ID */
    "Freed space",           		/* Metadata client name (for debugging) */
    H5FD_MEM_SUPER,                     /* File space memory type for client */
    0,					/* Client class behavior flags */
    NULL,    				/* 'get_initial_load_size' callback */
    NULL,    				/* 'get_final_load_size' callback */
    NULL,				/* 'verify_chksum' callback */
    NULL,    				/* 'deserialize' callback */
    H5AC__freedspace_image_len,	        /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5AC__freedspace_serialize,	        /* 'serialize' callback */
    H5AC__freedspace_notify,		/* 'notify' callback */
    H5AC__freedspace_free_icr,          /* 'free_icr' callback */
    NULL,                              	/* 'fsf_size' callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage H5AC_freedspace_t objects */
H5FL_DEFINE_STATIC(H5AC_freedspace_t);



/*-------------------------------------------------------------------------
 * Function:    H5AC__freedspace_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Houjun Tang
 *              June 8, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__freedspace_image_len(const void H5_ATTR_UNUSED *thing, size_t *image_len)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image_len);

    /* Set the image length size to 1 byte */
    *image_len = 1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5AC__freedspace_image_len() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__freedspace_serialize
 *
 * Purpose:     Serializes a data structure for writing to disk.
 *
 * Note:        Should never be invoked.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Houjun Tang
 *              June 8, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__freedspace_serialize(const H5F_t H5_ATTR_UNUSED *f, void H5_ATTR_UNUSED *image,
    size_t H5_ATTR_UNUSED len, void H5_ATTR_UNUSED *thing)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    /* Should never be invoked */
    HDassert(0 && "Invalid callback?!?");

    HERROR(H5E_CACHE, H5E_CANTSERIALIZE, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5AC__freedspace_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__freedspace_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Houjun Tang
 *              June 8, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__freedspace_free_icr(void *_thing)
{
    H5AC_freedspace_t *pentry = (H5AC_freedspace_t *)_thing;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_STATIC

    /* Destroy the freedspace entry */
    if(H5AC_freedspace_dest(pentry) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "unable to destroy freedspace entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__freedspace_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_freedspace_create_cb()
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
H5AC__freedspace_create_cb(H5C_cache_entry_t *entry, void *_ctx)
{
    H5AC_freedspace_ctx_t *ctx = (H5AC_freedspace_ctx_t*)_ctx;   /* Callback context */
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
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, H5_ITER_ERROR, "unable to get entry type")

    /* Don't create flush dependency on clean entries or cache-internal ones */
    if(type_id != H5AC_FREEDSPACE_ID && type_id != H5AC_PROXY_ENTRY_ID
            && type_id != H5AC_EPOCH_MARKER_ID && type_id != H5AC_PREFETCHED_ENTRY_ID
            && entry->is_dirty) {

        /* Only create flush dependencies on entries in rings which will be flushed same / earlier */
        if(entry->ring <= ctx->ring) {
            if(ctx->fs == NULL) {
                haddr_t fs_addr;        /* Address of freedspace entry */

                /* Allocate new freedspace object */
                if(NULL == (ctx->fs = H5FL_CALLOC(H5AC_freedspace_t)))
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, H5_ITER_ERROR, "can't allocate freed space entry")

                /* Allocate a temporary address for the freedspace entry */
                if(HADDR_UNDEF == (fs_addr = H5MF_alloc_tmp(ctx->f, 1)))
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, H5_ITER_ERROR, "can't allocate temporary space for freed space entry")

                /* Set the ring for the new freedspace entry */
                if(H5AC_set_ring(ctx->dxpl_id, ctx->ring, &dxpl, &orig_ring) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, H5_ITER_ERROR, "unable to set ring value")
                reset_ring = TRUE;

                /* Insert freedspace entry into the cache */
                if(H5AC_insert_entry(ctx->f, ctx->dxpl_id, H5AC_FREEDSPACE, fs_addr, ctx->fs, H5AC__PIN_ENTRY_FLAG) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, H5_ITER_ERROR, "unable to insert freedspace")
            } /* if ctx->fs == NULL */

            /* Create flush dependency between the freedspace entry and the dirty entry */
            if(H5AC_create_flush_dependency(ctx->fs, entry) < 0)
               HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, H5_ITER_ERROR, "can't create flush dependency")
        } /* end if */
    } /* end if */

done:
    /* Reset the ring in the DXPL */
    if(reset_ring)
        if(H5AC_reset_ring(dxpl, orig_ring) < 0)
            HDONE_ERROR(H5E_CACHE, H5_ITER_ERROR, FAIL, "unable to reset ring value")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_freedspace_create_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_freedspace_create
 *
 * Purpose:     Create a new freedspace entry
 *
 * Return:	Success:	Pointer to the new freedspace entry object.
 *		Failure:	NULL
 *
 * Programmer:  Houjun Tang
 *              June 8, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_freedspace_create(H5F_t *f, hid_t dxpl_id, haddr_t client_addr,
    herr_t (*cb)(void *), void *client_ctx, H5AC_freedspace_t** fs)
{
    herr_t ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(H5F_addr_defined(client_addr));
    HDassert(cb);
    HDassert(fs);
    HDassert(NULL == *fs);

    /* Check if there's any dirty entries in the cache currently */
    if(H5AC_has_dirty_entry(f)) {
        unsigned status = 0;    /* Cache entry status for address being freed */

        /* Check cache status of address being freed */
        if(H5AC_get_entry_status(f, client_addr, &status) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "unable to get entry status")

        /* If the entry is in the cache, attempt to create a freedspace entry for it */
        if((status & H5AC_ES__IN_CACHE) != 0) {
            H5AC_freedspace_ctx_t fs_ctx;       /* Context for cache iteration */

            /* Create a freed space ctx */
            fs_ctx.fs          = NULL;
            fs_ctx.f           = f;
            fs_ctx.dxpl_id     = dxpl_id;
            if(H5AC_get_entry_ring(f, client_addr, &fs_ctx.ring) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't get ring of entry")

            /* Iterate through cache entries, setting up flush dependencies */
            if(H5AC_iterate(f, H5AC__freedspace_create_cb, &fs_ctx) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "unable to iterate cache entries")

            /* If there were entries in the cache that set up flush dependencies, finish setting up freedspace object */
            if(NULL != fs_ctx.fs) {
                /* Set the callback and client context */
                fs_ctx.fs->cb = cb;
                fs_ctx.fs->client_ctx = client_ctx;

                /* Set the pointer to the freedspace entry, for the calling routine */
                *fs = fs_ctx.fs;
            } /* end if */
        } /* end if */

        /* Add sanity check to make certain that freedspace entry has at least one flush dependency child entry */
    } /* end if */

done:
    /* Release resources on error */
    if(ret_value < 0)
        if(*fs)
            *fs = H5FL_FREE(H5AC_freedspace_t, *fs);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_freedspace_create() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_freedspace_dest
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
H5AC_freedspace_dest(H5AC_freedspace_t *pentry)
{
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pentry);

    /* Free the freedspace entry object */
    pentry = H5FL_FREE(H5AC_freedspace_t, pentry);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_freedspace_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__freedspace_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Houjun Tang
 *              June 8, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__freedspace_notify(H5AC_notify_action_t action, void *_thing, ...)
{
    va_list ap;
    hbool_t va_started = FALSE;     /* Whether the variable argument list is open */
    H5AC_freedspace_t *pentry;      /* Pointer to freedspace entry being notified */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_STATIC

    va_start (ap, _thing);         /* Initialize the argument list. */
    va_started = TRUE;

    pentry = (H5AC_freedspace_t*)_thing;
    HDassert(pentry);

    switch(action) {
        case H5AC_NOTIFY_ACTION_CHILD_BEFORE_EVICT:
        case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
            {
                H5C_cache_entry_t *child_entry; /* Child entry */
                haddr_t child_addr;             /* Address of child entry */
                unsigned nchildren;             /* # of flush dependency children for freedspace entry */

                /* The child freedspace entry address in the varg */
                child_addr = va_arg(ap, haddr_t);
                HDassert(H5F_addr_defined(child_addr));

                /* Get the cache entry for the child */
                if(NULL == (child_entry = (H5C_cache_entry_t *)H5C_get_entry_from_addr(pentry->cache_info.cache_ptr, child_addr)))
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't get cache entry from address")

                /* Remove flush dependency on cleaned or evicted child */
                if(H5AC_destroy_flush_dependency(pentry, child_entry) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency on freedspace entry")

                /* Get # of flush dependency children now */
                if(H5C_get_flush_dep_nchildren((H5C_cache_entry_t *)pentry, &nchildren) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't get cache entry nchildren")

                /* If the # of children drops to zero, invoke the callback and delete the freedspace entry */
                if(0 == nchildren ) {
                    /* Invoke callback registered when freedspace entry was created */
                    if(pentry->cb(pentry->client_ctx) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CALLBACK, FAIL, "freedspace client callback failed")

                    if(H5AC_mark_entry_clean(pentry) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTMARKCLEAN, FAIL, "can't mark entry clean")
                    if(H5AC_unpin_entry(pentry) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "can't unpin entry")
                    if(H5AC_remove_entry(pentry) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTREMOVE, FAIL, "can't remove entry")

                    if(H5AC_freedspace_dest(pentry) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTDELETE, FAIL, "can't destroy freedspace")
                } /* end if */
            } /* end case */
            break;

        case H5AC_NOTIFY_ACTION_CHILD_UNDEPEND_DIRTY:
            /* Ignore notification about undepending a dirty child, since it's
             *  the result of removing the dependency from the "child before evict"
             *  case above.  QAK - 2017/08/05
             */
            break;

        case H5C_NOTIFY_ACTION_AFTER_INSERT:
        case H5C_NOTIFY_ACTION_AFTER_LOAD:
        case H5C_NOTIFY_ACTION_AFTER_FLUSH:
        case H5C_NOTIFY_ACTION_ENTRY_DIRTIED:
        case H5C_NOTIFY_ACTION_ENTRY_CLEANED:
        case H5C_NOTIFY_ACTION_BEFORE_EVICT:
        case H5C_NOTIFY_ACTION_CHILD_UNSERIALIZED:
        case H5C_NOTIFY_ACTION_CHILD_SERIALIZED:
        case H5C_NOTIFY_ACTION_CHILD_DIRTIED:
            break;

        default:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "unknown notify action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
    } /* end switch */

done:
    if(va_started)
        va_end(ap);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC__freedspace_notify() */

