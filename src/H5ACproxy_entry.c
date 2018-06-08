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
 * Created:             H5ACproxy_entry.c
 *
 * Purpose:             Functions and a cache client for a "proxy" cache entry.
 *			A proxy cache entry is used as a placeholder for entire
 *			data structures to attach flush dependencies, etc.
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


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Proxy entry parent list node */
typedef struct H5AC_proxy_parent_t {
    struct H5AC_proxy_parent_t *next;   /* Pointer to next node in parent list */
    struct H5AC_proxy_parent_t *prev;   /* Pointer to previous node in parent list */
    H5AC_info_t *parent;                /* Pointer to parent entry */
} H5AC_proxy_parent_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */
static herr_t H5AC__proxy_entry_image_len(const void *thing, size_t *image_len);
static herr_t H5AC__proxy_entry_serialize(const H5F_t *f, void *image_ptr,
    size_t len, void *thing);
static herr_t H5AC__proxy_entry_notify(H5AC_notify_action_t action, void *thing, ...);
static herr_t H5AC__proxy_entry_free_icr(void *thing);

/*********************/
/* Package Variables */
/*********************/

/* H5AC proxy entries inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_PROXY_ENTRY[1] = {{
    H5AC_PROXY_ENTRY_ID,               	/* Metadata client ID */
    "Proxy entry",           		/* Metadata client name (for debugging) */
    H5FD_MEM_SUPER,                     /* File space memory type for client */
    0,					/* Client class behavior flags */
    NULL,    				/* 'get_initial_load_size' callback */
    NULL,    				/* 'get_final_load_size' callback */
    NULL,				/* 'verify_chksum' callback */
    NULL,    				/* 'deserialize' callback */
    H5AC__proxy_entry_image_len,	/* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5AC__proxy_entry_serialize,	/* 'serialize' callback */
    H5AC__proxy_entry_notify,		/* 'notify' callback */
    H5AC__proxy_entry_free_icr,        	/* 'free_icr' callback */
    NULL,                              	/* 'fsf_size' callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage H5AC_proxy_entry_t objects */
H5FL_DEFINE_STATIC(H5AC_proxy_entry_t);

/* Declare a free list to manage H5AC_proxy_parent_t objects */
H5FL_DEFINE_STATIC(H5AC_proxy_parent_t);



/*-------------------------------------------------------------------------
 * Function:    H5AC_proxy_entry_create
 *
 * Purpose:     Create a new proxy entry
 *
 * Return:	Success:	Pointer to the new proxy entry object.
 *		Failure:	NULL
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
H5AC_proxy_entry_t *
H5AC_proxy_entry_create(H5F_t *f)
{
    H5AC_proxy_entry_t *pentry = NULL;  /* Pointer to new proxy entry */
    H5AC_proxy_entry_t *ret_value = NULL;       /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Allocate new proxy entry */
    if(NULL == (pentry = H5FL_CALLOC(H5AC_proxy_entry_t)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, NULL, "can't allocate proxy entry")

    /* Set non-zero fields */
    pentry->f = f;
    pentry->addr = HADDR_UNDEF;

    /* Set return value */
    ret_value = pentry;

done:
    /* Release resources on error */
    if(!ret_value)
        if(pentry)
            pentry = H5FL_FREE(H5AC_proxy_entry_t, pentry);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_proxy_entry_create() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_proxy_entry_add_parent
 *
 * Purpose:     Add a parent to a proxy entry
 *
 * Note:	Defers creating flush dependencies on parents until there are
 *		children of the proxy entry (because the proxy entry can't be
 *		dirty until it has children).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_proxy_entry_add_parent(H5AC_proxy_entry_t *pentry, void *_parent)
{
    H5AC_info_t *parent = (H5AC_info_t *)_parent; /* Parent entry's cache info */
    H5AC_proxy_parent_t *proxy_parent = NULL;   /* Proxy parent node for new parent */
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pentry);
    HDassert(parent);

    /* Allocate a proxy parent node */
    if(NULL == (proxy_parent = H5FL_MALLOC(H5AC_proxy_parent_t)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "can't allocate proxy parent node")

    /* Set (parent) entry for proxy parent node */
    proxy_parent->parent = parent;

    /* Add proxy parent node into list */
    proxy_parent->prev = NULL;
    proxy_parent->next = pentry->parents;
    if(pentry->parents)
        pentry->parents->prev = proxy_parent;
    pentry->parents = proxy_parent;

    /* Add flush dependency on parent, if the proxy entry has children */
    if(pentry->nchildren > 0) {
        /* Sanity check */
        HDassert(H5F_addr_defined(pentry->addr));

        if(H5AC_create_flush_dependency(parent, pentry) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTDEPEND, FAIL, "unable to set flush dependency on proxy entry")
    } /* end if */

done:
    if(ret_value < 0)
        if(proxy_parent) {
            /* Unlink from list, if in it */
            if(pentry->parents == proxy_parent) {
                if(proxy_parent->next)
                    proxy_parent->next->prev = NULL;
                pentry->parents = proxy_parent->next;
            } /* end if */

            /* Free the proxy parent node */
            proxy_parent = H5FL_FREE(H5AC_proxy_parent_t, proxy_parent);
        } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_proxy_entry_add_parent() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_proxy_entry_remove_parent
 *
 * Purpose:     Removes a parent from a proxy entry
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_proxy_entry_remove_parent(H5AC_proxy_entry_t *pentry, void *_parent)
{
    H5AC_info_t *parent = (H5AC_info_t *)_parent;   /* Pointer to the parent entry */
    H5AC_proxy_parent_t *proxy_parent;  /* Entry's parent node in proxy */
    herr_t ret_value = SUCCEED;        	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pentry);
    HDassert(pentry->parents);
    HDassert(parent);

    /* Find proxy parent in list */
    /* (Linear search, but these lists probably aren't huge) */
    proxy_parent = pentry->parents;
    while(proxy_parent) {
        /* Check for entry being removed */
        if(proxy_parent->parent == parent)
            break;

        /* Advance to next proxy parent node */
        proxy_parent = proxy_parent->next;
    } /* end while */
    if(NULL == proxy_parent)
        HGOTO_ERROR(H5E_CACHE, H5E_NOTFOUND, FAIL, "unable to find proxy entry parent in list")

    /* Remove proxy parent from list */
    if(pentry->parents == proxy_parent) {
        /* Sanity check */
        HDassert(NULL == proxy_parent->prev);

        /* Detech from head of list */
        if(proxy_parent->next)
            proxy_parent->next->prev = NULL;
        pentry->parents = proxy_parent->next;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(proxy_parent->prev);

        /* Detach from middle or end of list */
        proxy_parent->prev->next = proxy_parent->next;
        if(proxy_parent->next)
            proxy_parent->next->prev = proxy_parent->prev;
    } /* end else */

    /* Free the proxy parent node */
    proxy_parent = H5FL_FREE(H5AC_proxy_parent_t, proxy_parent);

    /* Remove flush dependency between the proxy entry and a parent */
    if(pentry->nchildren > 0)
        if(H5AC_destroy_flush_dependency(parent, pentry) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency on proxy entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_proxy_entry_remove_parent() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_proxy_entry_add_child
 *
 * Purpose:     Add a child a proxy entry
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_proxy_entry_add_child(H5AC_proxy_entry_t *pentry, void *child)
{
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pentry);
    HDassert(child);

    /* Check for first child */
    if(0 == pentry->nchildren) {
        /* Get an address, if the proxy doesn't already have one */
        if(!H5F_addr_defined(pentry->addr))
            if(HADDR_UNDEF == (pentry->addr = H5MF_alloc_tmp(pentry->f, 1)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "temporary file space allocation failed for proxy entry")

        /* Insert the proxy entry into the cache */
        if(H5AC_insert_entry(pentry->f, H5AC_PROXY_ENTRY, pentry->addr, pentry, H5AC__PIN_ENTRY_FLAG) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "unable to cache proxy entry")

        /* Proxies start out clean (insertions are automatically marked dirty) */
        if(H5AC_mark_entry_clean(pentry) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTCLEAN, FAIL, "can't mark proxy entry clean")

        /* Proxies start out serialized (insertions are automatically marked unserialized) */
        if(H5AC_mark_entry_serialized(pentry) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTSERIALIZE, FAIL, "can't mark proxy entry clean")

        /* If there are currently parents, iterate over the list of parents, creating flush dependency on them */
        if(pentry->parents) {
            H5AC_proxy_parent_t *proxy_parent;  /* Entry's parent node in proxy */

            /* Iterate over proxy parent nodes */
            proxy_parent = pentry->parents;
            while(proxy_parent) {
                /* Add flush dependency on parent for proxy entry */
                if(H5AC_create_flush_dependency(proxy_parent->parent, pentry) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTDEPEND, FAIL, "unable to set flush dependency for proxy entry")

                /* Advance to next proxy parent node */
                proxy_parent = proxy_parent->next;
            } /* end while */
        } /* end if */
    } /* end if */

    /* Add flush dependency on proxy entry */
    if(H5AC_create_flush_dependency(pentry, child) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTDEPEND, FAIL, "unable to set flush dependency on proxy entry")

    /* Increment count of children */
    pentry->nchildren++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_proxy_entry_add_child() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_proxy_entry_remove_child
 *
 * Purpose:     Remove a child a proxy entry
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_proxy_entry_remove_child(H5AC_proxy_entry_t *pentry, void *child)
{
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pentry);
    HDassert(child);

    /* Remove flush dependency on proxy entry */
    if(H5AC_destroy_flush_dependency(pentry, child) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency on proxy entry")

    /* Decrement count of children */
    pentry->nchildren--;

    /* Check for last child */
    if(0 == pentry->nchildren) {
        /* Check for flush dependencies on proxy's parents */
        if(pentry->parents) {
            H5AC_proxy_parent_t *proxy_parent;  /* Entry's parent node in proxy */

            /* Iterate over proxy parent nodes */
            proxy_parent = pentry->parents;
            while(proxy_parent) {
                /* Remove flush dependency on parent for proxy entry */
                if(H5AC_destroy_flush_dependency(proxy_parent->parent, pentry) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency for proxy entry")

                /* Advance to next proxy parent node */
                proxy_parent = proxy_parent->next;
            } /* end while */
        } /* end if */

        /* Unpin proxy */
        if(H5AC_unpin_entry(pentry) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "can't unpin proxy entry")

        /* Remove proxy entry from cache */
        if(H5AC_remove_entry(pentry) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTREMOVE, FAIL, "unable to remove proxy entry")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_proxy_entry_remove_child() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_proxy_entry_dest
 *
 * Purpose:     Destroys a proxy entry in memory.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_proxy_entry_dest(H5AC_proxy_entry_t *pentry)
{
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pentry);
    HDassert(NULL == pentry->parents);
    HDassert(0 == pentry->nchildren);
    HDassert(0 == pentry->ndirty_children);
    HDassert(0 == pentry->nunser_children);

    /* Free the proxy entry object */
    pentry = H5FL_FREE(H5AC_proxy_entry_t, pentry);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_proxy_entry_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__proxy_entry_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__proxy_entry_image_len(const void H5_ATTR_UNUSED *thing, size_t *image_len)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image_len);

    /* Set the image length size to 1 byte */
    *image_len = 1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5AC__proxy_entry_image_len() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__proxy_entry_serialize
 *
 * Purpose:	Serializes a data structure for writing to disk.
 *
 * Note:	Should never be invoked.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__proxy_entry_serialize(const H5F_t H5_ATTR_UNUSED *f, void H5_ATTR_UNUSED *image,
    size_t H5_ATTR_UNUSED len, void H5_ATTR_UNUSED *thing)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    /* Should never be invoked */
    HDassert(0 && "Invalid callback?!?");

    HERROR(H5E_CACHE, H5E_CANTSERIALIZE, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5AC__proxy_entry_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__proxy_entry_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__proxy_entry_notify(H5AC_notify_action_t action, void *_thing, ...)
{
    H5AC_proxy_entry_t *pentry = (H5AC_proxy_entry_t *)_thing;
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(pentry);

    switch(action) {
        case H5AC_NOTIFY_ACTION_AFTER_INSERT:
	    break;

	case H5AC_NOTIFY_ACTION_AFTER_LOAD:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "invalid notify action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Invalid action?!?");
#endif /* NDEBUG */
            break;

	case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "invalid notify action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Invalid action?!?");
#endif /* NDEBUG */
	    break;

        case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
            /* Sanity checks */
            HDassert(0 == pentry->ndirty_children);
            HDassert(0 == pentry->nunser_children);

            /* No action */
            break;

        case H5AC_NOTIFY_ACTION_ENTRY_DIRTIED:
            /* Sanity checks */
            HDassert(pentry->ndirty_children > 0);

            /* No action */
            break;

        case H5AC_NOTIFY_ACTION_ENTRY_CLEANED:
            /* Sanity checks */
            HDassert(0 == pentry->ndirty_children);

            /* No action */
            break;

        case H5AC_NOTIFY_ACTION_CHILD_DIRTIED:
            /* Increment # of dirty children */
            pentry->ndirty_children++;

            /* Check for first dirty child */
            if(1 == pentry->ndirty_children)
                if(H5AC_mark_entry_dirty(pentry) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTDIRTY, FAIL, "can't mark proxy entry dirty")
            break;

        case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
        case H5AC_NOTIFY_ACTION_CHILD_UNDEPEND_DIRTY:
            /* Sanity check */
            HDassert(pentry->ndirty_children > 0);

            /* Decrement # of dirty children */
            pentry->ndirty_children--;

            /* Check for last dirty child */
            if(0 == pentry->ndirty_children)
                if(H5AC_mark_entry_clean(pentry) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTCLEAN, FAIL, "can't mark proxy entry clean")
            break;

        case H5AC_NOTIFY_ACTION_CHILD_UNSERIALIZED:
            /* Increment # of unserialized children */
            pentry->nunser_children++;

            /* Check for first unserialized child */
            if(1 == pentry->nunser_children)
                if(H5AC_mark_entry_unserialized(pentry) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTUNSERIALIZE, FAIL, "can't mark proxy entry unserialized")
            break;

        case H5AC_NOTIFY_ACTION_CHILD_SERIALIZED:
            /* Sanity check */
            HDassert(pentry->nunser_children > 0);

            /* Decrement # of unserialized children */
            pentry->nunser_children--;

            /* Check for last unserialized child */
            if(0 == pentry->nunser_children)
                if(H5AC_mark_entry_serialized(pentry) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTSERIALIZE, FAIL, "can't mark proxy entry serialized")
            break;

        case H5AC_NOTIFY_ACTION_CHILD_BEFORE_EVICT:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "invalid notify action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Invalid action?!?");
#endif /* NDEBUG */
	    break;

        default:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "unknown notify action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC__proxy_entry_notify() */


/*-------------------------------------------------------------------------
 * Function:	H5AC__proxy_entry_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 17, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__proxy_entry_free_icr(void *_thing)
{
    H5AC_proxy_entry_t *pentry = (H5AC_proxy_entry_t *)_thing;
    herr_t ret_value = SUCCEED;     		/* Return value */

    FUNC_ENTER_STATIC

    /* Destroy the proxy entry */
    if(H5AC_proxy_entry_dest(pentry) < 0)
	HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "unable to destroy proxy entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__proxy_entry_free_icr() */

