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

/* Proxy entry temporary child list node */
typedef struct H5AC_proxy_tmp_child_t {
    struct H5AC_proxy_tmp_child_t *next;   /* Pointer to next node in temp. child list */
    struct H5AC_proxy_tmp_child_t *prev;   /* Pointer to previous node in temp. child list */
    H5AC_info_t *tmp_child;                /* Pointer to temp. child entry */
} H5AC_proxy_tmp_child_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Temporary child helper routine */
static H5AC_proxy_tmp_child_t *H5AC__proxy_entry_find_tmp_child(const H5AC_proxy_entry_t *pentry,
    const H5AC_info_t *child_entry);
static void H5AC__proxy_entry_remove_tmp_child(H5AC_proxy_entry_t *pentry, 
    H5AC_proxy_tmp_child_t *proxy_tmp_child);
static herr_t H5AC__proxy_entry_tmp_child_check(H5AC_proxy_entry_t *pentry,
    H5AC_info_t *child_entry, hbool_t err_on_non_tmp_child);
static herr_t H5AC__proxy_entry_remove_child(H5AC_proxy_entry_t *pentry,
    void *child);

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

/* Declare a free list to manage H5AC_proxy_tmp_child_t objects */
H5FL_DEFINE_STATIC(H5AC_proxy_tmp_child_t);



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
 * Function:    H5AC_proxy_entry_add_tmp_child
 *
 * Purpose:     Add a temporary child to a proxy entry
 *
 * Note:	Temporary children are removed automatically from the proxy
 *		when they are marked clean or evicted from the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 31, 2018
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_proxy_entry_add_tmp_child(H5AC_proxy_entry_t *pentry, void *_tmp_child)
{
    H5AC_info_t *tmp_child = (H5AC_info_t *)_tmp_child; /* Temp. child entry's cache info */
    H5AC_proxy_tmp_child_t *proxy_tmp_child = NULL;   /* Proxy temp. child node for new temp. child */
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pentry);
    HDassert(tmp_child);

    /* Add the child to the proxy */
    if(H5AC_proxy_entry_add_child(pentry, tmp_child) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTSET, FAIL, "unable to add child to proxy")

    /* Allocate a proxy temp. child node */
    if(NULL == (proxy_tmp_child = H5FL_MALLOC(H5AC_proxy_tmp_child_t)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "can't allocate proxy temp. child node")

    /* Set (temp. child) entry for proxy temp. child node */
    proxy_tmp_child->tmp_child = tmp_child;

    /* Add proxy temp. child node into list */
    proxy_tmp_child->prev = NULL;
    proxy_tmp_child->next = pentry->tmp_children;
    if(pentry->tmp_children)
        pentry->tmp_children->prev = proxy_tmp_child;
    pentry->tmp_children = proxy_tmp_child;

done:
    if(ret_value < 0)
        if(proxy_tmp_child) {
            /* Unlink from list, if in it */
            if(pentry->tmp_children == proxy_tmp_child) {
                if(proxy_tmp_child->next)
                    proxy_tmp_child->next->prev = NULL;
                pentry->tmp_children = proxy_tmp_child->next;
            } /* end if */

            /* Free the proxy temp. child node */
            proxy_tmp_child = H5FL_FREE(H5AC_proxy_tmp_child_t, proxy_tmp_child);
        } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_proxy_entry_add_tmp_child() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__proxy_entry_find_tmp_child
 *
 * Purpose:     Find the temporary child node, if there is one
 *
 * Return:      NULL / Non-NULL, can't "fail"
 *
 * Programmer:  Quincey Koziol
 *              August 18, 2018
 *
 *-------------------------------------------------------------------------
 */
static H5AC_proxy_tmp_child_t *
H5AC__proxy_entry_find_tmp_child(const H5AC_proxy_entry_t *pentry,
    const H5AC_info_t *child_entry)
{
    H5AC_proxy_tmp_child_t *proxy_tmp_child = NULL;   /* Proxy temp. child node for new temp. child */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(pentry);
    HDassert(child_entry);

    /* Find a temporary child, if there is one */
    /* (Linear search, but these lists probably aren't huge) */
    proxy_tmp_child = pentry->tmp_children;
    while(proxy_tmp_child) {
        /* Check for entry being removed */
        if(proxy_tmp_child->tmp_child == child_entry)
            break;

        /* Advance to next proxy temp. child node */
        proxy_tmp_child = proxy_tmp_child->next;
    } /* end while */

    FUNC_LEAVE_NOAPI(proxy_tmp_child)
} /* end H5AC__proxy_entry_find_tmp_child() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__proxy_entry_remove_tmp_child
 *
 * Purpose:     Remove a temporary child from a proxy
 *
 * Return:      None (can't fail)
 *
 * Programmer:  Quincey Koziol
 *              August 18, 2018
 *
 *-------------------------------------------------------------------------
 */
static void
H5AC__proxy_entry_remove_tmp_child(H5AC_proxy_entry_t *pentry, 
    H5AC_proxy_tmp_child_t *proxy_tmp_child)
{
    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(pentry);
    HDassert(proxy_tmp_child);

    /* Remove proxy temp. child from list */
    if(pentry->tmp_children == proxy_tmp_child) {
        /* Sanity check */
        HDassert(NULL == proxy_tmp_child->prev);

        /* Detech from head of list */
        if(proxy_tmp_child->next)
            proxy_tmp_child->next->prev = NULL;
        pentry->tmp_children = proxy_tmp_child->next;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(proxy_tmp_child->prev);

        /* Detach from middle or end of list */
        proxy_tmp_child->prev->next = proxy_tmp_child->next;
        if(proxy_tmp_child->next)
            proxy_tmp_child->next->prev = proxy_tmp_child->prev;
    } /* end else */

    /* Free the proxy temp. child node */
    proxy_tmp_child = H5FL_FREE(H5AC_proxy_tmp_child_t, proxy_tmp_child);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5AC__proxy_entry_remove_tmp_child() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__proxy_entry_tmp_child_check
 *
 * Purpose:     Check if child being cleaned / evicted is a temporary child
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 31, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__proxy_entry_tmp_child_check(H5AC_proxy_entry_t *pentry, H5AC_info_t *child_entry,
    hbool_t err_on_non_tmp_child)
{
    H5AC_proxy_tmp_child_t *proxy_tmp_child;    /* Proxy temp. child node for new temp. child */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(pentry);
    HDassert(child_entry);

    /* Check if this is a temporary child */
    proxy_tmp_child = H5AC__proxy_entry_find_tmp_child(pentry, child_entry);

    /* Error if not temp. child, else remove from proxy */
    if(NULL == proxy_tmp_child) {
        if(err_on_non_tmp_child)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTREMOVE, FAIL, "can't remove non-temporary child")
    } /* end if */
    else {
        /* Remove the temp. child from the list */
        H5AC__proxy_entry_remove_tmp_child(pentry, proxy_tmp_child);

        /* Remove child from proxy */
        if(H5AC__proxy_entry_remove_child(pentry, child_entry) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTREMOVE, FAIL, "can't remove temporary child")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC__proxy_entry_tmp_child_check() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__proxy_entry_remove_child
 *
 * Purpose:     Remove a child a proxy entry
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 18, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__proxy_entry_remove_child(H5AC_proxy_entry_t *pentry, void *child)
{
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_STATIC

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
} /* end H5AC__proxy_entry_remove_child() */


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
    H5AC_proxy_tmp_child_t *proxy_tmp_child;    /* Proxy temp. child node for new temp. child */
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(pentry);
    HDassert(child);

    /* Remove child from temp. list, if child is one */
    if((proxy_tmp_child = H5AC__proxy_entry_find_tmp_child(pentry, (const H5AC_info_t *)child)))
        H5AC__proxy_entry_remove_tmp_child(pentry, proxy_tmp_child);

    /* Remove child from proxy */
    if(H5AC__proxy_entry_remove_child(pentry, child) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTREMOVE, FAIL, "unable to remove child from proxy entry")

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
    va_list ap;                     /* Varargs parameter */
    hbool_t va_started = FALSE;     /* Whether the variable argument list is open */
    herr_t ret_value = SUCCEED;     /* Return value */

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
            {
                H5AC_info_t *child_entry;       /* Child entry */

                /* Sanity check */
                HDassert(pentry->ndirty_children > 0);

                /* Decrement # of dirty children */
                pentry->ndirty_children--;

                /* Check for last dirty child */
                if(0 == pentry->ndirty_children)
                    if(H5AC_mark_entry_clean(pentry) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTCLEAN, FAIL, "can't mark proxy entry clean")

                /* Initialize the argument list */
                va_start(ap, _thing);
                va_started = TRUE;

                /* The child entry in the varg */
                child_entry = va_arg(ap, H5AC_info_t *);

                /* Check the cache entry for the child */
                if(NULL == child_entry)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_BADVALUE, FAIL, "no cache entry at address")

                /* Check if this is a temporary child */
                if(H5AC__proxy_entry_tmp_child_check(pentry, child_entry, FALSE) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "check for temporary child failed")
            } /* end case */
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
            {
                H5AC_info_t *child_entry;       /* Child entry */
                htri_t child_dirty;             /* Whether child entry is dirty */

                /* Initialize the argument list */
                va_start(ap, _thing);
                va_started = TRUE;

                /* The child entry in the varg */
                child_entry = va_arg(ap, H5AC_info_t *);

                /* Check the cache entry for the child */
                if(NULL == child_entry)
                    HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "no cache entry at address")

                /* Check 'dirty' status of child entry */
                if((child_dirty = H5AC_entry_is_dirty(child_entry)) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't get 'dirty' status for child entry")
                else if(child_dirty) {
                    /* Sanity check */
                    HDassert(pentry->ndirty_children > 0);

                    /* Decrement # of dirty children */
                    pentry->ndirty_children--;

                    /* Check for last dirty child */
                    if(0 == pentry->ndirty_children)
                        if(H5AC_mark_entry_clean(pentry) < 0)
                            HGOTO_ERROR(H5E_CACHE, H5E_CANTCLEAN, FAIL, "can't mark proxy entry clean")
                } /* end if */

                /* Remove child from proxy */
                if(H5AC__proxy_entry_remove_child(pentry, child_entry) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTREMOVE, FAIL, "unable to remove child from proxy entry")
            } /* end case */
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

