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
 * Created:             H5Oproxy.c
 *                      February 28 2012
 *                      Neil Fortner <nfortne2@hdfgroup.org>
 *
 * Purpose:             Implement object header's metadata cache proxy cache
 *                      methods.
 *
 * Note:                Object header proxies exist only to make integrating the
 *                      object header chunks with the metadata cache's flush
 *                      dependencies easier and less coupled than directly tying
 *                      them together.
 *
 *                      Object header proxies never exist on disk (hence their
 *                      lack of a 'load' callback) and their 'flush' callback
 *                      does nothing.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Omodule.h"          /* This source code file is part of the H5O module */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5Opkg.h"             /* Object headers                       */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5FLprivate.h"        /* Free Lists                           */
#include "H5MFprivate.h"        /* File memory management               */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */
static herr_t H5O__cache_proxy_get_load_size(const void *image, const void *udata, 
    size_t *image_len, size_t *actual_len);
static void *H5O__cache_proxy_deserialize(const void *image, size_t len, void *udata, hbool_t *dirty);
static herr_t H5O__cache_proxy_image_len(const void *thing, size_t *image_len, hbool_t *compressed_ptr,
    size_t *compressed_image_len_ptr);
static herr_t H5O__cache_proxy_serialize(const H5F_t *f, void *image, size_t len, void *thing);
static herr_t H5O__cache_proxy_notify(H5AC_notify_action_t action, void *thing);
static herr_t H5O__cache_proxy_free_icr(void *thing);

static herr_t H5O__cache_proxy_dest(H5O_proxy_t *proxy);

/*********************/
/* Package Variables */
/*********************/

const H5AC_class_t H5AC_OHDR_PROXY[1] = {{
    H5AC_OHDR_PROXY_ID,                	/* Metadata client ID */
    "Object header proxy",           	/* Metadata client name (for debugging) */
    H5FD_MEM_OHDR,                     	/* File space memory type for client */
    H5AC__CLASS_SKIP_READS|H5AC__CLASS_SKIP_WRITES,           /* Client class behavior flags */
    H5O__cache_proxy_get_load_size,    	/* 'get_load_size' callback */
    NULL,				/* 'verify_chksum' callback */
    H5O__cache_proxy_deserialize,    	/* 'deserialize' callback */
    H5O__cache_proxy_image_len,      	/* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5O__cache_proxy_serialize,       	/* 'serialize' callback */
    H5O__cache_proxy_notify,		/* 'notify' callback */
    H5O__cache_proxy_free_icr,        	/* 'free_icr' callback */
    NULL,                              	/* 'clear' callback */
    NULL,                              	/* 'fsf_size' callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage H5O_proxy_t objects */
H5FL_DEFINE_STATIC(H5O_proxy_t);



/*-------------------------------------------------------------------------
 * Function:    H5O__cache_proxy_get_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              May 18, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_proxy_get_load_size(const void *_image, const void H5_ATTR_UNUSED *_udata, 
    size_t *image_len, size_t H5_ATTR_UNUSED *actual_len)
{
    const uint8_t *image = (const uint8_t *)_image;       /* Pointer into raw data buffer */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image_len);

    if(image == NULL) {
	/* Set the image length size */
	/* Object header proxies are represented as 1 byte in cache */
	/* (would be 0 bytes, but cache won't allow it currently.  See
	* H5D_cache_proxy_size) */
	*image_len = 1;
    }
    /* Nothing to do for non-NULL image: no need to compute actual_len */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__cache_proxy_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_proxy_deserialize
 *
 * Purpose:     Creates an object header proxy and creates flush
 *              dependencies from all object header chunks on this proxy.
 *
 * Return:      Success:        Pointer to a new object header proxy
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              Mar 15 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O__cache_proxy_deserialize(const void H5_ATTR_UNUSED *_image, size_t H5_ATTR_UNUSED len, void *_udata,
    hbool_t H5_ATTR_UNUSED *dirty)
{
    H5O_proxy_t          *proxy = NULL;				/* Object header proxy */
    H5O_proxy_cache_ud_t *udata = (H5O_proxy_cache_ud_t *)_udata;	/* User data for callback */
    H5O_proxy_t          *ret_value;  				/* Return value */

    FUNC_ENTER_STATIC

    /* check arguments */
    HDassert(udata);

    /* Create object header proxy object & initialize fields */
    if(NULL == (proxy = H5FL_MALLOC(H5O_proxy_t)))
        HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "can't allocate object header proxy")
    HDmemset(&proxy->cache_info, 0, sizeof(H5AC_info_t));

    proxy->f = udata->f;
    proxy->oh = udata->oh;

    /* Set return value */
    ret_value = proxy;

done:
    if(!ret_value && proxy)
        if(H5O__cache_proxy_dest(proxy) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_CANTFREE, NULL, "unable to destroy object header proxy")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_proxy_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_hdr_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              May 20, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_proxy_image_len(const void H5_ATTR_UNUSED *_thing, size_t *image_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image_len);

    /* Set the image length size */
    /* Object header proxies are represented as 1 byte in cache */
    /* (would be 0 bytes, but cache won't allow it currently.  See
     * H5D_cache_proxy_size) */
    *image_len = 1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__cache_proxy_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5O__cache_proxy_serialize
 *
 * Purpose:	A no-op
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_proxy_serialize(const H5F_t H5_ATTR_UNUSED *f, void H5_ATTR_UNUSED *_image, size_t H5_ATTR_UNUSED len,
    void H5_ATTR_UNUSED *_thing)
{
    FUNC_ENTER_STATIC_NOERR

    /* A no-op */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5O__cache_proxy_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_proxy_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              nfortne2@hdfgroup.org
 *              Apr 25 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_proxy_notify(H5AC_notify_action_t action, void *_thing)
{
    H5O_proxy_t *proxy = (H5O_proxy_t *)_thing;
    H5O_chunk_proxy_t *chk_proxy = NULL; 	/* Object header chunk proxy */
    unsigned i;                         	/* Local index variable */
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(proxy);
    HDassert(proxy->f);
    HDassert(proxy->oh);
    HDassert(proxy->oh->swmr_write);

    switch(action) {
        case H5AC_NOTIFY_ACTION_AFTER_INSERT:
	case H5AC_NOTIFY_ACTION_AFTER_LOAD:
            /* Create flush dependency on object header chunk 0 */
            if(H5AC_create_flush_dependency(proxy->oh, proxy) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")

            /* Create flush dependencies on all other object header chunks */
            for(i = 1; i < proxy->oh->nchunks; i++) {
                if(NULL == (chk_proxy = H5O_chunk_protect(proxy->f, H5AC_ind_dxpl_id, proxy->oh, i)))
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header chunk")
/* same as before, but looks backward...need to check into that..(proxy, chk_proxy) */
                if(H5AC_create_flush_dependency(chk_proxy, proxy) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")
                if(H5O_chunk_unprotect(proxy->f, H5AC_ind_dxpl_id, chk_proxy, FALSE) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to unprotect object header chunk")
                chk_proxy = NULL;
            } /* end for */

            /* Mark proxy as present on the object header */
            proxy->oh->proxy_present = TRUE;

            break;

	case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
	    /* do nothing */
	    break;

        case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
            /* Mark proxy as not present on the object header */
            proxy->oh->proxy_present = FALSE;

            break;

        default:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "unknown action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
    } /* end switch */

done:
    if(chk_proxy) {
        HDassert(ret_value < 0);
        if(H5O_chunk_unprotect(proxy->f, H5AC_ind_dxpl_id, chk_proxy, FALSE) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to unprotect object header chunk")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5O__cache_proxy_notify() */


/*-------------------------------------------------------------------------
 * Function:	H5O__cache_proxy_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Mar 15 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_proxy_free_icr(void *_thing)
{
    H5O_proxy_t *proxy = (H5O_proxy_t *)_thing;
    herr_t ret_value = SUCCEED;     		/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5O__cache_proxy_dest(proxy) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to destroy object header proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O__cache_proxy_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5O_cache_proxy_dest
 *
 * Purpose:     Destroys an object header proxy in memory.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Mar 15 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_proxy_dest(H5O_proxy_t *proxy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(proxy);
    HDassert(proxy->oh);

    /* Free the object header proxy object */
    proxy = H5FL_FREE(H5O_proxy_t, proxy);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_proxy_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5O_proxy_create
 *
 * Purpose:     Allocate temporary space for the object header proxy.  We
 *              save the actual creation of the proxy to the "load"
 *              callback, to save time for objects that do not have any
 *              flush dependency children and to simplify the code.  The
 *              load callback needs to  set up the flush dependencies
 *              anyways because the proxy cannot stay pinned by the object
 *              header or it would be imnpossible to evict the two due to
 *              the circular dependencies.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Mar 15 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_proxy_create(H5F_t *f, hid_t H5_ATTR_UNUSED dxpl_id, H5O_t *oh)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(oh);

    /* Get a temp. address for object header proxy */
    if(HADDR_UNDEF == (oh->proxy_addr = H5MF_alloc_tmp(f, (hsize_t)1)))
        HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, FAIL, "file allocation failed for object header proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_proxy_create() */


/*-------------------------------------------------------------------------
 * Function:    H5O_proxy_pin
 *
 * Purpose:     Returns the proxy object for the specified object header,
 *              pinned.  This proxy can be used as a flush dependency
 *              parent for items that depend on this object header.
 *
 * Return:      Success:        Pointer to a pinned object header proxy
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              Mar 15 2012
 *
 *-------------------------------------------------------------------------
 */
H5O_proxy_t *
H5O_proxy_pin(H5F_t *f, hid_t dxpl_id, H5O_t *oh)
{
    H5O_proxy_t *proxy = NULL;          /* Object header proxy */
    H5O_proxy_cache_ud_t udata;		/* User-data for callback */
    H5O_proxy_t *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(f);
    HDassert(oh);
    HDassert(H5F_addr_defined(oh->proxy_addr));

    udata.f = f;
    udata.oh  = oh;
    /* Protect the object header proxy */
    if(NULL == (proxy = (H5O_proxy_t *)H5AC_protect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, &udata, H5AC__READ_ONLY_FLAG)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, NULL, "unable to protect object header proxy");

    /* Unprotect the object header proxy and pin it */
    if(H5AC_unprotect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, proxy, H5AC__PIN_ENTRY_FLAG) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, NULL, "unable to unprotect object header proxy");

    /* Set return value */
    ret_value = proxy;
    proxy = NULL;

done:
    if(proxy) {
        HDassert(!ret_value);
        if(H5AC_unprotect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, proxy, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, NULL, "unable to unprotect object header proxy");
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_proxy_pin() */


/*-------------------------------------------------------------------------
 * Function:    H5O_proxy_unpin
 *
 * Purpose:     Unpins the specified object header proxy from the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Mar 15 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_proxy_unpin(H5O_proxy_t *proxy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(proxy);

    /* Unpin the object header proxy */
    if(H5AC_unpin_entry(proxy) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTUNPIN, FAIL, "unable to unpin object header proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_proxy_unpin() */


/*-------------------------------------------------------------------------
 * Function:    H5O_proxy_depend
 *
 * Purpose:     Creates a flush dependency between the object header proxy
 *              (as child) and the specified object (as parent).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Mar 20 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_proxy_depend(H5F_t *f, hid_t dxpl_id, H5O_t *oh, void *parent)
{
    H5O_proxy_t *proxy = NULL;          /* Object header proxy */
    H5O_proxy_cache_ud_t udata;         /* User-data for callback */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(f);
    HDassert(oh);
    HDassert(H5F_addr_defined(oh->proxy_addr));
    HDassert(parent);

    udata.f = f;
    udata.oh  = oh;
    /* Protect the object header proxy */
    if(NULL == (proxy = (H5O_proxy_t *)H5AC_protect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, &udata, H5AC__READ_ONLY_FLAG)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to protect object header proxy");

    /* Add the flush dependency on the parent object */
    if(H5AC_create_flush_dependency(parent, proxy) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")

    /* Unprotect the object header proxy */
    if(H5AC_unprotect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, proxy, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to unprotect object header proxy");
    proxy = NULL;

done:
    if(proxy) {
        HDassert(ret_value < 0);
        if(H5AC_unprotect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, proxy, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to unprotect object header proxy");
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_proxy_depend() */


/*-------------------------------------------------------------------------
 * Function:    H5O_proxy_undepend
 *
 * Purpose:     Destroys the flush dependency between the object header
 *              proxy (as child) and the specified object (as parent).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Mar 20 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_proxy_undepend(H5F_t *f, hid_t dxpl_id, H5O_t *oh, void *parent)
{
    H5O_proxy_t *proxy = NULL;          /* Object header proxy */
    H5O_proxy_cache_ud_t udata;         /* User-data for callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(f);
    HDassert(oh);
    HDassert(H5F_addr_defined(oh->proxy_addr));
    HDassert(parent);

    udata.f = f;
    udata.oh  = oh;
    /* Protect the object header proxy */
    if(NULL == (proxy = (H5O_proxy_t *)H5AC_protect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, &udata, H5AC__READ_ONLY_FLAG)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to protect object header proxy");

    /* Add the flush dependency on the parent object */
    if(H5AC_destroy_flush_dependency(parent, proxy) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency")

    /* Unprotect the object header proxy */
    if(H5AC_unprotect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, proxy, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to unprotect object header proxy");
    proxy = NULL;

done:
    if(proxy) {
        HDassert(ret_value < 0);
        if(H5AC_unprotect(f, dxpl_id, H5AC_OHDR_PROXY, oh->proxy_addr, proxy, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to unprotect object header proxy");
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_proxy_undepend() */

