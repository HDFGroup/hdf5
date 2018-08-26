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
 * Created:             H5ACshadow_entry.c
 *
 * Purpose:             Functions and a cache client for a "shadow" cache entry.
 *			A shadow cache entry is used as a placeholder for an
 *			object, to retain state about the object, attach data
 *			structure flush dependencies, etc.
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


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */
static void *H5AC__shadow_entry_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty); 
static herr_t H5AC__shadow_entry_image_len(const void *thing, size_t *image_len);
static herr_t H5AC__shadow_entry_serialize(const H5F_t *f, void *image_ptr,
    size_t len, void *thing);
static herr_t H5AC__shadow_entry_notify(H5AC_notify_action_t action, void *thing, ...);
static herr_t H5AC__shadow_entry_free_icr(void *thing);

/*********************/
/* Package Variables */
/*********************/

/* H5AC shadow entries inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_SHADOW_ENTRY[1] = {{
    H5AC_SHADOW_ENTRY_ID,              	/* Metadata client ID */
    "Shadow entry",           		/* Metadata client name (for debugging) */
    H5FD_MEM_SUPER,                     /* File space memory type for client */
    0,					/* Client class behavior flags */
    NULL,    				/* 'get_initial_load_size' callback */
    NULL,    				/* 'get_final_load_size' callback */
    NULL,				/* 'verify_chksum' callback */
    H5AC__shadow_entry_deserialize,	/* 'deserialize' callback */
    H5AC__shadow_entry_image_len,	/* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5AC__shadow_entry_serialize,	/* 'serialize' callback */
    H5AC__shadow_entry_notify,		/* 'notify' callback */
    H5AC__shadow_entry_free_icr,       	/* 'free_icr' callback */
    NULL,                              	/* 'fsf_size' callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage H5AC_shadow_entry_t objects */
H5FL_DEFINE_STATIC(H5AC_shadow_entry_t);



/*-------------------------------------------------------------------------
 * Function:    H5AC_shadow_entry_create
 *
 * Purpose:     Create a new shadow entry
 *
 * Return:	Success:	Pointer to the new shadow entry object.
 *		Failure:	NULL
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
H5AC_shadow_entry_t *
H5AC_shadow_entry_create(H5F_t *f, haddr_t addr)
{
    H5AC_shadow_entry_t *shadow = NULL;         /* Pointer to new shadow entry */
    H5AC_shadow_entry_t *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Sanity checks */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Allocate new shadow entry */
    if(NULL == (shadow = H5FL_CALLOC(H5AC_shadow_entry_t)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, NULL, "can't allocate shadow entry")

    /* Set non-zero fields */
    shadow->f = f;
    shadow->addr = addr;

    /* Insert shadow entry into the cache */
    if(H5AC_insert_entry(f, H5AC_SHADOW_ENTRY, addr, shadow, H5AC__PIN_ENTRY_FLAG) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, NULL, "unable to insert shadow entry")

    /* Set return value */
    ret_value = shadow;

done:
    /* Release resources on error */
    if(!ret_value)
        if(shadow)
            shadow = H5FL_FREE(H5AC_shadow_entry_t, shadow);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_shadow_entry_create() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_shadow_entry_protect
 *
 * Purpose:     Protect a shadow entry
 *
 * Return:	Success:	Pointer to the shadow entry object.
 *		Failure:	NULL
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
H5AC_shadow_entry_t *
H5AC_shadow_entry_protect(H5F_t *f, haddr_t addr, unsigned flags)
{
    H5AC_shadow_entry_t *shadow;                /* Pointer to shadow entry */
    H5AC_shadow_entry_t *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Sanity checks */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Protect the shadow entry */
    if(NULL == (shadow = (H5AC_shadow_entry_t *)H5AC_protect(f, H5AC_SHADOW_ENTRY, addr, NULL, flags)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTPROTECT, NULL, "unable to protect shadow entry")

    /* Set return value */
    ret_value = shadow;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_shadow_entry_protect() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_shadow_entry_add_component
 *
 * Purpose:     Add a component of an object to a shadow entry
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_shadow_entry_add_component(H5AC_shadow_entry_t *shadow,
    H5AC_shadow_comp_t ctype, H5AC_proxy_entry_t *comp)
{
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shadow);
    HDassert(comp);

    /* Set the correct pointer for tracking the component */
    switch(ctype) {
        /* Object header 'top' proxy */
        case H5AC_SHADOW_OHDR:
            /* Sanity check */
            HDassert(NULL == shadow->ohdr_proxy);

            /* Set proxy pointer */
            shadow->ohdr_proxy = comp;
            break;

        /* Object heap 'top' proxy */
        case H5AC_SHADOW_OBJ_HEAP:
            /* Sanity check */
            HDassert(NULL == shadow->obj_heap_proxy);

            /* Set proxy pointer */
            shadow->obj_heap_proxy = comp;
            break;

        /* Object primary index 'top' proxy */
        case H5AC_SHADOW_OBJ_IDX:
            /* Sanity check */
            HDassert(NULL == shadow->obj_idx_proxy);

            /* Set proxy pointer */
            shadow->obj_idx_proxy = comp;
            break;

        /* Object secondary index 'top' proxy */
        case H5AC_SHADOW_OBJ_AUX_IDX:
            /* Sanity check */
            HDassert(NULL == shadow->obj_aux_idx_proxy);

            /* Set proxy pointer */
            shadow->obj_aux_idx_proxy = comp;
            break;

        /* Object attribute heap 'top' proxy */
        case H5AC_SHADOW_ATTR_HEAP:
            /* Sanity check */
            HDassert(NULL == shadow->attr_heap_proxy);

            /* Set proxy pointer */
            shadow->attr_heap_proxy = comp;
            break;

        /* Object attribute index 'top' proxy */
        case H5AC_SHADOW_ATTR_IDX:
            /* Sanity check */
            HDassert(NULL == shadow->attr_idx_proxy);

            /* Set proxy pointer */
            shadow->attr_idx_proxy = comp;
            break;

        default:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "invalid shadow entry component type")
#else /* NDEBUG */
            HDassert(0 && "Invalid component type?!?");
#endif /* NDEBUG */
            break;
    } /* end switch */

    /* Add component as child flush dependency on shadow */
    if(H5AC_create_flush_dependency(shadow, comp) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTDEPEND, FAIL, "unable to set flush dependency on shadow entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_shadow_entry_add_component() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_shadow_entry_unprotect
 *
 * Purpose:     Unprotect a shadow entry in the cache.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_shadow_entry_unprotect(H5AC_shadow_entry_t *shadow, unsigned cache_flags)
{
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shadow);
    HDassert(shadow->f);
    HDassert(H5F_addr_defined(shadow->addr));

    /* Unprotect the shadow entry, with appropriate flags */
    if(H5AC_unprotect(shadow->f, H5AC_SHADOW_ENTRY, shadow->addr, shadow, cache_flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPROTECT, FAIL, "unable to unprotect shadow entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_shadow_entry_unprotect() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_shadow_entry_dest
 *
 * Purpose:     Destroys a shadow entry in memory.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_shadow_entry_dest(H5AC_shadow_entry_t *shadow)
{
    herr_t ret_value = SUCCEED;         	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(shadow);

    /* Free the shadow entry object */
    shadow = H5FL_FREE(H5AC_shadow_entry_t, shadow);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AC_shadow_entry_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__shadow_entry_deserialize
 *
 * Purpose:	Deserializes a data structure read from disk.
 *
 * Note:	Should never be invoked.
 *
 * Return:	Success:	Pointer to in core representation
 *		Failure:	NULL
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5AC__shadow_entry_deserialize(const void H5_ATTR_UNUSED*_image, size_t H5_ATTR_UNUSED len,
    void H5_ATTR_UNUSED *_udata, hbool_t H5_ATTR_UNUSED *dirty)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    /* Should never be invoked */
    HDassert(0 && "Invalid callback?!?");

    HERROR(H5E_CACHE, H5E_CANTUNSERIALIZE, "called unreachable function");

    FUNC_LEAVE_NOAPI(NULL)
} /* end H5AC__shadow_entry_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__shadow_entry_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__shadow_entry_image_len(const void H5_ATTR_UNUSED *thing, size_t *image_len)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image_len);

    /* Set the image length size to 1 byte */
    *image_len = 1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5AC__shadow_entry_image_len() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__shadow_entry_serialize
 *
 * Purpose:	Serializes a data structure for writing to disk.
 *
 * Note:	Should never be invoked.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__shadow_entry_serialize(const H5F_t H5_ATTR_UNUSED *f, void H5_ATTR_UNUSED *image,
    size_t H5_ATTR_UNUSED len, void H5_ATTR_UNUSED *thing)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    /* Should never be invoked */
    HDassert(0 && "Invalid callback?!?");

    HERROR(H5E_CACHE, H5E_CANTSERIALIZE, "called unreachable function");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5AC__shadow_entry_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__shadow_entry_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__shadow_entry_notify(H5AC_notify_action_t action, void *_thing, ...)
{
    H5AC_shadow_entry_t *shadow = (H5AC_shadow_entry_t *)_thing;
    va_list ap;                     /* Varargs parameter */
    hbool_t va_started = FALSE;     /* Whether the variable argument list is open */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(shadow);

    switch(action) {
        case H5AC_NOTIFY_ACTION_AFTER_INSERT:
            /* Sanity checks */

            /* Shadows start out clean (insertions are automatically marked dirty) */
            if(H5AC_mark_entry_clean(shadow) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTCLEAN, FAIL, "can't mark shadow entry clean")

            /* Shadows start out serialized (insertions are automatically marked unserialized) */
            if(H5AC_mark_entry_serialized(shadow) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTSERIALIZE, FAIL, "can't mark shadow entry clean")
	    break;

	case H5AC_NOTIFY_ACTION_AFTER_LOAD:
	case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "invalid notify action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Invalid action?!?");
#endif /* NDEBUG */
            break;

        case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
            /* No action */
            break;

        case H5AC_NOTIFY_ACTION_ENTRY_DIRTIED:
#ifdef NDEBUG
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "invalid notify action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Invalid action?!?");
#endif /* NDEBUG */
            break;

        case H5AC_NOTIFY_ACTION_ENTRY_CLEANED:
        case H5AC_NOTIFY_ACTION_CHILD_DIRTIED:
        case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
        case H5AC_NOTIFY_ACTION_CHILD_UNSERIALIZED:
        case H5AC_NOTIFY_ACTION_CHILD_SERIALIZED:
            /* No action */
            break;

        case H5AC_NOTIFY_ACTION_CHILD_BEFORE_EVICT:
            {
                void *child_entry = NULL;       /* Child entry */
                H5AC_flush_dep_t *flush_dep;    /* Flush dependency object */
                unsigned nchildren;             /* # of flush dependency children for freedspace entry */

                /* Initialize the argument list */
                va_start(ap, _thing);
                va_started = TRUE;

                /* The child entry in the varg */
                child_entry = va_arg(ap, void *);

                /* Check the cache entry for the child */
                if(NULL == child_entry)
                    HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "no cache entry at address")

                /* Reset pointer to proxy in shadow entry */
                if(child_entry == shadow->ohdr_proxy)
                    shadow->ohdr_proxy = NULL;
                else if(child_entry == shadow->obj_heap_proxy)
                    shadow->obj_heap_proxy = NULL;
                else if(child_entry == shadow->obj_idx_proxy)
                    shadow->obj_idx_proxy = NULL;
                else if(child_entry == shadow->obj_aux_idx_proxy)
                    shadow->obj_aux_idx_proxy = NULL;
                else if(child_entry == shadow->attr_heap_proxy)
                    shadow->attr_heap_proxy = NULL;
                else if(child_entry == shadow->attr_idx_proxy)
                    shadow->attr_idx_proxy = NULL;
                else
                    HGOTO_ERROR(H5E_CACHE, H5E_NOTFOUND, FAIL, "proxy entry not a component of shadow entry?")

                /* The flush dependency object, in the vararg */
                flush_dep = va_arg(ap, H5AC_flush_dep_t *);
                HDassert(flush_dep);

                /* Remove flush dependency on evicted component */
                if(H5AC_destroy_flush_dep(flush_dep) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency on shadow entry")

                /* Get # of flush dependency children on shadow entry now */
                if(H5AC_get_flush_dep_nchildren((const H5AC_info_t *)shadow, &nchildren) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't get cache entry flush dep nchildren")

                /* If the # of children drops to zero, delete the shadow entry */
                if(0 == nchildren ) {
                    /* Remove shadow object from metadata cache */
                    if(H5AC_unpin_entry(shadow) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTUNPIN, FAIL, "can't unpin entry")
                    if(H5AC_remove_entry(shadow) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTREMOVE, FAIL, "can't remove entry")

                    /* Destroy shadow object */
                    if(H5AC_shadow_entry_dest(shadow) < 0)
                        HGOTO_ERROR(H5E_CACHE, H5E_CANTDELETE, FAIL, "can't destroy shadow entry")
                } /* end if */
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
} /* end H5AC__shadow_entry_notify() */


/*-------------------------------------------------------------------------
 * Function:	H5AC__shadow_entry_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 30, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5AC__shadow_entry_free_icr(void *_thing)
{
    H5AC_shadow_entry_t *shadow = (H5AC_shadow_entry_t *)_thing;
    herr_t ret_value = SUCCEED;     		/* Return value */

    FUNC_ENTER_STATIC

    /* Destroy the shadow entry */
    if(H5AC_shadow_entry_dest(shadow) < 0)
	HGOTO_ERROR(H5E_CACHE, H5E_CANTFREE, FAIL, "unable to destroy shadow entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__shadow_entry_free_icr() */

