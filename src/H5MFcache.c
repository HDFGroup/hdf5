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
 * Created:             H5MFcache.c
 *
 * Purpose:             Functions for freed space cache entry management.
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


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache callbacks */
static herr_t H5MF__freedspace_notify(H5AC_notify_action_t action, void *_thing, ...);
static herr_t H5MF__freedspace_image_len(const void *thing, size_t *image_len);
static herr_t H5MF__freedspace_serialize(const H5F_t *f, void *image_ptr,
            size_t len, void *thing);
static herr_t H5MF__freedspace_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/

/* H5MF freedspace entries inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_FREEDSPACE[1] = {{
    H5AC_FREEDSPACE_ID,               	/* Metadata client ID */
    "Freed space",           		/* Metadata client name (for debugging) */
    H5FD_MEM_SUPER,                     /* File space memory type for client */
    0,					/* Client class behavior flags */
    NULL,    				/* 'get_initial_load_size' callback */
    NULL,    				/* 'get_final_load_size' callback */
    NULL,				/* 'verify_chksum' callback */
    NULL,    				/* 'deserialize' callback */
    H5MF__freedspace_image_len,	        /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5MF__freedspace_serialize,	        /* 'serialize' callback */
    H5MF__freedspace_notify,		/* 'notify' callback */
    H5MF__freedspace_free_icr,          /* 'free_icr' callback */
    NULL,                              	/* 'fsf_size' callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5MF__freedspace_image_len
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
H5MF__freedspace_image_len(const void H5_ATTR_UNUSED *thing, size_t *image_len)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image_len);

    /* Set the image length size to 1 byte */
    *image_len = 1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5MF__freedspace_image_len() */


/*-------------------------------------------------------------------------
 * Function:    H5MF__freedspace_serialize
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
H5MF__freedspace_serialize(const H5F_t H5_ATTR_UNUSED *f, void H5_ATTR_UNUSED *image,
    size_t H5_ATTR_UNUSED len, void H5_ATTR_UNUSED *thing)
{
    FUNC_ENTER_STATIC_NOERR /* Yes, even though this pushes an error on the stack */

    /* Should never be invoked */
    HDassert(0 && "Invalid callback?!?");

    HERROR(H5E_RESOURCE, H5E_CANTSERIALIZE, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5MF__freedspace_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5MF__freedspace_notify
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
H5MF__freedspace_notify(H5AC_notify_action_t action, void *_thing, ...)
{
    H5MF_freedspace_t *pentry = (H5MF_freedspace_t *)_thing;    /* Pointer to freedspace entry being notified */
    va_list ap;                     /* Varargs parameter */
    hbool_t va_started = FALSE;     /* Whether the variable argument list is open */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(pentry);

    switch(action) {
        case H5AC_NOTIFY_ACTION_CHILD_BEFORE_EVICT:
        case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
            {
                void *child_entry = NULL;       /* Child entry */
                haddr_t child_addr;             /* Address of child entry */
                unsigned nchildren;             /* # of flush dependency children for freedspace entry */

                /* Initialize the argument list */
                va_start(ap, _thing);
                va_started = TRUE;

                /* The child freedspace entry address in the varg */
                child_addr = va_arg(ap, haddr_t);
                HDassert(H5F_addr_defined(child_addr));

                /* Get the cache entry for the child */
                if(H5AC_get_entry_from_addr(pentry->f, child_addr, &child_entry) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't lookup cache entry at address")
                if(NULL == child_entry)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_BADVALUE, FAIL, "no cache entry at address")

                /* Remove flush dependency on cleaned or evicted child */
                if(H5AC_destroy_flush_dependency(pentry, child_entry) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency on freedspace entry")

                /* Get # of flush dependency children now */
                if(H5AC_get_flush_dep_nchildren((const H5AC_info_t *)pentry, &nchildren) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "can't get cache entry nchildren")

                /* If the # of children drops to zero, invoke the callback and delete the freedspace entry */
                if(0 == nchildren ) {
                    /* Remove freed space object from metadata cache */
                    if(H5AC_mark_entry_clean(pentry) < 0)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTMARKCLEAN, FAIL, "can't mark entry clean")
                    if(H5AC_unpin_entry(pentry) < 0)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTUNPIN, FAIL, "can't unpin entry")
                    if(H5AC_remove_entry(pentry) < 0)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTREMOVE, FAIL, "can't remove entry")

                    /* Invoke routine to release freed space */
                    if(H5MF__xfree_freedspace(pentry) < 0)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CALLBACK, FAIL, "can't release freed space")

                    /* Release freed space object */
                    if(H5MF__freedspace_dest(pentry) < 0)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTDELETE, FAIL, "can't destroy freedspace")
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
            HGOTO_ERROR(H5E_RESOURCE, H5E_BADVALUE, FAIL, "unknown notify action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
    } /* end switch */

done:
    if(va_started)
        va_end(ap);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF__freedspace_notify() */


/*-------------------------------------------------------------------------
 * Function:    H5MF__freedspace_free_icr
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
H5MF__freedspace_free_icr(void *_thing)
{
    H5MF_freedspace_t *pentry = (H5MF_freedspace_t *)_thing;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_STATIC

    /* Destroy the freedspace entry */
    if(H5MF__freedspace_dest(pentry) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "unable to destroy freedspace entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MF__freedspace_free_icr() */

