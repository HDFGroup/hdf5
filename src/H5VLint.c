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

/*
 * Purpose:	The Virtual Object Layer as described in documentation.
 *              The pupose is to provide an abstraction on how to access the
 *              underlying HDF5 container, whether in a local file with
 *              a specific file format, or remotely on other machines, etc...
 */

/****************/
/* Module Setup */
/****************/

#include "H5VLmodule.h"         /* This source code file is part of the H5VL module */


/***********/
/* Headers */
/***********/

#include "H5private.h"          /* Generic Functions                                */
#include "H5Eprivate.h"         /* Error handling                                   */
#include "H5Iprivate.h"         /* IDs                                              */
#include "H5MMprivate.h"        /* Memory management                                */
#include "H5Pprivate.h"         /* Property lists                                   */
#include "H5VLpkg.h"            /* Virtual Object Layer                             */


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

static herr_t H5VL_free_cls(H5VL_class_t *cls);

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

/* VOL ID class */
static const H5I_class_t H5I_VOL_CLS[1] = {{
    H5I_VOL,                    /* ID class value */
    0,                          /* Class flags */
    0,                          /* # of reserved IDs for class */
    (H5I_free_t)H5VL_free_cls   /* Callback routine for closing objects of this class */
}};


/*-------------------------------------------------------------------------
 * Function:    H5VL_init
 *
 * Purpose:     Initialize the interface from some other package
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_init() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__init_package
 *
 * Purpose:     Initialize interface-specific information
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__init_package(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* Initialize the atom group for the VL IDs */
    if(H5I_register_type(H5I_VOL_CLS) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__init_package() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_term_package
 *
 * Purpose:     Terminate various H5VL objects
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {

        if(H5I_nmembers(H5I_VOL) > 0) {
            /* XXX: Need to investigate if this is okay... */
            (void)H5I_clear_type(H5I_VOL, FALSE, FALSE);
            n++;
        }
        else {
            n += (H5I_dec_type_ref(H5I_VOL) > 0);

            /* Mark interface as closed */
            if(0 == n)
                H5_PKG_INIT_VAR = FALSE;
        }
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5VL_term_package() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_free_cls
 *
 * Purpose:     Frees a file VOL class struct and returns an indication of
 *              success. This function is used as the free callback for the
 *              virtual object layer object identifiers
 *              (c.f.: H5VL_init_interface).
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_free_cls(H5VL_class_t *cls)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(cls);

    /* XXX: Need to retrieve the VOL termination property list for the
     * terminate operation - JTH
     */
    if(cls->terminate && cls->terminate(H5P_DEFAULT) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "VOL plugin did not terminate cleanly")

    H5MM_free(cls);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_cls() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_register
 *
 * Purpose:     Registers a new vol plugin as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A vol plugin ID which is good until the
 *                          library is closed or the driver is unregistered.
 *
 *              Failure:    A negative value.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_register(const void *_cls, size_t size, hbool_t app_ref)
{
    const H5VL_class_t	*cls = (const H5VL_class_t *)_cls;
    H5VL_class_t	*saved = NULL;
    hid_t		ret_value = -1;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    HDassert(cls);

    /* Copy the class structure so the caller can reuse or free it */
    if(NULL == (saved = (H5VL_class_t *)H5MM_calloc(size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for vol plugin class struct")
    HDmemcpy(saved, cls, size);

    /* Create the new class ID */
    if((ret_value = H5I_register(H5I_VOL, saved, app_ref)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register vol plugin ID")

done:
    if(ret_value < 0)
        if(saved)
            H5MM_xfree(saved);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register() */

