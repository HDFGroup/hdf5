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

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              January, 2012
 *
 * Purpose:	The native VOL plugin where access is to a single HDF5 file 
 *              using HDF5 VFDs. 
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_native_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLnative.h"         /* Native VOL plugin			*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/

/* The driver identification number, initialized at runtime */
static hid_t H5VL_NATIVE_g = 0;


/* Prototypes */
static H5F_t *H5VL_native_open(const char *name, unsigned flags, hid_t fcpl_id, 
                               hid_t fapl_id, hid_t dxpl_id);
static herr_t H5VL_native_close(H5F_t *f);
static H5F_t *H5VL_native_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
static herr_t H5VL_native_term(void);

static const H5VL_class_t H5VL_native_g = {
    "native",					/* name */
    H5VL_native_term,                           /*terminate             */
    0, 						/*fapl_size		*/
    NULL,					/*fapl_get		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    {                                           /* general_cls */
        NULL,                                   /* open */
        NULL,                                   /* create */
        NULL,                                   /* exists */
        NULL                                    /* close */
    },
    {                                           /* file_cls */
        H5VL_native_open,                       /* open */
        H5VL_native_close,                      /* close */
        H5VL_native_create,                     /* create */
        NULL,                                   /* flush */
        NULL,                                   /* is_hdf5 */
        NULL,                                   /* mount */
        NULL,                                   /* unmount */
        NULL                                    /* reopen */
    },
    {                                           /* dataset_cls */
        NULL,                                   /* read */
        NULL,                                   /* write */
        NULL,                                   /* set_extent */
    },
    {                                           /* attribute_cls */
        NULL,                                   /* create_by_name */
        NULL,                                   /* delete */
        NULL,                                   /* delete_by_idx */
        NULL,                                   /* delete_by_name */
        NULL,                                   /* open_by_idx */
        NULL,                                   /* open_by_name */
        NULL,                                   /* open_idx */
        NULL,                                   /* read */
        NULL,                                   /* write */
        NULL                                    /* rename */
    },
    {                                           /* datatype_cls */
        NULL,                                   /* commit */
    },
    {                                           /* link_cls */
        NULL,                                   /* create */
        NULL,                                   /* delete */
        NULL,                                   /* move */
        NULL,                                   /* copy */
        NULL,                                   /* delete_by_idx */
        NULL,                                   /* create_external */
        NULL                                    /* create_ud */
    },
    {                                           /* object_cls */
        NULL,                                   /* set_comment */
        NULL,                                   /* open_by_addr */
        NULL,                                   /* open_by_idx */
        NULL,                                   /* copy */
        NULL,                                   /* incr_refcount */
        NULL                                    /* decr_refcount */
    }
};


/*--------------------------------------------------------------------------
NAME
   H5VL_native_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_native_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_native_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_native_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5VL_native_init_interface)

    FUNC_LEAVE_NOAPI(H5VL_native_init())
} /* H5VL_native_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the native plugin.
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_native_init(void)
{
    hid_t ret_value;            /* Return value */

    FUNC_ENTER_NOAPI(H5VL_native_init, FAIL)

    if(H5I_VOL != H5I_get_type(H5VL_NATIVE_g))
        H5VL_NATIVE_g = H5VL_register(&H5VL_native_g, sizeof(H5VL_class_t), FALSE);

    /* Set return value */
    ret_value = H5VL_NATIVE_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_init() */


/*---------------------------------------------------------------------------
 * Function:	H5VL_native_term
 *
 * Purpose:	Shut down the VOL plugin
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL_native_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5VL_native_term)

    /* Reset VOL ID */
    H5VL_NATIVE_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_native_term() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_native
 *
 * Purpose:	Modify the file access property list to use the H5VL_NATIVE
 *		plugin defined in this source file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_native(hid_t fapl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(H5Pset_fapl_native, FAIL)
    H5TRACE1("e", "i", fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_vol(plist, H5VL_NATIVE, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_native() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_open
 *
 * Purpose:	Opens a file as a native HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static H5F_t *
H5VL_native_open(const char *name, unsigned flags, hid_t fcpl_id, 
                 hid_t fapl_id, hid_t dxpl_id)
{
    H5F_t *ret_value = NULL;           /* file struct */

    FUNC_ENTER_NOAPI_NOINIT(H5VL_native_open)

    /* Open the file */ 
    if(NULL == (ret_value= H5F_open(name, flags, fcpl_id, fapl_id, dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_create
 *
 * Purpose:	Creates a file as a native HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static H5F_t *
H5VL_native_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    H5F_t *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5VL_native_create)

    /* Create the file */ 
    if(NULL == (ret_value = H5F_open(name, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_close
 *
 * Purpose:	Closes a file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_close(H5F_t *f)
{
    int nref;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5VL_native_close)

    /* Flush file if this is the last reference to this id and we have write
     * intent, unless it will be flushed by the "shared" file being closed.
     * This is only necessary to replicate previous behaviour, and could be
     * disabled by an option/property to improve performance. */
    if((f->shared->nrefs > 1) && (H5F_INTENT(f) & H5F_ACC_RDWR)) {
        if((nref = H5I_get_ref(f->file_id, FALSE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get ID ref count")
        if(nref == 1)
            if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    } /* end if */

    /*
     * Decrement reference count on atom.  When it reaches zero the file will
     * be closed.
     */
    if(H5I_dec_app_ref(f->file_id) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_close() */
