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
 * Purpose:	The dummy VOL plugin where access is to a single HDF5 file 
 *              using HDF5 VFDs. 
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_dummy_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLdummy.h"         /* Dummy VOL plugin			*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/

/* The driver identification number, initialized at runtime */
static hid_t H5VL_DUMMY_g = 0;


/* Prototypes */
static herr_t H5VL_dummy_get(hid_t file_id, H5VL_file_get_t get_type, 
                              void *data, int argc, void **argv);
static herr_t H5VL_dummy_term(void);

static const H5VL_class_t H5VL_dummy_g = {
    "dummy",					/* name */
    H5VL_dummy_term,                           /*terminate             */
    0, 						/*fapl_size		*/
    NULL,					/*fapl_get		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    {                                           /* file_cls */
        H5VL_dummy_open,                       /* open */
        H5VL_dummy_close,                      /* close */
        H5VL_dummy_create,                     /* create */
        NULL,                                  /* flush */
        NULL                                   /* get */
    },
    {                                           /* dataset_cls */
        NULL,                                   /* open */
        NULL,                                   /* close */
        NULL,                                   /* create */
        NULL,                                   /* read */
        NULL,                                   /* write */
        NULL                                    /* set_extent */
    },
    {                                           /* attribute_cls */
        NULL,                                   /* open */
        NULL,                                   /* close */
        NULL,                                   /* create */
        NULL,                                   /* delete */
        NULL,                                   /* read */
        NULL                                    /* write */
    },
    {                                           /* datatype_cls */
        NULL                                   /* open */
    },
    {                                           /* link_cls */
        NULL,                                   /* create */
        NULL,                                   /* delete */
        NULL,                                   /* move */
        NULL                                    /* copy */
    },
    {                                           /* object_cls */
        NULL,                                   /* create */
        NULL,                                   /* open */
        NULL,                                   /* close */
        NULL,                                   /* move */
        NULL                                    /* copy */
    }
};


/*--------------------------------------------------------------------------
NAME
   H5VL_dummy_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_dummy_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_dummy_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_dummy_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5VL_dummy_init())
} /* H5VL_dummy_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dummy_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the dummy plugin.
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_dummy_init(void)
{
    hid_t ret_value;            /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5I_VOL != H5I_get_type(H5VL_DUMMY_g))
        H5VL_DUMMY_g = H5VL_register(&H5VL_dummy_g, sizeof(H5VL_class_t), FALSE);

    /* Set return value */
    ret_value = H5VL_DUMMY_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dummy_init() */


/*---------------------------------------------------------------------------
 * Function:	H5VL_dummy_term
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
H5VL_dummy_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VOL ID */
    H5VL_DUMMY_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_dummy_term() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_dummy
 *
 * Purpose:	Modify the file access property list to use the H5VL_DUMMY
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
H5Pset_fapl_dummy(hid_t fapl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_vol(plist, H5VL_DUMMY, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_dummy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dummy_open
 *
 * Purpose:	Opens a file as a dummy HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_dummy_open(const char *name, unsigned flags, hid_t fcpl_id, 
                 hid_t fapl_id, hid_t dxpl_id)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    printf ("DUMMY OPEN\n");

    FUNC_LEAVE_NOAPI(1)
} /* end H5VL_dummy_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dummy_create
 *
 * Purpose:	Creates a file as a dummy HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_dummy_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    printf ("DUMMY CREATE\n");

    FUNC_LEAVE_NOAPI(2)
} /* end H5VL_dummy_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dummy_close
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
herr_t
H5VL_dummy_close(hid_t file_id)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    printf ("DUMMY CLOSE\n");

    FUNC_LEAVE_NOAPI(1)
} /* end H5VL_dummy_close() */
