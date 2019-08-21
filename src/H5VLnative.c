/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose:     The native VOL connector where access is to a single HDF5 file
 *              using HDF5 VFDs.
 */

#include "H5private.h"          /* Generic Functions                        */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5Pprivate.h"         /* Property lists                           */
#include "H5VLprivate.h"        /* Virtual Object Layer                     */

#include "H5VLnative_private.h" /* Native VOL connector                     */


/* The VOL connector identification number */
static hid_t H5VL_NATIVE_ID_g = H5I_INVALID_HID;

/* Prototypes */
static herr_t H5VL__native_term(void);

/* Native VOL connector class struct */
static H5VL_class_t H5VL_native_cls_g = {
    H5VL_NATIVE_VERSION,                            /* version      */
    H5VL_NATIVE_VALUE,                              /* value        */
    H5VL_NATIVE_NAME,                               /* name         */
    0,                                              /* capability flags */
    NULL,                                           /* initialize   */
    H5VL__native_term,                              /* terminate    */
    {   /* info_cls */
        (size_t)0,                                  /* info size    */
        NULL,                                       /* info copy    */
        NULL,                                       /* info compare */
        NULL,                                       /* info free    */
        NULL,                                       /* info to str  */
        NULL                                        /* str to info  */
    },
    {   /* wrap_cls */
        NULL,                                       /* get_object   */
        NULL,                                       /* get_wrap_ctx */
        NULL,                                       /* wrap_object  */
        NULL,                                       /* unwrap_object */
        NULL                                        /* free_wrap_ctx */
    },
    {   /* attribute_cls */
        H5VL__native_attr_create,                   /* create       */
        H5VL__native_attr_open,                     /* open         */
        H5VL__native_attr_read,                     /* read         */
        H5VL__native_attr_write,                    /* write        */
        H5VL__native_attr_get,                      /* get          */
        H5VL__native_attr_specific,                 /* specific     */
        H5VL__native_attr_optional,                 /* optional     */
        H5VL__native_attr_close                     /* close        */
    },
    {   /* dataset_cls */
        H5VL__native_dataset_create,                /* create       */
        H5VL__native_dataset_open,                  /* open         */
        H5VL__native_dataset_read,                  /* read         */
        H5VL__native_dataset_write,                 /* write        */
        H5VL__native_dataset_get,                   /* get          */
        H5VL__native_dataset_specific,              /* specific     */
        H5VL__native_dataset_optional,              /* optional     */
        H5VL__native_dataset_close                  /* close        */
    },
    {   /* datatype_cls */
        H5VL__native_datatype_commit,               /* commit       */
        H5VL__native_datatype_open,                 /* open         */
        H5VL__native_datatype_get,                  /* get          */
        H5VL__native_datatype_specific,             /* specific     */
        NULL,                                       /* optional     */
        H5VL__native_datatype_close                 /* close        */
    },
    {   /* file_cls */
        H5VL__native_file_create,                   /* create       */
        H5VL__native_file_open,                     /* open         */
        H5VL__native_file_get,                      /* get          */
        H5VL__native_file_specific,                 /* specific     */
        H5VL__native_file_optional,                 /* optional     */
        H5VL__native_file_close                     /* close        */
    },
    {   /* group_cls */
        H5VL__native_group_create,                  /* create       */
        H5VL__native_group_open,                    /* open         */
        H5VL__native_group_get,                     /* get          */
        H5VL__native_group_specific,                /* specific     */
        H5VL__native_group_optional,                /* optional     */
        H5VL__native_group_close                    /* close        */
    },
    {   /* link_cls */
        H5VL__native_link_create,                   /* create       */
        H5VL__native_link_copy,                     /* copy         */
        H5VL__native_link_move,                     /* move         */
        H5VL__native_link_get,                      /* get          */
        H5VL__native_link_specific,                 /* specific     */
        NULL                                        /* optional     */
    },
    {   /* object_cls */
        H5VL__native_object_open,                   /* open         */
        H5VL__native_object_copy,                   /* copy         */
        H5VL__native_object_get,                    /* get          */
        H5VL__native_object_specific,               /* specific     */
        H5VL__native_object_optional                /* optional     */
    },
    {   /* request_cls */
        NULL,                                       /* wait         */
        NULL,                                       /* notify       */
        NULL,                                       /* cancel       */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* free         */
    },
    {   /* blob_cls */
        H5VL__native_blob_put,                      /* put */
        H5VL__native_blob_get,                      /* get */
        H5VL__native_blob_specific,                 /* specific */
        H5VL__native_blob_optional                  /* optional */
    },
    NULL                                            /* optional     */
};


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_register
 *
 * Purpose:     Register the native VOL connector and retrieve an ID for it.
 *
 * Return:      Success:    The ID for the native connector
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_native_register(void)
{
    hid_t ret_value = H5I_INVALID_HID;            /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Register the native VOL connector, if it isn't already */
    if(NULL == H5I_object_verify(H5VL_NATIVE_ID_g, H5I_VOL))
        if((H5VL_NATIVE_ID_g = H5VL_register_connector((const H5VL_class_t *)&H5VL_native_cls_g, TRUE, H5P_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINSERT, H5I_INVALID_HID, "can't create ID for native VOL connector")

    /* Set return value */
    ret_value = H5VL_NATIVE_ID_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_register() */


/*---------------------------------------------------------------------------
 * Function:    H5VL__native_term
 *
 * Purpose:     Shut down the native VOL
 *
 * Returns:     SUCCEED (Can't fail)
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL__native_term(void)
{
    FUNC_ENTER_STATIC_NOERR

    /* Reset VOL ID */
    H5VL_NATIVE_ID_g = H5I_INVALID_HID;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__native_term() */

