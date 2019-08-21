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
 * Purpose:     Blob callbacks for the native VOL connector
 */

/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Fprivate.h"         /* File access				*/
#include "H5HGprivate.h"	/* Global Heaps				*/
#include "H5VLnative_private.h" /* Native VOL connector                 */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5VL__native_blob_put
 *
 * Purpose:     Handles the blob 'put' callback
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *		Friday, August 15, 2019
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__native_blob_put(void *blob, size_t size, void *_ctx, void *_id)
{
    uint8_t *id = (uint8_t *)_id;       /* Pointer to blob ID */
    H5F_t *f = (H5F_t *)_ctx;           /* Retrieve file pointer from context */
    H5HG_t hobjid;                      /* New VL sequence's heap ID */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check parameters */
    HDassert(id);
    HDassert(size == 0 || blob);
    HDassert(f);

    /* Write the VL information to disk (allocates space also) */
    if(H5HG_insert(f, size, blob, &hobjid) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "unable to write blob information")

    /* Encode the heap information */
    H5F_addr_encode(f, &id, hobjid.addr);
    UINT32ENCODE(id, hobjid.idx);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__native_blob_put() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__native_blob_get
 *
 * Purpose:     Handles the blob 'get' callback
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *		Friday, August 15, 2019
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__native_blob_get(const void *_id, void *_ctx, void *buf)
{
    const uint8_t *id = (const uint8_t *)_id; /* Pointer to the disk blob ID */
    H5F_t *f = (H5F_t *)_ctx;           /* Retrieve file pointer from context */
    H5HG_t hobjid;                      /* Global heap ID for sequence */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(id);
    HDassert(f);
    HDassert(buf);

    /* Get the heap information */
    H5F_addr_decode(f, &id, &hobjid.addr);
    UINT32DECODE(id, hobjid.idx);

    /* Check if this sequence actually has any data */
    if(hobjid.addr > 0)
        /* Read the VL information from disk */
        if(NULL == H5HG_read(f, &hobjid, buf, NULL))
            HGOTO_ERROR(H5E_VOL, H5E_READERROR, FAIL, "unable to read VL information")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__native_blob_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__native_blob_specific
 *
 * Purpose:     Handles the blob 'specific' callback
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *		Friday, August 15, 2019
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__native_blob_specific(void *_id, H5VL_blob_specific_t specific_type,
    va_list arguments)
{
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    switch(specific_type) {
        case H5VL_BLOB_GETSIZE:
            {
                const uint8_t *id = (const uint8_t *)_id; /* Pointer to the blob ID */
                H5F_t *f = HDva_arg(arguments, H5F_t *);
                size_t *size = HDva_arg(arguments, size_t *);
                H5HG_t hobjid;              /* blob's heap ID */

                /* Get heap information */
                H5F_addr_decode(f, &id, &(hobjid.addr));
                UINT32DECODE(id, hobjid.idx);

                /* Free heap object */
                if(hobjid.addr > 0) {
                    if(H5HG_get_obj_size(f, &hobjid, size) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTREMOVE, FAIL, "unable to remove heap object")
                } /* end if */
                else
                    *size = 0;  /* Return '0' size for 'nil' blob ID */

                break;
            }

        case H5VL_BLOB_ISNULL:
            {
                const uint8_t *id = (const uint8_t *)_id; /* Pointer to the blob ID */
                H5F_t *f = HDva_arg(arguments, H5F_t *);
                hbool_t *isnull = HDva_arg(arguments, hbool_t *);
                haddr_t addr;               /* Sequence's heap address */

                /* Get the heap address */
                H5F_addr_decode(f, &id, &addr);

                /* Check if heap address is 'nil' */
                *isnull = (addr == 0 ? TRUE : FALSE);

                break;
            }

        case H5VL_BLOB_SETNULL:
            {
                uint8_t *id = (uint8_t *)_id; /* Pointer to the blob ID */
                H5F_t *f = HDva_arg(arguments, H5F_t *);

                /* Encode the "nil" heap pointer information */
                H5F_addr_encode(f, &id, (haddr_t)0);
                UINT32ENCODE(id, 0);

                break;
            }

        case H5VL_BLOB_DELETE:
            {
                const uint8_t *id = (const uint8_t *)_id; /* Pointer to the blob ID */
                H5F_t *f = HDva_arg(arguments, H5F_t *);
                H5HG_t hobjid;              /* VL sequence's heap ID */

                /* Get heap information */
                H5F_addr_decode(f, &id, &hobjid.addr);
                UINT32DECODE(id, hobjid.idx);

                /* Free heap object */
                if(hobjid.addr > 0)
                    if(H5HG_remove(f, &hobjid) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTREMOVE, FAIL, "unable to remove heap object")

                break;
            }

        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid specific operation")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__native_blob_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__native_blob_optional
 *
 * Purpose:     Handles the blob 'optional' callback
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *		Friday, August 15, 2019
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__native_blob_optional(void *id, va_list arguments)
{
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__native_blob_optional() */

