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

/* Programmer:  Quincey Koziol
 *              Saturday, August 22, 2020
 *
 * Purpose:     Streaming I/O routines, for native VOL connector.
 *
 */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h"          /* This source code file is part of the H5D module */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                        */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Dpkg.h"             /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#ifdef LATER
#include "H5FLprivate.h"        /* Free lists                               */
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5VLprivate.h"        /* Virtual Object Layer                     */
#include "H5VLnative_private.h" /* Native VOL connector                     */
#endif /* LATER */


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


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5D__stream_start
 *
 * Purpose:     Start streaming I/O for a dataset.
 *
 *              This routine caches information that would otherwise be
 *              repeatedly set up / torn down by append & sequence operations.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_start(H5D_t *dset, hid_t dxpl_id, unsigned axis, size_t extension,
    hid_t mem_type_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    /* (Check datatype is convertible) */
    /* (Check axis is in range) */
    /* (Check layout is chunked) */


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__stream_start() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_append
 *
 * Purpose:     Append data to dataset
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_append(H5D_t *dset, const void *buf)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__stream_append() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_sequence
 *
 * Purpose:     Sequentially read data to dataset
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_sequence(H5D_t *dset, void *buf)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__stream_sequence() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_is_streaming
 *
 * Purpose:     Query if a dataset is in streamed I/O mode
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_is_streaming(const H5D_t *dset, hbool_t *is_streaming)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__stream_is_streaming() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_stop
 *
 * Purpose:     Stop streaming I/O for a dataset.
 *
 *              This routine releases cached streaming information.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_stop(H5D_t *dset)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__stream_stop() */

