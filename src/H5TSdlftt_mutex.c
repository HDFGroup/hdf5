/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: This file contains support for mutex locks, equivalent to the
 *        pthread 'pthread_mutex_t' type and capabilities, but efficiently
 *        obeying the "DLFFT" locking protocol.
 *
 * Note:  Because this threadsafety framework operates outside the library,
 *        it does not use the error stack (although it does use error macros
 *        that don't push errors on a stack) and only uses the "namecheck only"
 *        FUNC_ENTER_* / FUNC_LEAVE_* macros.
 */

/****************/
/* Module Setup */
/****************/

#include "H5TSmodule.h" /* This source code file is part of the H5TS module */

/***********/
/* Headers */
/***********/
#include "H5private.h"  /* Generic Functions                   */
#include "H5Eprivate.h" /* Error handling                      */
#include "H5TSpkg.h"    /* Threadsafety                        */

#ifdef H5_HAVE_THREADS

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
 * Function: H5TS_dlftt_mutex_init
 *
 * Purpose:  Initialize a H5TS_dlftt_mutex_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_dlftt_mutex_init(H5TS_dlftt_mutex_t *mutex)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(H5TS_mutex_init(&mutex->mtx, H5TS_MUTEX_TYPE_PLAIN)) < 0)
        HGOTO_DONE(FAIL);
    mutex->dlftt = 0;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_dlftt_mutex_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_dlftt_mutex_destroy
 *
 * Purpose:  Destroy a H5TS_dlftt_mutex_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_dlftt_mutex_destroy(H5TS_dlftt_mutex_t *mutex)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(H5TS_mutex_destroy(&mutex->mtx) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_dlftt_mutex_destroy() */

#endif /* H5_HAVE_THREADS */
