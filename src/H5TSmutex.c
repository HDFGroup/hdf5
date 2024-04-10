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
 *        pthread 'pthread_mutex_t' type and capabilities.
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
#define H5TS_NO_THREAD_SAFETY_ANALYSIS H5_ATTR_THREAD_ANNOT(no_thread_safety_analysis)

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

#ifdef H5_HAVE_C11_THREADS
/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_init
 *
 * Purpose:  Initialize a H5TS_mutex_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_init(H5TS_mutex_t *mutex, int type)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(mtx_init(mutex, type) != thrd_success))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_lock
 *
 * Purpose:  Lock a H5TS_mutex_t
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_lock(H5TS_mutex_t *mutex) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(mtx_lock(mutex) != thrd_success))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_lock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_trylock
 *
 * Purpose:  Attempt to lock a H5TS_mutex_t, sets *acquired to TRUE if so
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_trylock(H5TS_mutex_t *mutex, bool *acquired) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    int    rc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    rc = mtx_trylock(mutex);
    if (thrd_success == rc)
        *acquired = true;
    else if (thrd_busy == rc)
        *acquired = false;
    else
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_trylock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_unlock
 *
 * Purpose:  Unlock a H5TS_mutex_t
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_unlock(H5TS_mutex_t *mutex) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(mtx_unlock(mutex) != thrd_success))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_unlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_destroy
 *
 * Purpose:  Destroy a H5TS_mutex_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_destroy(H5TS_mutex_t *mutex)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* NOTE: mtx_destroy() can't fail */
    mtx_destroy(mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_mutex_destroy() */
#else
#ifdef H5_HAVE_WIN_THREADS
/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_init
 *
 * Purpose:  Initialize a H5TS_mutex_t (does not allocate it)
 *
 * Note:     All Windows CriticalSections are recursive
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_init(H5TS_mutex_t *mutex, int H5_ATTR_UNUSED type)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    InitializeCriticalSection(mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_mutex_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_lock
 *
 * Purpose:  Lock a H5TS_mutex_t
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_lock(H5TS_mutex_t *mutex) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    EnterCriticalSection(mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_mutex_lock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_trylock
 *
 * Purpose:  Attempt to lock a H5TS_mutex_t, sets *acquired to TRUE if so
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_trylock(H5TS_mutex_t *mutex, bool *acquired) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    *acquired = TryEnterCriticalSection(mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_mutex_trylock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_unlock
 *
 * Purpose:  Unlock a H5TS_mutex_t
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_unlock(H5TS_mutex_t *mutex) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    LeaveCriticalSection(mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_mutex_unlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_destroy
 *
 * Purpose:  Destroy a H5TS_mutex_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_destroy(H5TS_mutex_t *mutex)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    DeleteCriticalSection(mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_mutex_destroy() */

#else
/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_init
 *
 * Purpose:  Initialize a H5TS_mutex_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_init(H5TS_mutex_t *mutex, int type)
{
    pthread_mutexattr_t  _attr;
    pthread_mutexattr_t *attr      = NULL;
    herr_t               ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Set up recursive mutex, if requested */
    if (H5TS_MUTEX_TYPE_RECURSIVE == type) {
        if (H5_UNLIKELY(pthread_mutexattr_init(&_attr)))
            HGOTO_DONE(FAIL);
        attr = &_attr;

        if (H5_UNLIKELY(pthread_mutexattr_settype(attr, PTHREAD_MUTEX_RECURSIVE)))
            HGOTO_DONE(FAIL);
    }

    if (H5_UNLIKELY(pthread_mutex_init(mutex, attr)))
        HGOTO_DONE(FAIL);

done:
    if (NULL != attr)
        if (H5_UNLIKELY(pthread_mutexattr_destroy(attr)))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_lock
 *
 * Purpose:  Lock a H5TS_mutex_t
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_lock(H5TS_mutex_t *mutex) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_mutex_lock(mutex)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_lock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_trylock
 *
 * Purpose:  Attempt to lock a H5TS_mutex_t, sets *acquired to TRUE if so
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_trylock(H5TS_mutex_t *mutex, bool *acquired) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    int    rc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    rc = pthread_mutex_trylock(mutex);
    if (0 == rc)
        *acquired = true;
    else if (EBUSY == rc)
        *acquired = false;
    else
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_trylock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_unlock
 *
 * Purpose:  Unlock a H5TS_mutex_t
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_unlock(H5TS_mutex_t *mutex) H5TS_NO_THREAD_SAFETY_ANALYSIS
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_mutex_unlock(mutex)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_unlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_mutex_destroy
 *
 * Purpose:  Destroy a H5TS_mutex_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_destroy(H5TS_mutex_t *mutex)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_mutex_destroy(mutex)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_mutex_destroy() */

#endif
#endif

#endif /* H5_HAVE_THREADS */
