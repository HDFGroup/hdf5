/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: This file contains the framework for ensuring that the global
 *        library lock is held when an API routine is called.  This framework
 *        works in concert with the FUNC_ENTER_API / FUNC_LEAVE_API macros
 *        defined in H5private.h.
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

#ifdef H5_HAVE_THREADSAFE_API

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

/* API threadsafety info */
H5TS_api_info_t H5TS_api_info_p;

/*****************************/
/* Library Private Variables */
/*****************************/

/* Global thread pool */
H5TS_pool_t *H5TS_pool_g = NULL;

#ifdef H5_HAVE_CONCURRENCY
/* Whether there are concurrent threads in the library (from internal spawning) */
bool H5TS_currently_concurrent_g = false;
#endif /* H5_HAVE_CONCURRENCY */

/*******************/
/* Local Variables */
/*******************/

/*--------------------------------------------------------------------------
 * Function:    H5TSmutex_acquire
 *
 * Purpose:     Attempts to acquire the HDF5 library global lock. Should be preceded by a call to
 *              H5TSmutex_release().
 *
 * Parameters:
 *              lock_count; IN: The lock count that was held on the mutex before its release
 *              acquired; OUT: Whether the HDF5 library global lock was acquired
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TSmutex_acquire(unsigned lock_count, bool *acquired)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NAMECHECK_ONLY

    /* Acquire the "API" lock */
    if (H5_UNLIKELY(H5TS__api_mutex_acquire(lock_count, acquired) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_API_NAMECHECK_ONLY(ret_value)
} /* end H5TSmutex_acquire() */

/*--------------------------------------------------------------------------
 * Function:    H5TSmutex_get_attempt_count
 *
 * Purpose:     Get the current count of the global lock attempt
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Houjun Tang
 *              June 24, 2019
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TSmutex_get_attempt_count(unsigned *count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NAMECHECK_ONLY

    *count = H5TS_atomic_load_uint(&H5TS_api_info_p.attempt_lock_count);

    FUNC_LEAVE_API_NAMECHECK_ONLY(ret_value)
} /* end H5TSmutex_get_attempt_count() */

/*--------------------------------------------------------------------------
 * Function:    H5TSmutex_release
 *
 * Purpose:     Releases the HDF5 library global lock. Should be followed by a call to H5TSmutex_acquire().
 *
 *              This should be used by applications to temporarily release the lock in order to either perform
 *              multi-threaded work of their own or yield control to another thread using HDF5. The value
 *              returned in lock_count should be provided to H5TSmutex_acquire() in order to resume a
 *              consistent library state.
 *
 * Parameters:
 *              lock_count; OUT: The current lock count for the calling thread.
 *
 * Return:      Non-negative on success / Negative on failure
 *--------------------------------------------------------------------------
 */
herr_t
H5TSmutex_release(unsigned *lock_count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NAMECHECK_ONLY

    /* Release the "API" lock */
    *lock_count = 0;
    if (H5_UNLIKELY(H5TS__api_mutex_release(lock_count) < 0))
        ret_value = FAIL;

    FUNC_LEAVE_API_NAMECHECK_ONLY(ret_value)
} /* end H5TSmutex_release() */

#ifdef H5_HAVE_CONCURRENCY
/*--------------------------------------------------------------------------
 * Function:    H5TSglobal_pool_create
 *
 * Purpose:     Creates a global thread pool for the HDF5 library to use to
 *              accelerate parallelizable operations. The thread pool must
 *              not already exist.
 *
 *              This function does use the error stack because it is not
 *              meant to be called within a concurrent section.
 *
 * Parameters:
 *              num_threads; IN: The number of threads to add to the newly
 *              created thread pool.
 *
 * Return:      Non-negative on success / Negative on failure
 *--------------------------------------------------------------------------
 */
herr_t
H5TSglobal_pool_create(unsigned num_threads)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check if pool already exists */
    if (H5TS_pool_g)
        HGOTO_ERROR(H5E_LIB, H5E_ALREADYEXISTS, FAIL, "global thread pool already exists");

    /* Create global thread pool */
    if (H5TS_pool_create(&H5TS_pool_g, num_threads) < 0)
        HGOTO_ERROR(H5E_LIB, H5E_CANTINIT, FAIL, "can't create thread pool");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5TSglobal_pool_create() */

/*--------------------------------------------------------------------------
 * Function:    H5TSglobal_pool_create
 *
 * Purpose:     Destroys the global thread pool created with
 *              H5TSglobal_pool_create().
 *
 *              This function does use the error stack because it is not
 *              meant to be called within a concurrent section.
 *
 * Return:      Non-negative on success / Negative on failure
 *--------------------------------------------------------------------------
 */
herr_t
H5TSglobal_pool_destroy(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check if pool exists */
    if (!H5TS_pool_g)
        HGOTO_ERROR(H5E_LIB, H5E_CANTFREE, FAIL, "global thread pool does not exist");

    /* Destroy global thread pool */
    if (H5TS_pool_destroy(H5TS_pool_g) < 0)
        HGOTO_ERROR(H5E_LIB, H5E_CANTFREE, FAIL, "can't destroy thread pool");
    H5TS_pool_g = NULL;

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5TSglobal_pool_destroy() */
#endif /* H5_HAVE_CONCURRENCY */

#endif /* H5_HAVE_THREADSAFE_API */
