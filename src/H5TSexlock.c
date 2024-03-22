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
 * Purpose: This file contains support for recursive exclusive locks, equivalent to
 *        the pthread 'pthread_mutex_t' type and capabilities, except that
 *        threads that hold the lock are allowed to acquire access again
 *        (and must match each lock with an unlock operation).
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

#ifdef H5_HAVE_THREADSAFE

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

/* Default value to initialize exclusive locks */
static const H5TS_ex_lock_t H5TS_ex_lock_def = H5TS_EX_LOCK_INIT;

/*--------------------------------------------------------------------------
 * Function:    H5TS__ex_lock_init
 *
 * Purpose:     Initialize the supplied instance of H5TS_ex_lock_t.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__ex_lock_init(H5TS_ex_lock_t *lock, bool disable_cancel)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == lock))
        HGOTO_DONE(FAIL);

    /* Initialize the lock */
    memcpy(lock, &H5TS_ex_lock_def, sizeof(H5TS_ex_lock_def));

#ifdef H5_HAVE_PTHREAD_H
    /* Set non-default fields */
    lock->disable_cancel = disable_cancel;
#else
    (void)disable_cancel;
#endif

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__ex_lock_init() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__ex_lock
 *
 * Purpose:     Acquire a lock on the associated recursive exclusive lock.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__ex_lock(H5TS_ex_lock_t *lock)
{
    H5TS_thread_t my_thread_id = H5TS_thread_self();
    bool          have_mutex   = false;
    herr_t        ret_value    = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Acquire the mutex for the lock */
    if (H5_UNLIKELY(H5TS_mutex_lock(&lock->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Check if this thread already owns the lock */
    if (lock->lock_count && H5TS_thread_equal(my_thread_id, lock->owner_thread))
        /* Already owned by self - increment count */
        lock->lock_count++;
    else {
        /* Wait until the mutex is released by current owner thread */
        while (lock->lock_count)
            if (H5_UNLIKELY(H5TS_cond_wait(&lock->cond_var, &lock->mutex) < 0))
                HGOTO_DONE(FAIL);

        /* After we've received the signal, take ownership of the lock */
        lock->owner_thread = my_thread_id;
        lock->lock_count   = 1;

#ifdef H5_HAVE_PTHREAD_H
        /* Disable cancellation, if requested for this lock */
        if (lock->disable_cancel)
            /* Set cancellation state to 'disable', and remember previous state */
            if (H5_UNLIKELY(pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &lock->previous_state)))
                HGOTO_DONE(FAIL);
#endif
    }

done:
    if (H5_LIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&lock->mutex) < 0))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__ex_lock() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__ex_acquire
 *
 * Purpose:     Attempts to acquire an exclusive lock, without blocking
 *
 * Note:        On success, the 'acquired' flag indicates if lock was acquired.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__ex_acquire(H5TS_ex_lock_t *lock, unsigned lock_count, bool *acquired)
{
    bool          have_mutex   = false;
    H5TS_thread_t my_thread_id = H5TS_thread_self();
    herr_t        ret_value    = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    /* Attempt to acquire the lock's mutex */
    if (H5_UNLIKELY(H5TS_mutex_lock(&lock->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Check if locked already */
    if (lock->lock_count) {
        /* Check for this thread already owning the lock */
        if (H5TS_thread_equal(my_thread_id, lock->owner_thread)) {
            /* Already owned by self - increment count */
            lock->lock_count += lock_count;
            *acquired = true;
        }
        else
            *acquired = false;
    }
    else {
        /* Take ownership of the lock */
        lock->owner_thread = my_thread_id;
        lock->lock_count   = lock_count;
        *acquired          = true;
    }

done:
    /* Release the mutex, if acquired */
    if (H5_LIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&lock->mutex) < 0))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__ex_acquire() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__ex_release
 *
 * Purpose:     Release an exclusive lock.  Passes back the previous lock count
 *              to the caller in a parameter.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__ex_release(H5TS_ex_lock_t *lock, unsigned int *lock_count)
{
    bool   have_mutex = false;
    herr_t ret_value  = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Attempt to acquire the lock's mutex */
    if (H5_UNLIKELY(H5TS_mutex_lock(&lock->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Reset the lock count for this thread */
    *lock_count      = lock->lock_count;
    lock->lock_count = 0;

    /* Signal the condition variable, to wake any thread waiting on the lock */
    if (H5_UNLIKELY(H5TS_cond_signal(&lock->cond_var) < 0))
        HGOTO_DONE(FAIL);

done:
    if (H5_LIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&lock->mutex) < 0))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* H5TS__ex_release */

/*--------------------------------------------------------------------------
 * Function:    H5TS__ex_unlock
 *
 * Purpose:     Decrements the lock counter, releasing the lock when the counter
 *              reaches 0.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__ex_unlock(H5TS_ex_lock_t *lock)
{
    bool   have_mutex = false;
    herr_t ret_value  = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Acquire the mutex for the lock */
    if (H5_UNLIKELY(H5TS_mutex_lock(&lock->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Decrement the lock count for this thread */
    lock->lock_count--;

    if (lock->lock_count == 0) {
#ifdef H5_HAVE_PTHREAD_H
        /* Restore previous cancellation state, if requested for this lock */
        if (lock->disable_cancel)
            if (H5_UNLIKELY(pthread_setcancelstate(lock->previous_state, NULL)))
                HGOTO_DONE(FAIL);
#endif

        /* If the lock count drops to zero, signal the condition variable, to
         * wake any thread waiting on the lock.
         */
        if (H5_UNLIKELY(H5TS_cond_signal(&lock->cond_var) < 0))
            HGOTO_DONE(FAIL);
    }

done:
    if (H5_LIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&lock->mutex) < 0))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* H5TS__ex_unlock */

/*--------------------------------------------------------------------------
 * Function:    H5TS__ex_lock_destroy
 *
 * Purpose:     Destroy an exlusive lock.  All mutex, condition variables,
 *		etc. are destroyed.  However, the instance of H5TS_ex_lock_t
 *		is not freed.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__ex_lock_destroy(H5TS_ex_lock_t *lock)
{
    bool   have_mutex = false;
    herr_t ret_value  = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == lock))
        HGOTO_DONE(FAIL);

    /* Acquire the mutex for the lock */
    if (H5_UNLIKELY(H5TS_mutex_lock(&lock->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Fail if this thread owns the lock */
    if (lock->lock_count && H5TS_thread_equal(H5TS_thread_self(), lock->owner_thread))
        HGOTO_DONE(FAIL);

    /* Release the mutex for the lock */
    if (H5_UNLIKELY(H5TS_mutex_unlock(&lock->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = false;

    /* Call the appropriate destroy routines.  We are committed
     * to the destroy at this point, so call them all, even if one fails
     * along the way.
     */
    if (H5_UNLIKELY(H5TS_mutex_destroy(&lock->mutex) < 0))
        ret_value = FAIL;
    if (H5_UNLIKELY(H5TS_cond_destroy(&lock->cond_var) < 0))
        ret_value = FAIL;

done:
    if (H5_UNLIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&lock->mutex) < 0))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__ex_lock_destroy() */

#endif /* H5_HAVE_THREADSAFE */
