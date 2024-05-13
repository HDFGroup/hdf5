/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright (c) 2024 NVIDIA CORPORATION & AFFILIATES. All rights reserved.  *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: This file contains support for non-recursive R/W locks, equivalent
 *        to the pthread 'pthread_rwlock_t' type and capabilities.
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

#ifdef H5_HAVE_C11_THREADS
/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_init
 *
 * Purpose:  Initialize a H5TS_rwlock_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_init(H5TS_rwlock_t *lock)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Initialize synchronization primitives */
    if (H5_UNLIKELY(mtx_init(&lock->mutex, mtx_plain) != thrd_success))
        HGOTO_DONE(FAIL);
    if (H5_UNLIKELY(cnd_init(&lock->read_cv) != thrd_success))
        HGOTO_DONE(FAIL);
    if (H5_UNLIKELY(cnd_init(&lock->write_cv) != thrd_success))
        HGOTO_DONE(FAIL);

    /* Initialize scalar fields */
    lock->readers = 0;
    lock->writers = 0;
    lock->read_waiters = 0;
    lock->write_waiters = 0;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_rdlock
 *
 * Purpose:  Acquire a read lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_rdlock(H5TS_rwlock_t *lock)
{
    bool   have_mutex = false;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Acquire the lock's mutex */
    if (H5_UNLIKELY(mtx_lock(&lock->mutex) != thrd_success))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Check for writers */
    if (lock->writers || lock->write_waiters) {
        /* Read waiting */
        lock->read_waiters++;

        /* Wait for writers */
        do {
            if (H5_UNLIKELY(thrd_success != cnd_wait(&lock->read_cv, &lock->mutex)))
                HGOTO_DONE(FAIL);
        } while (lock->writers || lock->write_waiters);

        /* Read not waiting any longer */
        lock->read_waiters--;
    }

    /* Increment # of readers */
    lock->readers++;

done:
    /* Release mutex, if we're holding it */
    if (H5_LIKELY(have_mutex))
	if (H5_UNLIKELY(mtx_unlock(&lock->mutex) != thrd_success))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_rdlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_rdunlock
 *
 * Purpose:  Release a read lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_rdunlock(H5TS_rwlock_t *lock)
{
    bool   have_mutex = false;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Acquire the lock's mutex */
    if (H5_UNLIKELY(mtx_lock(&lock->mutex) != thrd_success))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Decrement # of readers */
    lock->readers--;

    /* Check for waiting writers when last readers */
    if (lock->write_waiters && 0 == lock->readers)
        if (H5_UNLIKELY(cnd_signal(&lock->write_cv) != thrd_success))
            HGOTO_DONE(FAIL);

done:
    /* Release mutex, if we're holding it */
    if (H5_LIKELY(have_mutex))
	if (H5_UNLIKELY(mtx_unlock(&lock->mutex) != thrd_success))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_rdunlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_wrlock
 *
 * Purpose:  Acquire a write lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_wrlock(H5TS_rwlock_t *lock)
{
    bool   have_mutex = false;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Acquire the lock's mutex */
    if (H5_UNLIKELY(mtx_lock(&lock->mutex) != thrd_success))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Check for readers or other writers */
    if (lock->readers || lock->writers) {
        /* Write waiting */
        lock->write_waiters++;

        /* Wait for mutex */
        do {
            if (H5_UNLIKELY(thrd_success != cnd_wait(&lock->write_cv, &lock->mutex)))
                HGOTO_DONE(FAIL);
        } while (lock->readers || lock->writers);

        /* Write not waiting any longer */
        lock->write_waiters--;
    }

    /* Increment # of writers */
    lock->writers++;

done:
    /* Release mutex, if we're holding it */
    if (H5_LIKELY(have_mutex))
	if (H5_UNLIKELY(mtx_unlock(&lock->mutex) != thrd_success))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_wrlock() */

-------------------------------------------------------------------------
 * Function: H5TS_rwlock_wrunlock
 *
 * Purpose:  Release a write lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_wrunlock(H5TS_rwlock_t *lock)
{
    bool   have_mutex = false;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Acquire the lock's mutex */
    if (H5_UNLIKELY(mtx_lock(&lock->mutex) != thrd_success))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Decrement # of writers */
    lock->writers--;

    /* Check for waiting writers */
    if (lock->write_waiters) {
        if (H5_UNLIKELY(cnd_signal(&lock->write_cv) != thrd_success))
            HGOTO_DONE(FAIL);
    } else if (lock->read_waiters)
	if (H5_UNLIKELY(cnd_broadcast(&lock->read_cv) != thrd_success))
            HGOTO_DONE(FAIL);

done:
    /* Release mutex, if we're holding it */
    if (H5_LIKELY(have_mutex))
	if (H5_UNLIKELY(mtx_unlock(&lock->mutex) != thrd_success))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_wrunlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_destroy
 *
 * Purpose:  Destroy a H5TS_rwlock_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_destroy(H5TS_rwlock_t *lock)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Destroy synchronization primitives */
    /* NOTE: mtx_destroy() & cnd_destroy() can't fail */
    mtx_destroy(&lock->mutex);
    cnd_destroy(&lock->read_cv);
    cnd_destroy(&lock->write_cv);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_rwlock_destroy() */
#else
#ifdef H5_HAVE_WIN_THREADS
/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_init
 *
 * Purpose:  Initialize a H5TS_rwlock_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_init(H5TS_rwlock_t *lock)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    InitializeSRWLock(lock);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_rwlock_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_rdlock
 *
 * Purpose:  Acquire a read lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_rdlock(H5TS_rwlock_t *lock)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    AcquireSRWLockShared(lock);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_rwlock_rdlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_rdunlock
 *
 * Purpose:  Release a read lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_rdunlock(H5TS_rwlock_t *lock)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    ReleaseSRWLockShared(lock);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_rwlock_rdunlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_wrlock
 *
 * Purpose:  Acquire a write lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_wrlock(H5TS_rwlock_t *lock)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    AcquireSRWLockExclusive(lock);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_rwlock_wrlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_wrunlock
 *
 * Purpose:  Release a write lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_wrunlock(H5TS_rwlock_t *lock)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    ReleaseSRWLockExclusive(lock);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_rwlock_wrunlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_destroy
 *
 * Purpose:  Destroy a H5TS_rwlock_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_destroy(H5TS_rwlock_t *lock)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Destroy synchronization primitives */
    /* SRWLOCKs don't have to be destroyed */

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_rwlock_destroy() */
#else
/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_init
 *
 * Purpose:  Initialize a H5TS_rwlock_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_init(H5TS_rwlock_t *lock)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_rwlock_init(lock, NULL)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_rdlock
 *
 * Purpose:  Acquire a read lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_rdlock(H5TS_rwlock_t *lock)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_rwlock_rdlock(lock)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_rdlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_rdunlock
 *
 * Purpose:  Release a read lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_rdunlock(H5TS_rwlock_t *lock)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_rwlock_unlock(lock)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_rdunlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_wrlock
 *
 * Purpose:  Acquire a write lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_wrlock(H5TS_rwlock_t *lock)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_rwlock_wrlock(lock)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_wrlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_rdunlock
 *
 * Purpose:  Release a write lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_wrunlock(H5TS_rwlock_t *lock)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_rwlock_unlock(lock)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_wrunlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_destroy
 *
 * Purpose:  Destroy a H5TS_rwlock_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_rwlock_destroy(H5TS_rwlock_t *lock)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(pthread_rwlock_destroy(lock)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_rwlock_destroy() */
#endif
#endif

#endif /* H5_HAVE_THREADS */

