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
 * Purpose: This file contains support for thread barrier operations, equivalent
 *        to the pthread 'pthread_barrier_t' type and capabilities.
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

#ifndef H5_HAVE_PTHREAD_BARRIER
/* Barrier initialization macro */
#define H5TS_BARRIER_INIT                                                                                    \
    {                                                                                                        \
        PTHREAD_MUTEX_INITIALIZER,    /* mutex */                                                            \
            PTHREAD_COND_INITIALIZER, /* cv */                                                               \
            0,                        /* count */                                                            \
            0,                        /* entered */                                                          \
            0                         /* threshold */                                                        \
    }
#endif

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

/* Default value to initialize barriers */
#ifndef H5_HAVE_PTHREAD_BARRIER
static const H5TS_barrier_t H5TS_barrier_def = H5TS_BARRIER_INIT;
#endif

/*--------------------------------------------------------------------------
 * Function:    H5TS__barrier_init
 *
 * Purpose:     Initialize a thread barrier
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__barrier_init(H5TS_barrier_t *barrier, uint64_t count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == barrier || 0 == count))
        HGOTO_DONE(FAIL);

        /* Initialize the barrier */
#ifdef H5_HAVE_PTHREAD_BARRIER
    if (H5_UNLIKELY(pthread_barrier_init(barrier, NULL, count)))
        HGOTO_DONE(FAIL);
#else
    memcpy(barrier, &H5TS_barrier_def, sizeof(H5TS_barrier_def));

    /* Set non-default fields */
    barrier->count = barrier->threshold = count;
#endif

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__barrier_init() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__barrier_wait
 *
 * Purpose:     Wait at a barrier.
 *
 * Note:     	Similar to pthread_barrier_wait, a barrier may be re-used
 *		multiple times without intervening calls to H5TS_barrier_init.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__barrier_wait(H5TS_barrier_t *barrier)
{
#ifndef H5_HAVE_PTHREAD_BARRIER
    bool have_mutex = false;
#endif
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == barrier))
        HGOTO_DONE(FAIL);

#ifdef H5_HAVE_PTHREAD_BARRIER
    if (H5_UNLIKELY(pthread_barrier_wait(barrier)))
        HGOTO_DONE(FAIL);
#else
    /* Acquire the mutex for the barrier */
    if (H5_UNLIKELY(H5TS_mutex_lock(&barrier->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    barrier->entered++;
    if (barrier->entered < barrier->threshold) {
        if (H5_UNLIKELY(H5TS_cond_wait(&barrier->cv, &barrier->mutex) < 0))
            HGOTO_DONE(FAIL);
    }
    else {
        if (H5_UNLIKELY(H5TS_cond_broadcast(&barrier->cv) < 0))
            HGOTO_DONE(FAIL);

        /* Increment threshold count of threads for next barrier */
        barrier->threshold += barrier->count;
    }
#endif

done:
#ifndef H5_HAVE_PTHREAD_BARRIER
    if (H5_LIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&barrier->mutex) < 0))
            ret_value = FAIL;
#endif

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__barrier_wait() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__barrier_destroy
 *
 * Purpose:     Destroy an H5TS_barrier_t.  All internal components are
 *              destroyed, but the instance of H5TS_barrier_t is not freed.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__barrier_destroy(H5TS_barrier_t *barrier)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == barrier))
        HGOTO_DONE(FAIL);

#ifdef H5_HAVE_PTHREAD_BARRIER
    if (H5_UNLIKELY(pthread_barrier_destroy(barrier)))
        HGOTO_DONE(FAIL);
#else

    /* Acquire the mutex for the barrier */
    if (H5_UNLIKELY(H5TS_mutex_lock(&barrier->mutex) < 0))
        HGOTO_DONE(FAIL);

    /* Check for barrier still in use */
    if (H5_UNLIKELY((barrier->threshold - barrier->entered) != barrier->count)) {
        (void)H5TS_mutex_unlock(&barrier->mutex);
        HGOTO_DONE(FAIL);
    }

    /* Release the barrier's mutex */
    if (H5_UNLIKELY(H5TS_mutex_unlock(&barrier->mutex) < 0))
        HGOTO_DONE(FAIL);

    /* Call the appropriate pthread destroy routines.  We are committed
     * to the destroy at this point, so call them all, even if one fails
     * along the way.
     */
    if (H5_UNLIKELY(H5TS_mutex_destroy(&barrier->mutex) < 0))
        ret_value = FAIL;
    if (H5_UNLIKELY(H5TS_cond_destroy(&barrier->cv) < 0))
        ret_value = FAIL;
#endif

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__barrier_destroy() */

#endif /* H5_HAVE_THREADS */
