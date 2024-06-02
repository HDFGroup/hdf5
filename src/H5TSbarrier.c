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

/*--------------------------------------------------------------------------
 * Function:    H5TS_barrier_init
 *
 * Purpose:     Initialize a thread barrier
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_barrier_init(H5TS_barrier_t *barrier, unsigned count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == barrier || 0 == count))
        HGOTO_DONE(FAIL);

fprintf(stderr, "%s:%u\n", __func__, __LINE__);
#ifdef H5_HAVE_PTHREAD_BARRIER
    /* Initialize the barrier */
    if (H5_UNLIKELY(pthread_barrier_init(barrier, NULL, count)))
        HGOTO_DONE(FAIL);
#else
    /* Initialize fields */
    barrier->count = count;
fprintf(stderr, "%s:%u\n", __func__, __LINE__);
    H5TS_atomic_init_uint(&barrier->openings, count);
fprintf(stderr, "%s:%u\n", __func__, __LINE__);
    H5TS_atomic_init_uint(&barrier->generation, 0);
fprintf(stderr, "%s:%u\n", __func__, __LINE__);
#endif

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_barrier_init() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_barrier_wait
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
H5TS_barrier_wait(H5TS_barrier_t *barrier)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == barrier))
        HGOTO_DONE(FAIL);

#ifdef H5_HAVE_PTHREAD_BARRIER
    {
        int ret = pthread_barrier_wait(barrier);
        if (H5_UNLIKELY(ret != 0 && ret != PTHREAD_BARRIER_SERIAL_THREAD))
            HGOTO_DONE(FAIL);
    }
#else
    {
        const unsigned my_generation = H5TS_atomic_load_uint(&barrier->generation);

        /* When the last thread enters, reset the openings & bump the generation */
        if (1 == H5TS_atomic_fetch_sub_uint(&barrier->openings, 1)) {
            H5TS_atomic_store_uint(&barrier->openings, barrier->count);
            H5TS_atomic_fetch_add_uint(&barrier->generation, 1);
        }
        else {
            /* Not the last thread, when for the generation to change */
            while (H5TS_atomic_load_uint(&barrier->generation) == my_generation)
                ;
        }
    }
#endif

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_barrier_wait() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_barrier_destroy
 *
 * Purpose:     Destroy an H5TS_barrier_t.  All internal components are
 *              destroyed, but the instance of H5TS_barrier_t is not freed.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_barrier_destroy(H5TS_barrier_t *barrier)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == barrier))
        HGOTO_DONE(FAIL);

#ifdef H5_HAVE_PTHREAD_BARRIER
    if (H5_UNLIKELY(pthread_barrier_destroy(barrier)))
        HGOTO_DONE(FAIL);
#else
    /* Destroy the (emulated) atomic variables */
    H5TS_atomic_destroy_uint(&barrier->openings);
    H5TS_atomic_destroy_uint(&barrier->generation);
#endif

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_barrier_destroy() */

#endif /* H5_HAVE_THREADS */
