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
 * Purpose: This file contains support for semaphores, equivalent to POSIX
 *        semaphore's capabilities.  The implementation is based on the
 *	  "lightweight semaphore" describend here:
 *https://preshing.com/20150316/semaphores-are-surprisingly-versatile/ and implemented here:
 *https://github.com/preshing/cpp11-on-multicore/blob/master/common/sema.h
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

#if defined(H5_HAVE_THREADS) && defined(H5_HAVE_STDATOMIC_H)

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

#if defined(_WIN32)
/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_init
 *
 * Purpose:  Initialize a H5TS_semaphore_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5TS__semaphore_init(H5TS_semaphore_t *sem, int initial_count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(NULL == (sem->sem = CreateSemaphore(NULL, initial_count, LONG_MAX, NULL))))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__semaphore_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_destroy
 *
 * Purpose:  Destroy a H5TS_semaphore_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5TS__semaphore_destroy(H5TS_semaphore_t *sem)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(0 == CloseHandle(sem->sem)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__semaphore_destroy() */

#elif defined(__MACH__)
/*
 * Mach semaphores, for MacOS
 * Can't use POSIX semaphores due to http://lists.apple.com/archives/darwin-kernel/2009/Apr/msg00010.html
 */

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_init
 *
 * Purpose:  Initialize a H5TS_semaphore_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5TS__semaphore_init(H5TS_semaphore_t *sem, int initial_count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(KERN_SUCCESS !=
                    semaphore_create(mach_task_self(), &sem->sem, SYNC_POLICY_FIFO, initial_count)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__semaphore_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_destroy
 *
 * Purpose:  Destroy a H5TS_semaphore_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5TS__semaphore_destroy(H5TS_semaphore_t *sem)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(KERN_SUCCESS != semaphore_destroy(mach_task_self(), sem->sem)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__semaphore_destroy() */

#elif defined(__unix__)
/*
 * POSIX semaphores
 */

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_init
 *
 * Purpose:  Initialize a H5TS_semaphore_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5TS__semaphore_init(H5TS_semaphore_t *sem, int initial_count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(0 != sem_init(&sem->sem, 0, initial_count)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__semaphore_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_destroy
 *
 * Purpose:  Destroy a H5TS_semaphore_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5TS__semaphore_destroy(H5TS_semaphore_t *sem)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(0 != sem_destroy(&sem->sem)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__semaphore_destroy() */
#else
/*
 * Emulate semaphore w/mutex & condition variable
 *
 * Based off the Netscape Portable Runtime implementation:
 *      https://hg.mozilla.org/projects/nspr/file/3e25d69ba6b268f2817e920a69eb2c091efe17e6/pr/src/threads/prsem.c
 */

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_init
 *
 * Purpose:  Initialize a H5TS_semaphore_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5TS__semaphore_init(H5TS_semaphore_t *sem, int initial_count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(H5TS_mutex_init(&sem->sem.mutex, H5TS_MUTEX_TYPE_PLAIN) < 0))
        HGOTO_DONE(FAIL);
    if (H5_UNLIKELY(H5TS_cond_init(&sem->sem.cond) < 0))
        HGOTO_DONE(FAIL);
    sem->sem.waiters = 0;
    sem->sem.counter = initial_count;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__semaphore_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_destroy
 *
 * Purpose:  Destroy a H5TS_semaphore_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5TS__semaphore_destroy(H5TS_semaphore_t *sem)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    if (H5_UNLIKELY(H5TS_mutex_destroy(&sem->sem.mutex) < 0))
        HGOTO_DONE(FAIL);
    if (H5_UNLIKELY(H5TS_cond_destroy(&sem->sem.cond) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__semaphore_destroy() */
#endif

/*-------------------------------------------------------------------------
 * Function: H5TS_semaphore_init
 *
 * Purpose:  Initialize a H5TS_semaphore_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_semaphore_init(H5TS_semaphore_t *sem, int initial_count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check argument */
    if (H5_UNLIKELY(NULL == sem))
        HGOTO_DONE(FAIL);

    atomic_init(&sem->count, initial_count);
    if (H5_UNLIKELY(H5TS__semaphore_init(sem, initial_count) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_semaphore_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_semaphore_destroy
 *
 * Purpose:  Destroy a H5TS_semaphore_t (does not free it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_semaphore_destroy(H5TS_semaphore_t *sem)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check argument */
    if (H5_UNLIKELY(NULL == sem))
        HGOTO_DONE(FAIL);

    if (H5_UNLIKELY(H5TS__semaphore_destroy(sem) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_semaphore_destroy() */

#endif /* H5_HAVE_THREADS && H5_HAVE_STDATOMIC_H */
