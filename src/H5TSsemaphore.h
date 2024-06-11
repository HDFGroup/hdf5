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

/***********/
/* Headers */
/***********/

/****************/
/* Local Macros */
/****************/
#if defined(H5_HAVE_THREADS) && defined(H5_HAVE_STDATOMIC_H)

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

#define H5TS__semaphore_signal(sem) (H5_UNLIKELY(0 == ReleaseSemaphore(sem->sem, 1, NULL)) ? FAIL : SUCCEED)
#define H5TS__semaphore_wait(sem)                                                                            \
    (H5_UNLIKELY(WAIT_OBJECT_0 != WaitForSingleObject(sem->sem, INFINITE)) ? FAIL : SUCCEED)

#elif defined(__MACH__)
/*
 * Mach semaphores, for MacOS
 * Can't use POSIX semaphores due to http://lists.apple.com/archives/darwin-kernel/2009/Apr/msg00010.html
 */

#define H5TS__semaphore_signal(sem) (H5_UNLIKELY(KERN_SUCCESS != semaphore_signal(sem->sem)) ? FAIL : SUCCEED)
#define H5TS__semaphore_wait(sem)   (H5_UNLIKELY(KERN_SUCCESS != semaphore_wait(sem->sem)) ? FAIL : SUCCEED)

#elif defined(__unix__)
/*
 * POSIX semaphores, everywhere else
 */

#define H5TS__semaphore_signal(mutex) ((H5_UNLIKELY(0 != sem_post(&sem->sem))) ? FAIL : SUCCEED)

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_wait
 *
 * Purpose:  Decrements (locks) the semaphore.  If the semaphore's value is
 *           greater than zero, then the decrement proceeds, and the function
 *           returns immediately.  If the semaphore currently has a value of
 *           zero or less, then the call blocks until it becomes possible to
 *           perform the decrement (i.e. the semaphore value rises above zero).
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static inline herr_t
H5TS__semaphore_wait(H5TS_semaphore_t *sem)
{
    int rc;

    /* Loop because of:
     *  http://stackoverflow.com/questions/2013181/gdb-causes-sem-wait-to-fail-with-eintr-error
     */
    do {
        rc = sem_wait(&sem->sem);
    } while (rc == -1 && errno == EINTR);

    if (H5_UNLIKELY(0 != rc))
        return FAIL;

    return SUCCEED;
} /* end H5TS__semaphore_wait() */
#else
#error "Unsupported platform!"
#endif

/*-------------------------------------------------------------------------
 * Function: H5TS__semaphore_wait_with_partial_spinning
 *
 * Purpose:  Spin on the lightweight semaphore's atomic counter for a little
 *           while before invoking the system semaphore.
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static inline herr_t
H5TS__semaphore_wait_with_partial_spinning(H5TS_semaphore_t *sem)
{
    int spin = 10000;
    int old_count;

    /* From original source: */
    /* Is there a better way to set the initial spin count?
     * If we lower it to 1000, testBenaphore becomes 15x slower on my Core i7-5930K Windows PC,
     * as threads start hitting the kernel semaphore.
     */
    while (spin--) {
        old_count = atomic_load_explicit(&sem->count, memory_order_relaxed);
        if ((old_count > 0) &&
            atomic_compare_exchange_strong_explicit(&sem->count, &old_count, old_count - 1,
                                                    memory_order_acquire, memory_order_acquire))
            return SUCCEED;

        /* Prevent the compiler from collapsing the loop. */
        atomic_signal_fence(memory_order_acquire);
    }
    old_count = atomic_fetch_sub_explicit(&sem->count, 1, memory_order_acquire);
    if (old_count <= 0)
        if (H5_UNLIKELY(H5TS__semaphore_wait(sem) < 0))
            return FAIL;

    return SUCCEED;
} /* end H5TS__semaphore_wait_with_partial_spinning() */

/*-------------------------------------------------------------------------
 * Function: H5TS_semaphore_signal
 *
 * Purpose:  Increments (unlocks) the semaphore.  If the semaphore's value
 *           becomes greater than zero, then another thread blocked in a wait
 *           call will be woken up and proceed to lock the semaphore.
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static inline herr_t
H5TS_semaphore_signal(H5TS_semaphore_t *sem)
{
    int old_count;
    int to_release;

    /* Check argument */
    if (H5_UNLIKELY(NULL == sem))
        return FAIL;

    old_count  = atomic_fetch_add_explicit(&sem->count, 1, memory_order_release);
    to_release = -old_count < 1 ? -old_count : 1;
    if (to_release > 0)
        if (H5_UNLIKELY(H5TS__semaphore_signal(sem) < 0))
            return FAIL;

    return SUCCEED;
} /* end H5TS_semaphore_signal() */

/*-------------------------------------------------------------------------
 * Function: H5TS_semaphore_wait
 *
 * Purpose:  Decrements (locks) the semaphore.  If the semaphore's value is
 *           greater than zero, then the decrement proceeds, and the function
 *           returns immediately.  If the semaphore currently has a value of
 *           zero or less, then the call blocks until it becomes possible to
 *           perform the decrement (i.e. the semaphore value rises above zero).
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static inline herr_t
H5TS_semaphore_wait(H5TS_semaphore_t *sem)
{
    int old_count;

    /* Check argument */
    if (H5_UNLIKELY(NULL == sem))
        return FAIL;

    /* Check atomic first, then try spinning (which will evetually call
     * system semaphore's 'wait').
     */
    old_count = atomic_load_explicit(&sem->count, memory_order_relaxed);
    if (!(old_count > 0 &&
          atomic_compare_exchange_strong_explicit(&sem->count, &old_count, old_count - 1,
                                                  memory_order_acquire, memory_order_acquire)))
        if (H5_UNLIKELY(H5TS__semaphore_wait_with_partial_spinning(sem) < 0))
            return FAIL;

    return SUCCEED;
} /* end H5TS_semaphore_wait() */

#endif /* H5_HAVE_THREADS && H5_HAVE_STDATOMIC_H */
