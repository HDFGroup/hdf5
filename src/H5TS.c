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

/* private headers */
#include "H5private.h"    /*library                     */
#include "H5Eprivate.h"    /*error handling              */
#include "H5MMprivate.h"  /*memory management functions    */

#ifdef H5_HAVE_THREADSAFE

/* Module specific data structures */

/* cancelability structure */
typedef struct H5TS_cancel_struct {
    int previous_state;
    unsigned int cancel_count;
} H5TS_cancel_t;

/* Global variable definitions */
#ifdef H5_HAVE_WIN_THREADS
H5TS_once_t H5TS_first_init_g;
#else /* H5_HAVE_WIN_THREADS */
H5TS_once_t H5TS_first_init_g = PTHREAD_ONCE_INIT;
#endif /* H5_HAVE_WIN_THREADS */
H5TS_key_t H5TS_errstk_key_g;
H5TS_key_t H5TS_funcstk_key_g;
H5TS_key_t H5TS_apictx_key_g;
H5TS_key_t H5TS_cancel_key_g;


/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_key_destructor
 *
 * USAGE
 *    H5TS_key_destructor()
 *
 * RETURNS
 *
 * DESCRIPTION
 *   Frees the memory for a key.  Called by each thread as it exits.
 *   Currently all the thread-specific information for all keys are simple
 *   structures allocated with malloc, so we can free them all uniformly.
 *
 * PROGRAMMER: Quincey Koziol
 *             February 7, 2003
 *
 *--------------------------------------------------------------------------
 */
static void
H5TS_key_destructor(void *key_val)
{
    /* Use HDfree here instead of H5MM_xfree(), to avoid calling the H5CS routines */
    if(key_val != NULL)
        HDfree(key_val);
}


#ifndef H5_HAVE_WIN_THREADS

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_pthread_first_thread_init
 *
 * USAGE
 *    H5TS_pthread_first_thread_init()
 *
 * RETURNS
 *
 * DESCRIPTION
 *   Initialization of global API lock, keys for per-thread error stacks and
 *   cancallability information. Called by the first thread that enters the
 *   library.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_pthread_first_thread_init(void)
{
    H5_g.H5_libinit_g = FALSE;  /* Library hasn't been initialized */
    H5_g.H5_libterm_g = FALSE;  /* Library isn't being shutdown */

#ifdef H5_HAVE_WIN32_API
# ifdef PTW32_STATIC_LIB
    pthread_win32_process_attach_np();
# endif
#endif

    /* initialize global API mutex lock */
    HDpthread_mutex_init(&H5_g.init_lock.atomic_lock, NULL);
    HDpthread_cond_init(&H5_g.init_lock.cond_var, NULL);
    H5_g.init_lock.lock_count = 0;

    HDpthread_mutex_init(&H5_g.init_lock.atomic_lock2, NULL);
    H5_g.init_lock.attempt_lock_count = 0;

    /* initialize key for thread-specific error stacks */
    HDpthread_key_create(&H5TS_errstk_key_g, H5TS_key_destructor);

    /* initialize key for thread-specific function stacks */
    HDpthread_key_create(&H5TS_funcstk_key_g, H5TS_key_destructor);

    /* initialize key for thread-specific API contexts */
    HDpthread_key_create(&H5TS_apictx_key_g, H5TS_key_destructor);

    /* initialize key for thread cancellability mechanism */
    HDpthread_key_create(&H5TS_cancel_key_g, H5TS_key_destructor);
}
#endif /* H5_HAVE_WIN_THREADS */


/*--------------------------------------------------------------------------
 * Function:    H5TS__mutex_acquire
 *
 * Purpose:     Attempts to acquire a mutex lock, without blocking
 *
 * Note:	On success, the 'acquired' flag indicates if the HDF5 library
 *		global lock was acquired.
 *
 * Note:	The Windows threads code is very likely bogus.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Februrary 27, 2019
 *
 *--------------------------------------------------------------------------
 */
static herr_t
H5TS__mutex_acquire(H5TS_mutex_t *mutex, hbool_t *acquired)
{
#ifdef  H5_HAVE_WIN_THREADS
    EnterCriticalSection(&mutex->CriticalSection); 
    *acquired = TRUE;
    return 0;
#else /* H5_HAVE_WIN_THREADS */
    pthread_t my_thread_id = HDpthread_self();
    herr_t ret_value = 0;

    /* Attempt to acquire the mutex lock */
    if(0 == HDpthread_mutex_lock(&mutex->atomic_lock)) {
        /* Check if locked already */
        if(mutex->lock_count) {
             /* Check for this thread already owning the lock */
             if(HDpthread_equal(my_thread_id, mutex->owner_thread)) {
                /* Already owned by self - increment count */
                mutex->lock_count++;
                *acquired = TRUE;
            } /* end if */
            else
                *acquired = FALSE;
        } /* end if */
        else {
            /* Take ownership of the mutex */
            mutex->owner_thread = my_thread_id;
            mutex->lock_count = 1;
            *acquired = TRUE;
        } /* end else */

        if(0 != HDpthread_mutex_unlock(&mutex->atomic_lock))
            ret_value = -1;
    } /* end if */
    else
        ret_value = -1;

    return ret_value;
#endif /* H5_HAVE_WIN_THREADS */
} /* end H5TS__mutex_acquire() */


/*--------------------------------------------------------------------------
 * Function:    H5TSmutex_acquire
 *
 * Purpose:     Attempts to acquire the HDF5 library global lock
 *
 * Note:	On success, the 'acquired' flag indicates if the HDF5 library
 *		global lock was acquired.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Februrary 27, 2019
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TSmutex_acquire(hbool_t *acquired)
{
    return(H5TS__mutex_acquire(&H5_g.init_lock, acquired));
} /* end H5TSmutex_acquire() */


/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_mutex_lock
 *
 * USAGE
 *    H5TS_mutex_lock(&mutex_var)
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Recursive lock semantics for HDF5 (locking) -
 *    Multiple acquisition of a lock by a thread is permitted with a
 *    corresponding unlock operation required.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_lock(H5TS_mutex_t *mutex)
{
#ifdef  H5_HAVE_WIN_THREADS
    EnterCriticalSection(&mutex->CriticalSection); 
    return 0;
#else /* H5_HAVE_WIN_THREADS */
    herr_t ret_value;
    struct timeval now_time;

    /* Acquire the lock, increment the attempt lock count, release the lock */
    ret_value = HDpthread_mutex_lock(&mutex->atomic_lock2);
    if (ret_value)
        return ret_value;
    mutex->attempt_lock_count++;
    mutex->attempt_thread = HDpthread_self();
    ret_value = HDpthread_mutex_unlock(&mutex->atomic_lock2);
    if (ret_value)
        return ret_value;

    ret_value = HDpthread_mutex_lock(&mutex->atomic_lock);
    if (ret_value)
        return ret_value;

    if(mutex->lock_count && HDpthread_equal(HDpthread_self(), mutex->owner_thread)) {
        /* already owned by self - increment count */
        mutex->lock_count++;
    } else {
        /* if owned by other thread, wait for condition signal */
        /* gettimeofday(&now_time, NULL); */
        /* HDfprintf(stdout, "%-20s lock count = %u, thread id = %u, time = %ld.%06ld\n", __func__, mutex->lock_count, pthread_self(), now_time.tv_sec, now_time.tv_usec, ret_value); */
        while(mutex->lock_count)
            HDpthread_cond_wait(&mutex->cond_var, &mutex->atomic_lock);

        /* After we've received the signal, take ownership of the mutex */
        mutex->owner_thread = HDpthread_self();
        mutex->lock_count = 1;
    }

    return HDpthread_mutex_unlock(&mutex->atomic_lock); 
#endif /* H5_HAVE_WIN_THREADS */
}


/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_mutex_unlock
 *
 * USAGE
 *    H5TS_mutex_unlock(&mutex_var)
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Recursive lock semantics for HDF5 (unlocking) -
 *    Multiple acquisition of a lock by a thread is permitted with a
 *    corresponding unlock operation required.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_mutex_unlock(H5TS_mutex_t *mutex)
{
#ifdef  H5_HAVE_WIN_THREADS
    /* Releases ownership of the specified critical section object. */
    LeaveCriticalSection(&mutex->CriticalSection);
    return 0; 
#else  /* H5_HAVE_WIN_THREADS */
    herr_t ret_value = HDpthread_mutex_lock(&mutex->atomic_lock);
    struct timeval now_time;

    if(ret_value)
        return ret_value;

    mutex->lock_count--;
    /* gettimeofday(&now_time, NULL); */
    /* HDfprintf(stdout, "%-20s lock count = %u, thread id = %u, time = %ld.%06ld\n", __func__, mutex->lock_count, pthread_self(), now_time.tv_sec, now_time.tv_usec, ret_value); */

    ret_value = HDpthread_mutex_unlock(&mutex->atomic_lock);

    if(mutex->lock_count == 0) {
        int err;

        err = HDpthread_cond_signal(&mutex->cond_var);
        if(err != 0)
            ret_value = err;
    } /* end if */

    ret_value = HDpthread_mutex_lock(&mutex->atomic_lock2);
    if (ret_value)
        return ret_value;
    /* mutex->attempt_lock_count--; */
    mutex->attempt_thread = 0;
    ret_value = HDpthread_mutex_unlock(&mutex->atomic_lock2);
    if (ret_value)
        return ret_value;

    return ret_value; 
#endif /* H5_HAVE_WIN_THREADS */
} /* H5TS_mutex_unlock */


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
H5TSmutex_get_attempt_count(unsigned int *count)
{
    herr_t ret_value = 0;

    ret_value = HDpthread_mutex_lock(&H5_g.init_lock.atomic_lock2);
    if (ret_value)
        return ret_value;

    *count = H5_g.init_lock.attempt_lock_count;

    ret_value = HDpthread_mutex_unlock(&H5_g.init_lock.atomic_lock2);
    if (ret_value)
        return ret_value;

    return(ret_value);
} /* end H5TSmutex_get_attempt_count() */




/*--------------------------------------------------------------------------
 * Function:    H5TSmutex_get_attempt_thread
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
H5TSmutex_get_attempt_thread(pthread_t *attemp_thread)
{
    herr_t ret_value = 0;

    ret_value = HDpthread_mutex_lock(&H5_g.init_lock.atomic_lock2);
    if (ret_value)
        return ret_value;

    *attemp_thread = H5_g.init_lock.attempt_thread;

    ret_value = HDpthread_mutex_unlock(&H5_g.init_lock.atomic_lock2);
    if (ret_value)
        return ret_value;

    return(ret_value);
} /* end H5TSmutex_get_attempt_thread() */



/*--------------------------------------------------------------------------
 * Function:    H5TSmutex_release
 *
 * Purpose:     Releases the HDF5 library global lock
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Februrary 27, 2019
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TSmutex_release(void)
{
    herr_t ret_value = 0;

    if(0 != H5TS_mutex_unlock(&H5_g.init_lock))
        ret_value = -1;

    return(ret_value);
} /* end H5TSmutex_release() */


/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_cancel_count_inc
 *
 * USAGE
 *    H5TS_cancel_count_inc()
 *
 * RETURNS
 *    0 on success non-zero error code on error.
 *
 * DESCRIPTION
 *    Creates a cancelation counter for a thread if it is the first time
 *    the thread is entering the library.
 *
 *    if counter value is zero, then set cancelability type of the thread
 *    to PTHREAD_CANCEL_DISABLE as thread is entering the library and store
 *    the previous cancelability type into cancelation counter.
 *    Increase the counter value by 1.
 *
 * PROGRAMMER: Chee Wai LEE
 *            May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_cancel_count_inc(void)
{
#ifdef  H5_HAVE_WIN_THREADS
    /* unsupported; just return 0 */
    return SUCCEED;
#else /* H5_HAVE_WIN_THREADS */
    H5TS_cancel_t *cancel_counter;
    herr_t ret_value = SUCCEED; 

    cancel_counter = (H5TS_cancel_t *)H5TS_get_thread_local_value(H5TS_cancel_key_g); 

    if (!cancel_counter) {
        /*
         * First time thread calls library - create new counter and associate
         * with key.
         * 
         * Don't use H5MM calls here since the destructor has to use HDfree in
         * order to avoid codestack calls.
         */
        cancel_counter = (H5TS_cancel_t *)HDcalloc(1, sizeof(H5TS_cancel_t));

        if (!cancel_counter) {
            HERROR(H5E_RESOURCE, H5E_NOSPACE, "memory allocation failed");
            return FAIL;
        }

        ret_value = HDpthread_setspecific(H5TS_cancel_key_g, (void *)cancel_counter);
    }

    if (cancel_counter->cancel_count == 0)
        /* thread entering library */
        ret_value = HDpthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &cancel_counter->previous_state);

    ++cancel_counter->cancel_count;

    return ret_value; 
#endif /* H5_HAVE_WIN_THREADS */
}


/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_cancel_count_dec
 *
 * USAGE
 *    H5TS_cancel_count_dec()
 *
 * RETURNS
 *    0 on success and a non-zero error code on error.
 *
 * DESCRIPTION
 *    If counter value is one, then set cancelability type of the thread
 *    to the previous cancelability type stored in the cancelation counter.
 *    (the thread is leaving the library).
 *
 *    Decrement the counter value by 1.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_cancel_count_dec(void)
{
#ifdef  H5_HAVE_WIN_THREADS 
    /* unsupported; will just return 0 */
    return SUCCEED;
#else /* H5_HAVE_WIN_THREADS */
    register H5TS_cancel_t *cancel_counter; 
    herr_t ret_value = SUCCEED;

    cancel_counter = (H5TS_cancel_t *)H5TS_get_thread_local_value(H5TS_cancel_key_g); 

    if (cancel_counter->cancel_count == 1)
        ret_value = HDpthread_setcancelstate(cancel_counter->previous_state, NULL);

    --cancel_counter->cancel_count; 

    return ret_value;
#endif /* H5_HAVE_WIN_THREADS */
}


#ifdef H5_HAVE_WIN_THREADS
/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_win32_process_enter
 *
 * RETURNS
 *    SUCCEED/FAIL
 *
 * DESCRIPTION
 *    Per-process setup on Windows when using Win32 threads.
 *
 *--------------------------------------------------------------------------
 */
H5_DLL BOOL CALLBACK 
H5TS_win32_process_enter(PINIT_ONCE InitOnce, PVOID Parameter, PVOID *lpContex)
{
    BOOL ret_value = TRUE;

    /* Initialize the critical section (can't fail) */
    InitializeCriticalSection(&H5_g.init_lock.CriticalSection);

    /* Set up thread local storage */
    if(TLS_OUT_OF_INDEXES == (H5TS_errstk_key_g = TlsAlloc()))
        ret_value = FALSE;

#ifdef H5_HAVE_CODESTACK
    if(TLS_OUT_OF_INDEXES == (H5TS_funcstk_key_g = TlsAlloc()))
        ret_value = FALSE;
#endif /* H5_HAVE_CODESTACK */

    if(TLS_OUT_OF_INDEXES == (H5TS_apictx_key_g = TlsAlloc()))
        ret_value = FALSE;

    return ret_value;
} /* H5TS_win32_process_enter() */
#endif /* H5_HAVE_WIN_THREADS */


#ifdef H5_HAVE_WIN_THREADS
/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_win32_thread_enter
 *
 * RETURNS
 *    SUCCEED/FAIL
 *
 * DESCRIPTION
 *    Per-thread setup on Windows when using Win32 threads.
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_win32_thread_enter(void)
{
    herr_t ret_value = SUCCEED;

    /* Currently a placeholder function.  TLS setup is performed
     * elsewhere in the library.
     *
     * WARNING: Do NOT use C standard library functions here.
     * CRT functions are not allowed in DllMain, which is where this code
     * is used.
     */

    return ret_value;
} /* H5TS_win32_thread_enter() */
#endif /* H5_HAVE_WIN_THREADS */


#ifdef H5_HAVE_WIN_THREADS
/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_win32_process_exit
 *
 * RETURNS
 *    SUCCEED/FAIL
 *
 * DESCRIPTION
 *    Per-process cleanup on Windows when using Win32 threads.
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_win32_process_exit(void)
{

    /* Windows uses a different thread local storage mechanism which does
     * not support auto-freeing like pthreads' keys.
     *
     * This function is currently registered via atexit() and is called
     * AFTER H5_term_library().
     */

    /* Clean up critical section resources (can't fail) */
    DeleteCriticalSection(&H5_g.init_lock.CriticalSection);

    /* Clean up per-process thread local storage */
    TlsFree(H5TS_errstk_key_g);
#ifdef H5_HAVE_CODESTACK
    TlsFree(H5TS_funcstk_key_g);
#endif /* H5_HAVE_CODESTACK */
    TlsFree(H5TS_apictx_key_g);

    return;
} /* H5TS_win32_process_exit() */
#endif /* H5_HAVE_WIN_THREADS */


#ifdef H5_HAVE_WIN_THREADS
/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_win32_thread_exit
 *
 * RETURNS
 *    SUCCEED/FAIL
 *
 * DESCRIPTION
 *    Per-thread cleanup on Windows when using Win32 threads.
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_win32_thread_exit(void)
{
    LPVOID lpvData;
    herr_t ret_value = SUCCEED;

    /* Windows uses a different thread local storage mechanism which does
     * not support auto-freeing like pthreads' keys.
     *
     * WARNING: Do NOT use C standard library functions here.
     * CRT functions are not allowed in DllMain, which is where this code
     * is used.
     */

    /* Clean up per-thread thread local storage */
    lpvData = TlsGetValue(H5TS_errstk_key_g);
    if(lpvData)
        LocalFree((HLOCAL)lpvData);

#ifdef H5_HAVE_CODESTACK
    lpvData = TlsGetValue(H5TS_funcstk_key_g);
    if(lpvData)
        LocalFree((HLOCAL)lpvData);
#endif /* H5_HAVE_CODESTACK */

    lpvData = TlsGetValue(H5TS_apictx_key_g);
    if(lpvData)
        LocalFree((HLOCAL)lpvData);

    return ret_value;
} /* H5TS_win32_thread_exit() */
#endif /* H5_HAVE_WIN_THREADS */


/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_create_thread
 *
 * RETURNS
 *    Thread identifier.
 *
 * DESCRIPTION
 *    Spawn off a new thread calling function 'func' with input 'udata'.
 *
 * PROGRAMMER: Mike McGreevy
 *             August 31, 2010
 *
 *--------------------------------------------------------------------------
 */
H5TS_thread_t
H5TS_create_thread(void *(*func)(void *), H5TS_attr_t *attr, void *udata)
{
    H5TS_thread_t ret_value;

#ifdef  H5_HAVE_WIN_THREADS 

    /* When calling C runtime functions, you should use _beginthread or
     * _beginthreadex instead of CreateThread.  Threads created with
     * CreateThread risk being killed in low-memory situations. Since we
     * only create threads in our test code, this is unlikely to be an issue
     * and we'll use the easier-to-deal-with CreateThread for now.
     *
     * NOTE: _beginthread() auto-recycles its handle when execution completes
     *       so you can't wait on it, making it unsuitable for the existing
     *       test code.
     */
    ret_value = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)func, udata, 0, NULL);

#else /* H5_HAVE_WIN_THREADS */

    HDpthread_create(&ret_value, attr, (void * (*)(void *))func, udata);

#endif /* H5_HAVE_WIN_THREADS */

    return ret_value;

} /* H5TS_create_thread */

#endif  /* H5_HAVE_THREADSAFE */

