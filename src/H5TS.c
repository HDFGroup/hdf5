/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Purpose: This file contains the framework for ensuring that the global
 *          library lock is held when an API routine is called.  This
 *          framework works in concert with the FUNC_ENTER_API / FUNC_LEAVE_API
 *          macros defined in H5private.h.
 *
 * Note:    Because this threadsafety framework operates outside the library,
 *          it does not use the error stack and only uses the "namecheck only"
 *          FUNC_ENTER_* / FUNC_LEAVE_* macros.
 */

/****************/
/* Module Setup */
/****************/

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5Eprivate.h"  /* Error handling                           */

#ifdef H5_HAVE_THREADSAFE

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/* Cancelability structure */
typedef struct H5TS_cancel_struct {
    int          previous_state;
    unsigned int cancel_count;
} H5TS_cancel_t;

/********************/
/* Local Prototypes */
/********************/
static void H5TS__key_destructor(void *key_val);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/* Global variable definitions */
#ifdef H5_HAVE_WIN_THREADS
H5TS_once_t H5TS_first_init_g;
#else
H5TS_once_t H5TS_first_init_g = PTHREAD_ONCE_INIT;
#endif

/* Thread-local keys, used by other interfaces */
H5TS_key_t H5TS_errstk_key_g; /* Error stack */
#ifdef H5_HAVE_CODESTACK
H5TS_key_t H5TS_funcstk_key_g; /* Function stack */
#endif                         /* H5_HAVE_CODESTACK */
H5TS_key_t H5TS_apictx_key_g;  /* API context */

/*******************/
/* Local Variables */
/*******************/

/* Thread-local keys, used in this module */
static H5TS_key_t H5TS_cancel_key_s; /* Thread cancellation state */

#ifndef H5_HAVE_WIN_THREADS

/* An H5TS_tid_t is a record of a thread identifier that is
 * available for reuse.
 */
struct _tid;
typedef struct _tid H5TS_tid_t;

struct _tid {
    H5TS_tid_t *next;
    uint64_t    id;
};

/* Pointer to first free thread ID record or NULL. */
static H5TS_tid_t *H5TS_tid_next_free = NULL;
static uint64_t    H5TS_tid_next_id   = 0;

/* Mutual exclusion for access to H5TS_tid_next_free and H5TS_tid_next_id. */
static pthread_mutex_t H5TS_tid_mtx;

/* Key for thread-local storage of the thread ID. */
static H5TS_key_t H5TS_tid_key;

#endif /* H5_HAVE_WIN_THREADS */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS__key_destructor
 *
 * USAGE
 *    H5TS__key_destructor()
 *
 * RETURNS
 *   None
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
H5TS__key_destructor(void *key_val)
{
    /* Use HDfree here instead of H5MM_xfree(), to avoid calling the H5CS routines */
    if (key_val != NULL)
        HDfree(key_val);
} /* end H5TS__key_destructor() */

#ifndef H5_HAVE_WIN_THREADS

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_tid_destructor
 *
 * USAGE
 *    H5TS_tid_destructor()
 *
 * RETURNS
 *
 * DESCRIPTION
 *   When a thread shuts down, put its ID record on the free list.
 *
 *--------------------------------------------------------------------------
 */
static void
H5TS_tid_destructor(void *_v)
{
    H5TS_tid_t *tid = _v;

    if (tid == NULL)
        return;

    /* TBD use an atomic CAS */
    pthread_mutex_lock(&H5TS_tid_mtx);
    tid->next          = H5TS_tid_next_free;
    H5TS_tid_next_free = tid;
    pthread_mutex_unlock(&H5TS_tid_mtx);
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_tid_init
 *
 * USAGE
 *    H5TS_tid_init()
 *
 * RETURNS
 *
 * DESCRIPTION
 *   Initialize for integer thread identifiers.
 *
 *--------------------------------------------------------------------------
 */
static void
H5TS_tid_init(void)
{
    pthread_mutex_init(&H5TS_tid_mtx, NULL);
    pthread_key_create(&H5TS_tid_key, H5TS_tid_destructor);
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_thread_id
 *
 * USAGE
 *    uint64_t id = H5TS_thread_id()
 *
 * RETURNS
 *   Return an integer identifier, ID, for the current thread.
 *
 * DESCRIPTION
 *   The ID satisfies the following properties:
 *
 *   1 1 <= ID <= UINT64_MAX
 *   2 ID is constant over the thread's lifetime.
 *   3 No two threads share an ID during their lifetimes.
 *   4 A thread's ID is available for reuse as soon as it is joined.
 *
 *   ID 0 is reserved.  H5TS_thread_id() returns 0 if the library was not
 *   built with thread safety or if an error prevents it from assigning an
 *   ID.
 *
 *--------------------------------------------------------------------------
 */
uint64_t
H5TS_thread_id(void)
{
    H5TS_tid_t *tid = pthread_getspecific(H5TS_tid_key);
    H5TS_tid_t  proto_tid;

    /* An ID is already assigned. */
    if (tid != NULL)
        return tid->id;

    /* An ID is *not* already assigned: reuse an ID that's on the
     * free list, or else generate a new ID.
     *
     * Allocating memory while holding a mutex is bad form, so
     * point `tid` at `proto_tid` if we need to allocate some
     * memory.
     */
    pthread_mutex_lock(&H5TS_tid_mtx);
    if ((tid = H5TS_tid_next_free) != NULL)
        H5TS_tid_next_free = tid->next;
    else if (H5TS_tid_next_id != UINT64_MAX) {
        tid     = &proto_tid;
        tid->id = ++H5TS_tid_next_id;
    }
    pthread_mutex_unlock(&H5TS_tid_mtx);

    /* If a prototype ID record was established, copy it to the heap. */
    if (tid == &proto_tid)
        if ((tid = HDmalloc(sizeof(*tid))) != NULL)
            *tid = proto_tid;

    if (tid == NULL)
        return 0;

    /* Finish initializing the ID record and set a thread-local pointer
     * to it.
     */
    tid->next = NULL;
    if (pthread_setspecific(H5TS_tid_key, tid) != 0) {
        H5TS_tid_destructor(tid);
        return 0;
    }

    return tid->id;
}

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
    H5_g.H5_libinit_g = FALSE; /* Library hasn't been initialized */
    H5_g.H5_libterm_g = FALSE; /* Library isn't being shutdown */

#ifdef H5_HAVE_WIN32_API
#ifdef PTW32_STATIC_LIB
    pthread_win32_process_attach_np();
#endif
#endif

    /* initialize global API mutex lock */
#ifdef H5_USE_RECURSIVE_WRITER_LOCKS
    H5TS_rw_lock_init(&H5_g.init_rw_lock, H5TS__RW_LOCK_POLICY__FAVOR_WRITERS);
#else
    pthread_mutex_init(&H5_g.init_lock.atomic_lock, NULL);
    pthread_cond_init(&H5_g.init_lock.cond_var, NULL);
    H5_g.init_lock.lock_count = 0;
#endif

    /* Initialize integer thread identifiers. */
    H5TS_tid_init();

    /* initialize key for thread-specific error stacks */
    pthread_key_create(&H5TS_errstk_key_g, H5TS__key_destructor);

#ifdef H5_HAVE_CODESTACK
    /* initialize key for thread-specific function stacks */
    pthread_key_create(&H5TS_funcstk_key_g, H5TS__key_destructor);
#endif

    /* initialize key for thread-specific API contexts */
    pthread_key_create(&H5TS_apictx_key_g, H5TS__key_destructor);

    /* initialize key for thread cancellability mechanism */
    pthread_key_create(&H5TS_cancel_key_s, H5TS__key_destructor);
} /* end H5TS_pthread_first_thread_init() */
#endif /* H5_HAVE_WIN_THREADS */

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
    herr_t ret_value = 0;

#ifdef H5_HAVE_WIN_THREADS
    EnterCriticalSection(&mutex->CriticalSection);
#else
    /* Acquire the library lock */
    ret_value = pthread_mutex_lock(&mutex->atomic_lock);
    if (ret_value)
        return ret_value;

    /* Check if this thread already owns the lock */
    if (mutex->lock_count && pthread_equal(pthread_self(), mutex->owner_thread))
        /* already owned by self - increment count */
        mutex->lock_count++;
    else {
        /* Wait until the lock is released by current owner thread */
        while (mutex->lock_count)
            pthread_cond_wait(&mutex->cond_var, &mutex->atomic_lock);

        /* After we've received the signal, take ownership of the mutex */
        mutex->owner_thread = pthread_self();
        mutex->lock_count   = 1;
    } /* end else */

    /* Release the library lock */
    ret_value = pthread_mutex_unlock(&mutex->atomic_lock);
#endif

    return ret_value;
} /* end H5TS_mutex_lock() */

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
    herr_t ret_value = 0;

#ifdef H5_HAVE_WIN_THREADS
    /* Releases ownership of the specified critical section object. */
    LeaveCriticalSection(&mutex->CriticalSection);
#else

    /* Decrement the lock count for this thread */
    ret_value = pthread_mutex_lock(&mutex->atomic_lock);
    if (ret_value)
        return ret_value;
    mutex->lock_count--;
    ret_value = pthread_mutex_unlock(&mutex->atomic_lock);

    /* If the lock count drops to zero, signal the condition variable, to
     * wake another thread.
     */
    if (mutex->lock_count == 0) {
        int err;

        err = pthread_cond_signal(&mutex->cond_var);
        if (err != 0)
            ret_value = err;
    }
#endif /* H5_HAVE_WIN_THREADS */

    return ret_value;
} /* H5TS_mutex_unlock */

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
 *    Creates a cancellation counter for a thread if it is the first time
 *    the thread is entering the library.
 *
 *    if counter value is zero, then set cancelability type of the thread
 *    to PTHREAD_CANCEL_DISABLE as thread is entering the library and store
 *    the previous cancelability type into cancellation counter.
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
#ifndef H5_HAVE_WIN_THREADS
    H5TS_cancel_t *cancel_counter;
#endif
    herr_t ret_value = SUCCEED;

#ifdef H5_HAVE_WIN_THREADS
    /* unsupported */
#else  /* H5_HAVE_WIN_THREADS */
    /* Acquire the thread's cancellation counter */
    cancel_counter = (H5TS_cancel_t *)H5TS_get_thread_local_value(H5TS_cancel_key_s);

    /* Check if it's created yet */
    if (!cancel_counter) {
        /*
         * First time thread calls library - create new counter and associate
         * with key.
         *
         * Don't use H5MM calls here since the destructor has to use HDfree in
         * order to avoid codestack calls.
         */
        cancel_counter = (H5TS_cancel_t *)HDcalloc(1, sizeof(H5TS_cancel_t));
        if (NULL == cancel_counter) {
            HERROR(H5E_RESOURCE, H5E_NOSPACE, "memory allocation failed");
            return FAIL;
        }

        /* Set the thread's cancellation counter with the new object */
        ret_value = pthread_setspecific(H5TS_cancel_key_s, (void *)cancel_counter);
        if (ret_value) {
            HDfree(cancel_counter);
            return FAIL;
        }
    }

    /* Check if thread entering library */
    if (cancel_counter->cancel_count == 0)
        /* Set cancellation state to 'disable', and remember previous state */
        ret_value = pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &cancel_counter->previous_state);

    /* Increment # of times the library API was re-entered, to avoid resetting
     * previous cancellation state until the final API routine is returning.
     */
    ++cancel_counter->cancel_count;
#endif /* H5_HAVE_WIN_THREADS */

    return ret_value;
} /* end H5TS_cancel_count_inc() */

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
 *    to the previous cancelability type stored in the cancellation counter.
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
#ifndef H5_HAVE_WIN_THREADS
    H5TS_cancel_t *cancel_counter;
#endif /* H5_HAVE_WIN_THREADS */
    herr_t ret_value = SUCCEED;

#ifdef H5_HAVE_WIN_THREADS
    /* unsupported */
#else  /* H5_HAVE_WIN_THREADS */
    /* Acquire the thread's cancellation counter */
    cancel_counter = (H5TS_cancel_t *)H5TS_get_thread_local_value(H5TS_cancel_key_s);

    /* Check for leaving last API routine */
    if (cancel_counter->cancel_count == 1)
        /* Reset to previous thread cancellation state, if last API */
        ret_value = pthread_setcancelstate(cancel_counter->previous_state, NULL);

    /* Decrement cancellation counter */
    --cancel_counter->cancel_count;
#endif /* H5_HAVE_WIN_THREADS */

    return ret_value;
} /* end H5TS_cancel_count_dec() */

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
    if (TLS_OUT_OF_INDEXES == (H5TS_errstk_key_g = TlsAlloc()))
        ret_value = FALSE;

#ifdef H5_HAVE_CODESTACK
    if (TLS_OUT_OF_INDEXES == (H5TS_funcstk_key_g = TlsAlloc()))
        ret_value = FALSE;
#endif

    if (TLS_OUT_OF_INDEXES == (H5TS_apictx_key_g = TlsAlloc()))
        ret_value = FALSE;

    return ret_value;
} /* H5TS_win32_process_enter() */

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
    if (lpvData)
        LocalFree((HLOCAL)lpvData);

#ifdef H5_HAVE_CODESTACK
    lpvData = TlsGetValue(H5TS_funcstk_key_g);
    if (lpvData)
        LocalFree((HLOCAL)lpvData);
#endif /* H5_HAVE_CODESTACK */

    lpvData = TlsGetValue(H5TS_apictx_key_g);
    if (lpvData)
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

#ifdef H5_HAVE_WIN_THREADS
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

    pthread_create(&ret_value, attr, (void *(*)(void *))func, udata);

#endif /* H5_HAVE_WIN_THREADS */

    return ret_value;
} /* H5TS_create_thread */

#ifdef H5_USE_RECURSIVE_WRITER_LOCKS

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_alloc_rec_entry_count
 *
 * RETURNS
 *    Pointer to allocated and initialized instance of
 *    H5TS_rec_entry_count, or NULL on failure.
 *
 * DESCRIPTION
 *    Allocate and initalize an instance of H5TS_rec_entry_count.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

H5TS_rec_entry_count *
H5TS_alloc_rec_entry_count(hbool_t write_lock)
{
    H5TS_rec_entry_count *ret_value = NULL;

    ret_value = (H5TS_rec_entry_count *)HDmalloc(sizeof(H5TS_rec_entry_count));

    if (ret_value) {

        ret_value->magic          = H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC;
        ret_value->write_lock     = write_lock;
        ret_value->rec_lock_count = 1;
    }

    return ret_value;

} /* H5TS_alloc_rec_entry_count() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_free_rec_entry_count
 *
 * RETURNS
 *    void.
 *
 * DESCRIPTION
 *    Free the supplied instance of H5TS_rec_entry_count.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

void
H5TS_free_rec_entry_count(void *target)
{
    H5TS_rec_entry_count *count;

    count = (H5TS_rec_entry_count *)target;

    HDassert(count);
    HDassert(count->magic == H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC);

    count->magic = 0;

    HDfree(count);

    return;

} /* H5TS_free_rec_entry_count() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_rw_lock_init
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Initialize the supplied instance of H5TS_rw_lock_t.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

herr_t
H5TS_rw_lock_init(H5TS_rw_lock_t *rw_lock, int policy)
{
    herr_t ret_value = SUCCEED;

    /* sanity checks -- until other policies are implemented,
     * policy must equal H5TS__RW_LOCK_POLICY__FAVOR_WRITERS.
     */
    if ((rw_lock == NULL) || (policy != H5TS__RW_LOCK_POLICY__FAVOR_WRITERS)) {

        ret_value = FAIL;
    }

    if (ret_value == SUCCEED) { /* initialized the mutex */

        if (pthread_mutex_init(&(rw_lock->mutex), NULL) != 0) {

            ret_value = FAIL;
        }
    }

    if (ret_value == SUCCEED) { /* initialize the waiting readers cv */

        if (pthread_cond_init(&(rw_lock->readers_cv), NULL) != 0) {

            ret_value = FAIL;
        }
    }

    if (ret_value == SUCCEED) { /* initialize the waiting writers cv */

        if (pthread_cond_init(&(rw_lock->writers_cv), NULL) != 0) {

            ret_value = FAIL;
        }
    }

    if (ret_value == SUCCEED) { /* initialize the key */

        if (pthread_key_create(&(rw_lock->rec_entry_count_key), H5TS_free_rec_entry_count) != 0) {

            ret_value = FAIL;
        }
    }

    if (ret_value == SUCCEED) { /* initialized scalar fields */

        rw_lock->magic                                = H5TS_RW_LOCK_MAGIC;
        rw_lock->policy                               = policy;
        rw_lock->waiting_readers_count                = 0;
        rw_lock->waiting_writers_count                = 0;
        rw_lock->active_readers                       = 0;
        rw_lock->active_writers                       = 0;
        rw_lock->stats.read_locks_granted             = 0;
        rw_lock->stats.read_locks_released            = 0;
        rw_lock->stats.real_read_locks_granted        = 0;
        rw_lock->stats.real_read_locks_released       = 0;
        rw_lock->stats.max_read_locks                 = 0;
        rw_lock->stats.max_read_lock_recursion_depth  = 0;
        rw_lock->stats.read_locks_delayed             = 0;
        rw_lock->stats.max_read_locks_pending         = 0;
        rw_lock->stats.write_locks_granted            = 0;
        rw_lock->stats.write_locks_released           = 0;
        rw_lock->stats.real_write_locks_granted       = 0;
        rw_lock->stats.real_write_locks_released      = 0;
        rw_lock->stats.max_write_locks                = 0;
        rw_lock->stats.max_write_lock_recursion_depth = 0;
        rw_lock->stats.write_locks_delayed            = 0;
        rw_lock->stats.max_write_locks_pending        = 0;
    }

    return ret_value;

} /* H5TS_rw_lock_init() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_rw_lock_takedown
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Takedown an instance of H5TS_rw_lock_t.  All mutex, condition
 *    variables, and keys are destroyed, and magic is set to an invalid
 *    value.  However, the instance of H5TS_rw_lock_t is not
 *    freed.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

herr_t
H5TS_rw_lock_takedown(H5TS_rw_lock_t *rw_lock)
{
    herr_t ret_value = SUCCEED;

    if ((rw_lock == NULL) || (rw_lock->magic != H5TS_RW_LOCK_MAGIC)) {

        ret_value = FAIL;
    }
    else {

        /* we are commited to the takedown at this point.  Set magic
         * to an invalid value, and call the appropriate pthread
         * destroy routines.  Call them all, even if one fails along
         * the way.
         */
        rw_lock->magic = 0;

        if ((pthread_mutex_destroy(&(rw_lock->mutex)) < 0) ||
            (pthread_cond_destroy(&(rw_lock->readers_cv)) < 0) ||
            (pthread_cond_destroy(&(rw_lock->writers_cv)) < 0) ||
            (pthread_key_delete(rw_lock->rec_entry_count_key) < 0)) {

            ret_value = FAIL;
        }
    }

    return ret_value;

} /* H5TS_rw_lock_takedown() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_rw_rdlock
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Attempt to obtain a read lock on the associated recursive read / write
 *    lock.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

herr_t
H5TS_rw_rdlock(H5TS_rw_lock_t *rw_lock)
{
    hbool_t               have_mutex = FALSE;
    int                   result;
    H5TS_rec_entry_count *count;
    herr_t                ret_value = SUCCEED;

    if ((rw_lock == NULL) || (rw_lock->magic != H5TS_RW_LOCK_MAGIC)) {

        ret_value = FAIL;
    }

    /* obtain the mutex */
    if (ret_value == SUCCEED) {

        if (pthread_mutex_lock(&(rw_lock->mutex)) != 0) {

            ret_value = FAIL;
        }
        else {

            have_mutex = TRUE;
        }
    }

    /* If there is no specific data for this thread, this is an
     * initial read lock request.
     */
    if (ret_value == SUCCEED) {

        count = (H5TS_rec_entry_count *)pthread_getspecific(rw_lock->rec_entry_count_key);

        if (count) { /* this is a recursive lock */

            if ((count->write_lock) || (rw_lock->active_readers == 0) || (rw_lock->active_writers != 0)) {

                ret_value = FAIL;
            }
            else {

                count->rec_lock_count++;

                REC_RW_LOCK_STATS__UPDATE_FOR_RD_LOCK(rw_lock, count);
            }
        }
        else { /* this is an initial read lock request */

            switch (rw_lock->policy) {

                case H5TS__RW_LOCK_POLICY__FAVOR_WRITERS:
                    if ((rw_lock->active_writers != 0) || (rw_lock->waiting_writers_count != 0)) {

                        int delayed = rw_lock->waiting_readers_count + 1;

                        REC_RW_LOCK_STATS__UPDATE_FOR_RD_LOCK_DELAY(rw_lock, delayed);
                    }

                    while ((rw_lock->active_writers != 0) || (rw_lock->waiting_writers_count != 0)) {

                        rw_lock->waiting_readers_count++;

                        result = pthread_cond_wait(&(rw_lock->readers_cv), &(rw_lock->mutex));

                        rw_lock->waiting_readers_count--;

                        if (result != 0) {

                            ret_value = FAIL;
                            break;
                        }
                    }
                    break;

                default:
                    ret_value = FAIL;
                    break;
            }

            if ((ret_value == SUCCEED) && (NULL == (count = H5TS_alloc_rec_entry_count(FALSE)))) {

                ret_value = FAIL;
            }

            if ((ret_value == SUCCEED) &&
                (pthread_setspecific(rw_lock->rec_entry_count_key, (void *)count) != 0)) {

                ret_value = FAIL;
            }

            if (ret_value == SUCCEED) {

                rw_lock->active_readers++;

                HDassert(count->rec_lock_count == 1);

                REC_RW_LOCK_STATS__UPDATE_FOR_RD_LOCK(rw_lock, count);
            }
        }
    }

    if (have_mutex) {

        pthread_mutex_unlock(&(rw_lock->mutex));
    }

    return ret_value;

} /* H5TS_rw_rdlock() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_rw_wrlock
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Attempt to obtain a write lock on the associated recursive read / write
 *    lock.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

herr_t
H5TS_rw_wrlock(H5TS_rw_lock_t *rw_lock)
{
    hbool_t               have_mutex = FALSE;
    int                   result;
    H5TS_rec_entry_count *count;
    herr_t                ret_value = SUCCEED;

    if ((rw_lock == NULL) || (rw_lock->magic != H5TS_RW_LOCK_MAGIC)) {

        ret_value = FAIL;
    }

    /* obtain the mutex */
    if (ret_value == SUCCEED) {

        if (pthread_mutex_lock(&(rw_lock->mutex)) != 0) {

            ret_value = FAIL;
        }
        else {

            have_mutex = TRUE;
        }
    }

    /* If there is no specific data for this thread, this is an
     * initial write lock request.
     */
    if (ret_value == SUCCEED) {

        count = (H5TS_rec_entry_count *)pthread_getspecific(rw_lock->rec_entry_count_key);

        if (count) { /* this is a recursive lock */

            if ((!(count->write_lock)) || (rw_lock->active_readers != 0) || (rw_lock->active_writers != 1)) {

                ret_value = FAIL;
            }
            else {

                count->rec_lock_count++;

                REC_RW_LOCK_STATS__UPDATE_FOR_WR_LOCK(rw_lock, count);
            }
        }
        else { /* this is an initial write lock request */

            switch (rw_lock->policy) {

                case H5TS__RW_LOCK_POLICY__FAVOR_WRITERS:
                    if ((rw_lock->active_readers > 0) || (rw_lock->active_writers > 0)) {

                        int delayed = rw_lock->waiting_writers_count + 1;

                        REC_RW_LOCK_STATS__UPDATE_FOR_WR_LOCK_DELAY(rw_lock, delayed);
                    }

                    while ((rw_lock->active_readers > 0) || (rw_lock->active_writers > 0)) {

                        rw_lock->waiting_writers_count++;

                        result = pthread_cond_wait(&(rw_lock->writers_cv), &(rw_lock->mutex));

                        rw_lock->waiting_writers_count--;

                        if (result != 0) {

                            ret_value = FAIL;
                            break;
                        }
                    }
                    break;

                default:
                    ret_value = FAIL;
                    break;
            }

            if ((ret_value == SUCCEED) && (NULL == (count = H5TS_alloc_rec_entry_count(TRUE)))) {

                ret_value = FAIL;
            }

            if ((ret_value == SUCCEED) &&
                (pthread_setspecific(rw_lock->rec_entry_count_key, (void *)count) != 0)) {

                ret_value = FAIL;
            }

            if (ret_value == SUCCEED) {

                rw_lock->active_writers++;

                HDassert(count->rec_lock_count == 1);

                REC_RW_LOCK_STATS__UPDATE_FOR_WR_LOCK(rw_lock, count);
            }
        }
    }

    if (have_mutex) {

        pthread_mutex_unlock(&(rw_lock->mutex));
    }

    return ret_value;

} /* H5TS_rw_wrlock() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_rw_unlock
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Attempt to unlock either a read or a write lock on the supplied
 *    recursive read / write lock.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

herr_t
H5TS_rw_unlock(H5TS_rw_lock_t *rw_lock)
{
    hbool_t               have_mutex        = FALSE;
    hbool_t               discard_rec_count = FALSE;
    H5TS_rec_entry_count *count;
    herr_t                ret_value = SUCCEED;

    if ((rw_lock == NULL) || (rw_lock->magic != H5TS_RW_LOCK_MAGIC)) {

        ret_value = FAIL;
    }

    /* obtain the mutex */
    if (ret_value == SUCCEED) {

        if (pthread_mutex_lock(&(rw_lock->mutex)) != 0) {

            ret_value = FAIL;
        }
        else {

            have_mutex = TRUE;
        }
    }

    /* If there is no specific data for this thread, no lock was held,
     * and thus the unlock call must fail.
     */
    if (ret_value == SUCCEED) {

        count = (H5TS_rec_entry_count *)pthread_getspecific(rw_lock->rec_entry_count_key);

        HDassert(count);
        HDassert(count->magic == H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC);
        HDassert(count->rec_lock_count > 0);

        if (NULL == count) {

            ret_value = FAIL;
        }
        else if (count->magic != H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC) {

            ret_value = FAIL;
        }
        else if (count->rec_lock_count <= 0) { /* corrupt count? */

            ret_value = FAIL;
        }
        else if (count->write_lock) { /* drop a write lock */

            HDassert((rw_lock->active_readers == 0) && (rw_lock->active_writers == 1));

            if ((rw_lock->active_readers != 0) || (rw_lock->active_writers != 1)) {

                ret_value = FAIL;
            }
            else {

                count->rec_lock_count--;

                HDassert(count->rec_lock_count >= 0);

                if (count->rec_lock_count == 0) {

                    /* make note that we must discard the
                     * recursive entry counter so it will not
                     * confuse us on the next lock request.
                     */
                    discard_rec_count = TRUE;

                    /* drop the write lock -- will signal later if needed */
                    rw_lock->active_writers--;

                    HDassert(rw_lock->active_writers == 0);
                }
            }

            REC_RW_LOCK_STATS__UPDATE_FOR_WR_UNLOCK(rw_lock, count);
        }
        else { /* drop a read lock */

            HDassert((rw_lock->active_readers > 0) && (rw_lock->active_writers == 0));

            if ((rw_lock->active_readers <= 0) || (rw_lock->active_writers != 0)) {

                ret_value = FAIL;
            }
            else {

                count->rec_lock_count--;

                HDassert(count->rec_lock_count >= 0);

                if (count->rec_lock_count == 0) {

                    /* make note that we must discard the
                     * recursive entry counter so it will not
                     * confuse us on the next lock request.
                     */
                    discard_rec_count = TRUE;

                    /* drop the read lock -- will signal later if needed */
                    rw_lock->active_readers--;
                }
            }

            REC_RW_LOCK_STATS__UPDATE_FOR_RD_UNLOCK(rw_lock, count);
        }

        if ((ret_value == SUCCEED) && (rw_lock->active_readers == 0) && (rw_lock->active_writers == 0)) {

            /* no locks held -- signal condition variables if required */

            switch (rw_lock->policy) {

                case H5TS__RW_LOCK_POLICY__FAVOR_WRITERS:

                    if (rw_lock->waiting_writers_count > 0) {

                        if (pthread_cond_signal(&(rw_lock->writers_cv)) != 0) {

                            ret_value = FAIL;
                        }
                    }
                    else if (rw_lock->waiting_readers_count > 0) {

                        if (pthread_cond_broadcast(&(rw_lock->readers_cv)) != 0) {

                            ret_value = FAIL;
                        }
                    }
                    break;

                default:
                    ret_value = FAIL;
                    break;
            }
        }
    }

    /* if we are really dropping the lock, must set the value of
     * rec_entry_count_key for this thread to NULL, so that
     * when this thread next requests a lock, it will appear
     * as an initial lock, not a recursive lock.
     */
    if (discard_rec_count) {

        HDassert(count);

        if (pthread_setspecific(rw_lock->rec_entry_count_key, (void *)NULL) != 0) {

            ret_value = FAIL;
        }

        H5TS_free_rec_entry_count((void *)count);
        count = NULL;
    }

    if (have_mutex) {

        pthread_mutex_unlock(&(rw_lock->mutex));
    }

    return ret_value;

} /* H5TS_rw_unlock() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_rw_lock_get_stats
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Obtain a copy of the current statistics on the supplied
 *    recursive read / write lock.  Note that to obtain a consistent
 *    set of statistics, the function must obtain the lock mutex.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

herr_t
H5TS_rw_lock_get_stats(H5TS_rw_lock_t *rw_lock, H5TS_rw_lock_stats_t *stats)
{
    hbool_t have_mutex = FALSE;
    herr_t  ret_value  = SUCCEED;

    if ((rw_lock == NULL) || (rw_lock->magic != H5TS_RW_LOCK_MAGIC) || (stats == NULL)) {

        ret_value = FAIL;
    }

    /* obtain the mutex */
    if (ret_value == SUCCEED) {

        if (pthread_mutex_lock(&(rw_lock->mutex)) != 0) {

            ret_value = FAIL;
        }
        else {

            have_mutex = TRUE;
        }
    }

    if (ret_value == SUCCEED) {

        *stats = rw_lock->stats;
    }

    if (have_mutex) {

        pthread_mutex_unlock(&(rw_lock->mutex));
    }

    return ret_value;

} /* H5TS_rw_lock_get_stats() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_rw_lock_reset_stats
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Reset the statistics for the supplied recursive read / write lock.
 *    Note that to reset the statistics consistently, the function must
 *    obtain the lock mutex.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

herr_t
H5TS_rw_lock_reset_stats(H5TS_rw_lock_t *rw_lock)
{
    hbool_t have_mutex = FALSE;
    /* update this initializer if you modify H5TS_rw_lock_stats_t */
    static const H5TS_rw_lock_stats_t reset_stats = {/* read_locks_granted             = */ 0,
                                                     /* read_locks_released            = */ 0,
                                                     /* real_read_locks_granted        = */ 0,
                                                     /* real_read_locks_released       = */ 0,
                                                     /* max_read_locks                 = */ 0,
                                                     /* max_read_lock_recursion_depth  = */ 0,
                                                     /* read_locks_delayed             = */ 0,
                                                     /* max_read_locks_pending         = */ 0,
                                                     /* write_locks_granted            = */ 0,
                                                     /* write_locks_released           = */ 0,
                                                     /* real_write_locks_granted       = */ 0,
                                                     /* real_write_locks_released      = */ 0,
                                                     /* max_write_locks                = */ 0,
                                                     /* max_write_lock_recursion_depth = */ 0,
                                                     /* write_locks_delayed            = */ 0,
                                                     /* max_write_locks_pending        = */ 0};
    herr_t                            ret_value   = SUCCEED;

    if ((rw_lock == NULL) || (rw_lock->magic != H5TS_RW_LOCK_MAGIC)) {

        ret_value = FAIL;
    }

    /* obtain the mutex */
    if (ret_value == SUCCEED) {

        if (pthread_mutex_lock(&(rw_lock->mutex)) != 0) {

            ret_value = FAIL;
        }
        else {

            have_mutex = TRUE;
        }
    }

    if (ret_value == SUCCEED) {

        rw_lock->stats = reset_stats;
    }

    if (have_mutex) {

        pthread_mutex_unlock(&(rw_lock->mutex));
    }

    return ret_value;

} /* H5TS_rw_lock_reset_stats() */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_rw_lock_print_stats
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Print the supplied pthresds recursive R/W lock statistics to
 *    standard out.
 *
 *    UPDATE THIS FUNCTION IF YOU MODIFY H5TS_rw_lock_stats_t.
 *
 * PROGRAMMER: John Mainzer
 *             August 28, 2020
 *
 *--------------------------------------------------------------------------
 */

herr_t
H5TS_rw_lock_print_stats(const char *header_str, H5TS_rw_lock_stats_t *stats)
{
    herr_t ret_value = SUCCEED;

    if ((header_str == NULL) || (stats == NULL)) {

        ret_value = FAIL;
    }
    else {

        HDfprintf(stdout, "\n\n%s\n\n", header_str);
        HDfprintf(stdout, "  read_locks_granted             = %lld\n",
                  (long long int)(stats->read_locks_granted));
        HDfprintf(stdout, "  read_locks_released            = %lld\n",
                  (long long int)(stats->read_locks_released));
        HDfprintf(stdout, "  real_read_locks_granted        = %lld\n",
                  (long long int)(stats->real_read_locks_granted));
        HDfprintf(stdout, "  real_read_locks_released       = %lld\n",
                  (long long int)(stats->real_read_locks_released));
        HDfprintf(stdout, "  max_read_locks                 = %lld\n",
                  (long long int)(stats->max_read_locks));
        HDfprintf(stdout, "  max_read_lock_recursion_depth  = %lld\n",
                  (long long int)(stats->max_read_lock_recursion_depth));
        HDfprintf(stdout, "  read_locks_delayed             = %lld\n",
                  (long long int)(stats->read_locks_delayed));
        HDfprintf(stdout, "  max_read_locks_pending         = %lld\n",
                  (long long int)(stats->max_read_locks_pending));
        HDfprintf(stdout, "  write_locks_granted            = %lld\n",
                  (long long int)(stats->write_locks_granted));
        HDfprintf(stdout, "  write_locks_released           = %lld\n",
                  (long long int)(stats->write_locks_released));
        HDfprintf(stdout, "  real_write_locks_granted       = %lld\n",
                  (long long int)(stats->real_write_locks_granted));
        HDfprintf(stdout, "  real_write_locks_released      = %lld\n",
                  (long long int)(stats->real_write_locks_released));
        HDfprintf(stdout, "  max_write_locks                = %lld\n",
                  (long long int)(stats->max_write_locks));
        HDfprintf(stdout, "  max_write_lock_recursion_depth = %lld\n",
                  (long long int)(stats->max_write_lock_recursion_depth));
        HDfprintf(stdout, "  write_locks_delayed            = %lld\n",
                  (long long int)(stats->write_locks_delayed));
        HDfprintf(stdout, "  max_write_locks_pending        = %lld\n\n",
                  (long long int)(stats->max_write_locks_pending));
    }

    return ret_value;

} /* H5TS_rw_lock_print_stats() */
#endif /* H5_USE_RECURSIVE_WRITER_LOCKS */

#endif /* H5_HAVE_THREADSAFE */
