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

#define H5E_FRIEND      /* Suppress error about including H5Epkg    */
#include "H5TSmodule.h" /* This source code file is part of the H5TS module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                   */
#include "H5CXprivate.h" /* API Contexts                        */
#include "H5Epkg.h"      /* Error handling                      */
#include "H5TSpkg.h"     /* Threadsafety                        */

#ifdef H5_HAVE_THREADSAFE

/****************/
/* Local Macros */
/****************/

#ifdef H5_HAVE_WIN_THREADS
#define H5TS_ONCE_INIT_FUNC H5TS__win32_process_enter
#else
#define H5TS_ONCE_INIT_FUNC H5TS__pthread_first_thread_init
#endif /* H5_HAVE_WIN_THREADS */

/******************/
/* Local Typedefs */
/******************/

/* Per-thread info */
typedef struct H5TS_thread_info_t {
    uint64_t            id;               /* Unique ID for each thread */
    struct H5CX_node_t *api_ctx_node_ptr; /* Pointer to an API context node */
    H5E_t               err_stack;        /* Error stack */
} H5TS_thread_info_t;

/* An H5TS_tinfo_node_t is a thread info that is available for reuse */
typedef struct H5TS_tinfo_node_t {
    struct H5TS_tinfo_node_t *next;
    H5TS_thread_info_t        info;
} H5TS_tinfo_node_t;

/********************/
/* Local Prototypes */
/********************/
static H5TS_tinfo_node_t *H5TS__tinfo_create(void);

/*********************/
/* Package Variables */
/*********************/

/* Per-thread info */
H5TS_key_t H5TS_thrd_info_key_g;

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Has threadsafety code been initialized? */
static H5TS_once_t H5TS_first_init_s = H5TS_ONCE_INITIALIZER;

/* Pointer to first free thread info record or NULL. */
static H5TS_tinfo_node_t *H5TS_tinfo_next_free_s = NULL;
static uint64_t           H5TS_next_thrd_id_s    = 0;

/* Mutex for access to H5TS_tinfo_next_free_s and H5TS_next_thrd_id_s */
static H5TS_mutex_t H5TS_tinfo_mtx_s = H5TS_MUTEX_INITIALIZER;

/*-------------------------------------------------------------------------
 * Function: H5TS_term_package
 *
 * Purpose:  Terminate this interface.
 *
 * Note:     This function is currently registered via atexit() and is called
 *              AFTER H5_term_library().
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
void
H5TS_term_package(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Destroy global API lock */
    H5TS__ex_lock_destroy(&H5TS_api_info_p.api_lock);

    /* Destroy the "lock acquisition attempt" mutex */
    H5TS_mutex_destroy(&H5TS_api_info_p.attempt_mutex);

    /* Clean up per-process thread local storage */
    H5TS__tinfo_term();

    FUNC_LEAVE_NOAPI_VOID
} /* end H5TS_term_package() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__mutex_acquire
 *
 * Purpose:     Attempts to acquire the API lock, without blocking
 *
 * Note:        On success, the 'acquired' flag indicates if the HDF5 library
 *              global lock was acquired.
 *
 * Note:        The Windows threads code is very likely bogus.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__mutex_acquire(unsigned int lock_count, bool *acquired)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    /* Attempt to acquire the lock */
    if (H5_UNLIKELY(H5TS__ex_acquire(&H5TS_api_info_p.api_lock, lock_count, acquired) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__mutex_acquire() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_api_lock
 *
 * Purpose:     Increment the global "API" lock counter for accessing the HDF5
 *              library, acquiring the lock for the thread if the counter is
 *              initially 0.
 *
 * Note:        Multiple (usually recursive) acquisitions of the "API" lock by
 *              the same thread is permitted with corresponding unlock
 *              operation(s).
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_api_lock(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Initialize the thread-safety code, once */
    if (H5_UNLIKELY(!H5_INIT_GLOBAL))
        if (H5_UNLIKELY(H5TS_once(&H5TS_first_init_s, H5TS_ONCE_INIT_FUNC) < 0))
            HGOTO_DONE(FAIL);

    /* Acquire the "attempt" mutex, increment the attempt lock count, release the lock */
    if (H5_UNLIKELY(H5TS_mutex_lock(&H5TS_api_info_p.attempt_mutex) < 0))
        HGOTO_DONE(FAIL);
    H5TS_api_info_p.attempt_lock_count++;
    if (H5_UNLIKELY(H5TS_mutex_unlock(&H5TS_api_info_p.attempt_mutex) < 0))
        HGOTO_DONE(FAIL);

    /* Acquire the library's exclusive API lock */
    if (H5_UNLIKELY(H5TS__ex_lock(&H5TS_api_info_p.api_lock) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_api_lock() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__mutex_release
 *
 * Purpose:     Release the global "API" lock for accessing the HDF5 library.
 *              Passes back the previous lock count to the caller in a
 *              parameter.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__mutex_release(unsigned *lock_count)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Release the library's exclusive API lock */
    if (H5_UNLIKELY(H5TS__ex_release(&H5TS_api_info_p.api_lock, lock_count) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* H5TS__mutex_release */

/*--------------------------------------------------------------------------
 * Function:    H5TS_api_unlock
 *
 * Purpose:     Decrements the global "API" lock counter for accessing the
 *              HDF5 library, releasing the lock when the counter reaches 0.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_api_unlock(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Release the library's exclusive API lock */
    if (H5_UNLIKELY(H5TS__ex_unlock(&H5TS_api_info_p.api_lock) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* H5TS_api_unlock */

/*--------------------------------------------------------------------------
 * Function:    H5TS__tinfo_init
 *
 * Purpose:     Initialize thread-local key for per-thread info
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__tinfo_init(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    /* Initialize the critical section for modifying the thread info globals */
    H5TS_mutex_init(&H5TS_tinfo_mtx_s);

    /* Initialize key for thread-specific API contexts */
#ifdef H5_HAVE_WIN_THREADS
    if (H5_UNLIKELY(H5TS_key_create(&H5TS_thrd_info_key_g, NULL) < 0))
        ret_value = FAIL;
#else
    if (H5_UNLIKELY(H5TS_key_create(&H5TS_thrd_info_key_g, H5TS__tinfo_destroy) < 0))
        ret_value = FAIL;
#endif

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__tinfo_init() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__tinfo_create
 *
 * Purpose:     Initialize per-thread info and set it for the thread-local key
 *
 * Return:      Pointer to per-thread info node on success / NULL on failure
 *
 *--------------------------------------------------------------------------
 */
static H5TS_tinfo_node_t *
H5TS__tinfo_create(void)
{
    uint64_t           new_id;
    H5TS_tinfo_node_t *tinfo_node;
    H5TS_tinfo_node_t *ret_value;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    /* Acquire the lock for modifying the thread info globals */
    /* Note: Must use lock here also, since 'destroy' callback can be
     *	invoked asynchronously when a thread is joined.
     */
    if (H5_UNLIKELY(H5TS_mutex_lock(&H5TS_tinfo_mtx_s) < 0))
        HGOTO_DONE(NULL);

    /* Reuse an info struct that's on the free list if possible */
    if (NULL != (tinfo_node = H5TS_tinfo_next_free_s))
        H5TS_tinfo_next_free_s = tinfo_node->next;

    /* Always use unique ID value for each thread, even when recycling a
     * H5TS_tinfo_node_t from the free list.
     *
     * Note: Don't worry about overflow for ID values
     */
    new_id = ++H5TS_next_thrd_id_s;

    /* Release the lock for modifying the thread info globals */
    if (H5_UNLIKELY(H5TS_mutex_unlock(&H5TS_tinfo_mtx_s) < 0))
        HGOTO_DONE(NULL);

    /* If a new info record is needed, allocate it */
    if (NULL == tinfo_node) {
        if (H5_UNLIKELY(NULL == (tinfo_node = H5MM_malloc(sizeof(*tinfo_node)))))
            HGOTO_DONE(NULL);
        tinfo_node->next = NULL;
    }

    /* Reset thread info struct */
    memset(tinfo_node, 0, sizeof(*tinfo_node));

    /* Set up non-zero per-thread info */
    tinfo_node->info.id = new_id;                       /* ID */
    H5E__set_default_auto(&tinfo_node->info.err_stack); /* Error stack */

    /* Set a thread-local pointer to the thread's info record */
    if (H5_UNLIKELY(H5TS_key_set_value(H5TS_thrd_info_key_g, tinfo_node))) {
        H5TS__tinfo_destroy(tinfo_node);
        HGOTO_DONE(NULL);
    }

    /* Success */
    ret_value = tinfo_node;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
}

/*--------------------------------------------------------------------------
 * Function:    H5TS_thread_id
 *
 * Purpose:     Return an identifier for the current thread.
 *
 *              The ID satisfies the following properties:
 *                1) 1 <= ID <= UINT64_MAX
 *                2) ID is constant over a thread's lifetime.
 *                3) No two threads share an ID
 *
 *              ID 0 is reserved.  H5TS_thread_id() returns 0 if the library
 *              was not built with thread safety or if an error prevents it
 *              from assigning an ID.
 *
 * Return:      ID for the current thread on success / 0 on failure
 *
 *--------------------------------------------------------------------------
 */
uint64_t
H5TS_thread_id(void)
{
    H5TS_tinfo_node_t *tinfo_node;
    uint64_t           ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check if info for thread has been created */
    if (H5_UNLIKELY(H5TS_key_get_value(H5TS_thrd_info_key_g, (void **)&tinfo_node) < 0))
        HGOTO_DONE(0);
    if (NULL == tinfo_node)
        /* Create thread info for this thread */
        if (H5_UNLIKELY(NULL == (tinfo_node = H5TS__tinfo_create())))
            HGOTO_DONE(0);

    /* Set return value */
    ret_value = tinfo_node->info.id;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* H5TS_thread_id() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_get_api_ctx_ptr
 *
 * Purpose:     Retrieve the address of the pointer to the head of API context
 *		stack for this thread.  (i.e. an H5CX_node_t **)
 *
 * Return:	Success: Non-NULL pointer to head pointer of API context stack for thread
 *		Failure: NULL
 *
 *--------------------------------------------------------------------------
 */
struct H5CX_node_t **
H5TS_get_api_ctx_ptr(void)
{
    H5TS_tinfo_node_t   *tinfo_node;
    struct H5CX_node_t **ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check if info for thread has been created */
    if (H5_UNLIKELY(H5TS_key_get_value(H5TS_thrd_info_key_g, (void **)&tinfo_node) < 0))
        HGOTO_DONE(NULL);
    if (NULL == tinfo_node)
        /* Create thread info for this thread */
        if (H5_UNLIKELY(NULL == (tinfo_node = H5TS__tinfo_create())))
            HGOTO_DONE(NULL);

    /* Set return value */
    ret_value = &tinfo_node->info.api_ctx_node_ptr;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* H5TS_get_api_ctx_ptr() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_get_err_stack
 *
 * Purpose:     Retrieve the address of error stack for this thread.
 *		(i.e. an H5E_t *)
 *
 * Return:	Success: Non-NULL pointer to error stack for thread
 *		Failure: NULL
 *
 *--------------------------------------------------------------------------
 */
H5E_t *
H5TS_get_err_stack(void)
{
    H5TS_tinfo_node_t *tinfo_node;
    H5E_t             *ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check if info for thread has been created */
    if (H5_UNLIKELY(H5TS_key_get_value(H5TS_thrd_info_key_g, (void **)&tinfo_node) < 0))
        HGOTO_DONE(NULL);
    if (NULL == tinfo_node)
        /* Create thread info for this thread */
        if (H5_UNLIKELY(NULL == (tinfo_node = H5TS__tinfo_create())))
            HGOTO_DONE(NULL);

    /* Set return value */
    ret_value = &tinfo_node->info.err_stack;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* H5TS_get_err_stack() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__tinfo_destroy
 *
 * Purpose:     When a thread shuts down, put its info record on the free list.
 *
 * Note:      	This routine runs asynchronously _outside_ of the library and
 *		is not covered by the library's API lock.  Therefore, protect
 *		access to the global variable with a mutex.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS__tinfo_destroy(void *_tinfo_node)
{
    H5TS_tinfo_node_t *tinfo_node = _tinfo_node;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    if (tinfo_node) {
        /* Add thread info node to the free list */
        H5TS_mutex_lock(&H5TS_tinfo_mtx_s);
        tinfo_node->next       = H5TS_tinfo_next_free_s;
        H5TS_tinfo_next_free_s = tinfo_node;
        H5TS_mutex_unlock(&H5TS_tinfo_mtx_s);
    }

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
}

/*--------------------------------------------------------------------------
 * Function:    H5TS__tinfo_term
 *
 * Purpose:     Terminate per-thread info at library shutdown
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__tinfo_term(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    /* Release nodes on the free list */
    if (H5_UNLIKELY(H5TS_mutex_lock(&H5TS_tinfo_mtx_s) < 0))
        HGOTO_DONE(FAIL);
    while (H5TS_tinfo_next_free_s) {
        H5TS_tinfo_node_t *next = H5TS_tinfo_next_free_s->next;
        H5MM_free(H5TS_tinfo_next_free_s);
        H5TS_tinfo_next_free_s = next;
    }
    if (H5_UNLIKELY(H5TS_mutex_unlock(&H5TS_tinfo_mtx_s) < 0))
        HGOTO_DONE(FAIL);

    /* Release critical section / mutex for modifying the thread info globals */
    if (H5_UNLIKELY(H5TS_mutex_destroy(&H5TS_tinfo_mtx_s) < 0))
        HGOTO_DONE(FAIL);

    /* Release key for thread-specific API contexts */
    if (H5_UNLIKELY(H5TS_key_delete(H5TS_thrd_info_key_g) < 0))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__tinfo_term() */

#endif /* H5_HAVE_THREADSAFE */
