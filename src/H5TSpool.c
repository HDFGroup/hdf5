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
 * Purpose: This file contains an implementation of a simple thread-pool,
 *        using the H5TS package's portable objects and operations.
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
#include "H5private.h"   /* Generic Functions                   */
#include "H5Eprivate.h"  /* Error handling                      */
#include "H5FLprivate.h" /* Free Lists                          */
#include "H5TSpkg.h"     /* Threadsafety                        */

#ifdef H5_HAVE_THREADS

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/* Thread pool task */
typedef struct H5TS_pool_task_t {
    H5TS_thread_start_func_t func; /* Function to invoke with thread */
    void                    *ctx;  /* Context for task's function */
    struct H5TS_pool_task_t *next; /* Pointer to next task */
} H5TS_pool_task_t;

/* Thread pool */
struct H5TS_pool_t {
    H5TS_mutex_t mutex; /* Mutex to control access to pool struct */
    H5TS_cond_t  cond;

    bool           shutdown;    /* Pool is shutting down */
    unsigned       num_threads; /* # of threads in pool */
    H5TS_thread_t *threads;     /* Array of worker threads in pool */

    H5TS_pool_task_t *head, *tail; /* Task queue */
};

/********************/
/* Local Prototypes */
/********************/
static herr_t                  H5TS__pool_free(H5TS_pool_t *pool);
static H5TS_THREAD_RETURN_TYPE H5TS__pool_do(void *_pool);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage H5TS_pool_t structs */
H5FL_DEFINE_STATIC(H5TS_pool_t);

/* Declare a free list to manage sequences of H5TS_thread_t structs */
H5FL_SEQ_DEFINE_STATIC(H5TS_thread_t);

/*--------------------------------------------------------------------------
 * Function:    H5TS_pool_free
 *
 * Purpose:     Free resources for a thread pool, waiting for tasks to be invoked
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
static herr_t
H5TS__pool_free(H5TS_pool_t *pool)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    /* Sanity check */
    assert(pool);

    /* Join all threads */
    for (unsigned u = 0; u < pool->num_threads; u++)
        if (H5_UNLIKELY(H5TS_thread_join(pool->threads[u], NULL) < 0))
            HGOTO_DONE(FAIL);

    /* Destroy the pool's mutex and condition variable */
    if (H5_UNLIKELY(H5TS_mutex_destroy(&pool->mutex) < 0))
        HGOTO_DONE(FAIL);
    if (H5_UNLIKELY(H5TS_cond_destroy(&pool->cond) < 0))
        HGOTO_DONE(FAIL);

    /* Release memory */
    if (pool->threads)
        H5FL_SEQ_FREE(H5TS_thread_t, pool->threads);
    H5FL_FREE(H5TS_pool_t, pool);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__pool_free() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__pool_do
 *
 * Purpose:     Routine for worker threads to service tasks
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
static H5TS_THREAD_RETURN_TYPE
H5TS__pool_do(void *_pool)
{
    H5TS_pool_t      *pool       = (H5TS_pool_t *)_pool; /* Pool for threads */
    bool              have_mutex = false;                /* Whether we're holding the mutex */
    H5TS_thread_ret_t ret_value  = (H5TS_thread_ret_t)0;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    /* Acquire tasks and invoke them, until pool is shut down */
    while (1) {
        /* Acquire the mutex for the pool */
        if (H5_UNLIKELY(H5TS_mutex_lock(&pool->mutex) < 0))
            HGOTO_DONE((H5TS_thread_ret_t)1);
        have_mutex = true;

        /* If queue is empty and pool is not shutting down, wait for a task */
        while (NULL == pool->head && !pool->shutdown)
            if (H5_UNLIKELY(H5TS_cond_wait(&pool->cond, &pool->mutex) < 0))
                HGOTO_DONE((H5TS_thread_ret_t)1);

        /* If there's a task, invoke it, else we're shutting down */
        if (NULL != pool->head) {
            H5TS_pool_task_t *task; /* Task to invoke */

            /* Grab our task */
            task = pool->head;
            if (task->next)
                pool->head = task->next;
            else
                pool->head = pool->tail = NULL;

            /* Release the pool's mutex */
            if (H5_UNLIKELY(H5TS_mutex_unlock(&pool->mutex) < 0))
                HGOTO_DONE((H5TS_thread_ret_t)1);
            have_mutex = false;

            /* Invoke function for task */
            (*task->func)(task->ctx);

            /* Free the task node */
            free(task);
        }
        else {
            assert(pool->shutdown);
            break;
        }
    }

done:
    /* Release the pool's mutex, if we're holding it */
    if (have_mutex)
        if (H5_UNLIKELY(H5TS_mutex_unlock(&pool->mutex) < 0))
            ret_value = (H5TS_thread_ret_t)1;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS__pool_do() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_pool_create
 *
 * Purpose:     Create a new thread pool, with the given # of threads
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_pool_create(H5TS_pool_t **pool, unsigned num_threads)
{
    H5TS_pool_t *new_pool = NULL; /* Newly created pool */
    unsigned     u;               /* Local index variable */
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Sanity checks */
    if (H5_UNLIKELY(NULL == pool))
        HGOTO_DONE(FAIL);
    if (H5_UNLIKELY(0 == num_threads))
        HGOTO_DONE(FAIL);

    /* Allocate new pool */
    if (H5_UNLIKELY(NULL == (new_pool = H5FL_MALLOC(H5TS_pool_t))))
        HGOTO_DONE(FAIL);

    /* Initialize pool fields to defaults */
    memset(new_pool, 0, sizeof(*new_pool));
    if (H5_UNLIKELY(H5TS_mutex_init(&new_pool->mutex, H5TS_MUTEX_TYPE_PLAIN) < 0))
        HGOTO_DONE(FAIL);
    if (H5_UNLIKELY(H5TS_cond_init(&new_pool->cond) < 0))
        HGOTO_DONE(FAIL);

    /* Allocate array of threads */
    if (H5_UNLIKELY(NULL == (new_pool->threads = H5FL_SEQ_MALLOC(H5TS_thread_t, num_threads))))
        HGOTO_DONE(FAIL);

    /* Set # of threads */
    new_pool->num_threads = num_threads;

    /* Start worker threads */
    for (u = 0; u < num_threads; u++)
        /* Create thread, which immediately starts processing tasks */
        if (H5_UNLIKELY(H5TS_thread_create(&new_pool->threads[u], H5TS__pool_do, (void *)new_pool) < 0))
            HGOTO_DONE(FAIL);

    /* Check for error when starting threads */
    if (u != new_pool->num_threads) {
        /* Set # of threads successfully created (for joining them, in free routine) */
        new_pool->num_threads = u;
        HGOTO_DONE(FAIL);
    }

    /* Set return value */
    *pool = new_pool;

done:
    if (H5_UNLIKELY(ret_value < 0))
        if (new_pool) {
            /* Tell any existing threads that the pool is shutting down */
            new_pool->shutdown = true;

            /* Free pool */
            H5TS__pool_free(new_pool);
        }

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_pool_create() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_pool_add_task
 *
 * Purpose:     Add a new task to a pool
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_pool_add_task(H5TS_pool_t *pool, H5TS_thread_start_func_t func, void *ctx)
{
    H5TS_pool_task_t *task       = NULL;  /* Task for function to invoke */
    bool              have_mutex = false; /* Whether we're holding the mutex */
    herr_t            ret_value  = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Sanity checks */
    if (H5_UNLIKELY(NULL == pool))
        HGOTO_DONE(FAIL);
    if (H5_UNLIKELY(NULL == func))
        HGOTO_DONE(FAIL);

    /* Allocate & initialize new task */
    if (H5_UNLIKELY(NULL == (task = malloc(sizeof(H5TS_pool_task_t)))))
        HGOTO_DONE(FAIL);
    task->func = func;
    task->ctx  = ctx;
    task->next = NULL;

    /* Acquire the mutex for the pool */
    if (H5_UNLIKELY(H5TS_mutex_lock(&pool->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Is pool shutting down? */
    if (H5_UNLIKELY(pool->shutdown))
        HGOTO_DONE(FAIL);

    /* Add task to pool's queue */
    if (NULL != pool->tail) {
        pool->tail->next = task;
        pool->tail       = task;
    }
    else
        pool->head = pool->tail = task;

    /* Avoid freeing the task on error, now */
    task = NULL;

    /* Wake up any sleeping worker */
    if (H5_UNLIKELY(H5TS_cond_signal(&pool->cond) < 0))
        HGOTO_DONE(FAIL);

done:
    /* Release the pool's mutex, if we're holding it */
    if (H5_LIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&pool->mutex) < 0))
            ret_value = FAIL;

    if (H5_UNLIKELY(ret_value < 0))
        if (task)
            free(task);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_pool_add_task() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_pool_destroy
 *
 * Purpose:     Destroy a thread pool, waiting for all tasks to be invoked
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS_pool_destroy(H5TS_pool_t *pool)
{
    bool   have_mutex = false; /* Whether we're holding the mutex */
    herr_t ret_value  = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Sanity checks */
    if (H5_UNLIKELY(NULL == pool))
        HGOTO_DONE(FAIL);

    /* Acquire the mutex for the pool */
    if (H5_UNLIKELY(H5TS_mutex_lock(&pool->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = true;

    /* Tell any existing threads that the pool is shutting down */
    pool->shutdown = true;

    /* Wake all the worker threads up */
    if (H5_UNLIKELY(H5TS_cond_broadcast(&pool->cond) < 0))
        HGOTO_DONE(FAIL);

    /* Release the pool's mutex */
    if (H5_UNLIKELY(H5TS_mutex_unlock(&pool->mutex) < 0))
        HGOTO_DONE(FAIL);
    have_mutex = false;

    /* Free pool */
    if (H5_UNLIKELY(H5TS__pool_free(pool) < 0))
        HGOTO_DONE(FAIL);

done:
    /* Release the pool's mutex, if we're holding it */
    if (H5_UNLIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&pool->mutex) < 0))
            ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_pool_destroy() */

#endif /* H5_HAVE_THREADS */
