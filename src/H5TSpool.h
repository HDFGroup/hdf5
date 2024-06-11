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

/***********/
/* Headers */
/***********/

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

    bool           shutdown;         /* Pool is shutting down */
    unsigned       num_threads;      /* # of threads in pool */
    unsigned       sleeping_workers; /* # of workers currently sleeping */
    H5TS_thread_t *threads;          /* Array of worker threads in pool */

    H5TS_pool_task_t *head, *tail; /* Task queue */
};

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
 * Function:    H5TS_pool_add_task
 *
 * Purpose:     Add a new task to a pool
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
static inline herr_t
H5TS_pool_add_task(H5TS_pool_t *pool, H5TS_thread_start_func_t func, void *ctx)
{
    H5TS_pool_task_t *task       = NULL;  /* Task for function to invoke */
    bool              have_mutex = false; /* Whether we're holding the mutex */
    herr_t            ret_value  = SUCCEED;

    /* Sanity checks */
    if (H5_UNLIKELY(NULL == pool))
        return FAIL;
    if (H5_UNLIKELY(NULL == func))
        return FAIL;

    /* Allocate & initialize new task */
    if (H5_UNLIKELY(NULL == (task = (H5TS_pool_task_t *)malloc(sizeof(H5TS_pool_task_t)))))
        return FAIL;
    task->func = func;
    task->ctx  = ctx;
    task->next = NULL;

    /* Acquire the mutex for the pool */
    if (H5_UNLIKELY(H5TS_mutex_lock(&pool->mutex) < 0)) {
        ret_value = FAIL;
        goto done;
    }
    have_mutex = true;

    /* Is pool shutting down? */
    if (H5_UNLIKELY(pool->shutdown)) {
        ret_value = FAIL;
        goto done;
    }

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
    if (pool->sleeping_workers > 0 && H5_UNLIKELY(H5TS_cond_signal(&pool->cond) < 0))
        ret_value = FAIL;

done:
    /* Release the pool's mutex, if we're holding it */
    if (H5_LIKELY(have_mutex))
        if (H5_UNLIKELY(H5TS_mutex_unlock(&pool->mutex) < 0))
            ret_value = FAIL;

    if (H5_UNLIKELY(ret_value < 0))
        if (task)
            free(task);

    return (ret_value);
} /* end H5TS_pool_add_task() */
