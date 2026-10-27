/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/********************************************************************
 *
 * Test the cached per-thread API context and error stack pointers.
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADS

#define TINFO_THREADS    4
#define TINFO_ITERATIONS 10000

typedef struct tinfo_test_data_t {
    unsigned        index;
    bool            reset_between_cycles;
    bool            failed;
    H5TS_barrier_t *barrier;
    void          **contexts;
    void          **error_stacks;
} tinfo_test_data_t;

static H5TS_THREAD_RETURN_TYPE
tinfo_cache_thread(void *_data)
{
    tinfo_test_data_t   *data = (tinfo_test_data_t *)_data;
    H5CX_node_t          marker;
    struct H5CX_node_t **context;
    struct H5E_stack_t  *error_stack;
    int                  i;

    context = H5TS_get_api_ctx_ptr();
    if (NULL == context) {
        TestErrPrintf("thread %u could not retrieve its API context pointer\n", data->index);
        data->failed = true;
    }

    error_stack = H5TS_get_err_stack();
    if (NULL == error_stack) {
        TestErrPrintf("thread %u could not retrieve its error stack pointer\n", data->index);
        data->failed = true;
    }

    if (!data->failed) {
        if (data->reset_between_cycles && NULL != *context) {
            TestErrPrintf("thread %u received a stale API context\n", data->index);
            data->failed = true;
        }

        for (i = 0; i < TINFO_ITERATIONS; i++) {
            if (context != H5TS_get_api_ctx_ptr() || error_stack != H5TS_get_err_stack()) {
                TestErrPrintf("thread %u observed a changing thread-info pointer\n", data->index);
                data->failed = true;
                break;
            }
        }

        data->contexts[data->index]     = context;
        data->error_stacks[data->index] = error_stack;
        *context                        = &marker;
    }

    if (H5TS_barrier_wait(data->barrier) < 0) {
        TestErrPrintf("thread %u failed to synchronize\n", data->index);
        data->failed = true;
    }

    return (H5TS_thread_ret_t)0;
}

/*
 **********************************************************************
 * tts_tinfo_cache
 *
 * Verify that the native TLS cache preserves per-thread pointer identity,
 * resets recycled records, and keeps the cleanup key authoritative.
 **********************************************************************
 */
void
tts_tinfo_cache(void H5_ATTR_UNUSED *params)
{
    struct H5CX_node_t **main_context_ptr;
    H5CX_node_t         *main_context;
    struct H5E_stack_t  *main_error_stack;
    unsigned             cycle;

    main_context_ptr = H5TS_get_api_ctx_ptr();
    main_error_stack = H5TS_get_err_stack();
    if (NULL == main_context_ptr || NULL == main_error_stack) {
        TestErrPrintf("main thread could not retrieve its thread-info pointers\n");
        return;
    }
    main_context = *main_context_ptr;

    for (cycle = 0; cycle < 2; cycle++) {
        tinfo_test_data_t thread_data[TINFO_THREADS];
        H5TS_thread_t     threads[TINFO_THREADS];
        void             *contexts[TINFO_THREADS];
        void             *error_stacks[TINFO_THREADS];
        H5TS_barrier_t    barrier;
        unsigned          i;

        memset(thread_data, 0, sizeof(thread_data));
        memset(contexts, 0, sizeof(contexts));
        memset(error_stacks, 0, sizeof(error_stacks));

        if (H5TS_barrier_init(&barrier, TINFO_THREADS) < 0) {
            TestErrPrintf("H5TS_barrier_init failed\n");
            return;
        }

        for (i = 0; i < TINFO_THREADS; i++) {
            thread_data[i].index                = i;
            thread_data[i].reset_between_cycles = true;
            thread_data[i].barrier              = &barrier;
            thread_data[i].contexts             = contexts;
            thread_data[i].error_stacks         = error_stacks;
            if (H5TS_thread_create(&threads[i], tinfo_cache_thread, &thread_data[i]) < 0) {
                TestErrPrintf("thread %u did not start\n", i);
                break;
            }
        }

        for (i = 0; i < TINFO_THREADS; i++)
            if (H5TS_thread_join(threads[i], NULL) < 0)
                TestErrPrintf("thread %u failed to join\n", i);

        if (H5TS_barrier_destroy(&barrier) < 0)
            TestErrPrintf("H5TS_barrier_destroy failed\n");

        for (i = 0; i < TINFO_THREADS; i++) {
            unsigned j;

            if (NULL == contexts[i] || NULL == error_stacks[i]) {
                TestErrPrintf("cycle %u thread %u did not publish its pointers\n", cycle, i);
                continue;
            }

            for (j = i + 1; j < TINFO_THREADS; j++) {
                if (contexts[i] == contexts[j])
                    TestErrPrintf("cycle %u threads %u and %u shared an API context pointer\n", cycle, i, j);
                if (error_stacks[i] == error_stacks[j])
                    TestErrPrintf("cycle %u threads %u and %u shared an error stack pointer\n", cycle, i, j);
            }
        }
    }

    if (main_context_ptr != H5TS_get_api_ctx_ptr() || main_context != *main_context_ptr ||
        main_error_stack != H5TS_get_err_stack())
        TestErrPrintf("main thread-info pointers changed while worker threads ran\n");
}

#endif /* H5_HAVE_THREADS */
