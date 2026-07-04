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
 * Test the correctness of the lightweight system-based semaphores
 *
 ********************************************************************/

#include "ttsafe.h"

#if defined(H5_HAVE_THREADS)

#define NUM_PINGPONG     (250 * 1000)
#define NUM_CLIENTSERVER (50 * 1000)

#define NUM_THREADS 16

typedef struct {
    H5TS_semaphore_t ping_sem, pong_sem;
    unsigned         ping_counter;
    unsigned         pong_counter;
} pingpong_t;

typedef struct {
    H5TS_semaphore_t ready_sem, work_avail_sem;
    unsigned         counter;
    bool             shutdown;
} clientserver_t;

static H5TS_THREAD_RETURN_TYPE
ping(void *_test_info)
{
    pingpong_t       *test_info = (pingpong_t *)_test_info;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    do {
        result = H5TS_semaphore_wait(&test_info->ping_sem);
        CHECK_I(result, "H5TS_semaphore_wait");

        test_info->ping_counter++;

        result = H5TS_semaphore_signal(&test_info->pong_sem);
        CHECK_I(result, "H5TS_semaphore_signal");
    } while (test_info->ping_counter < NUM_PINGPONG);

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
pong(void *_test_info)
{
    pingpong_t       *test_info = (pingpong_t *)_test_info;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    do {
        result = H5TS_semaphore_wait(&test_info->pong_sem);
        CHECK_I(result, "H5TS_semaphore_wait");

        test_info->pong_counter++;

        result = H5TS_semaphore_signal(&test_info->ping_sem);
        CHECK_I(result, "H5TS_semaphore_signal");
    } while (test_info->pong_counter < NUM_PINGPONG);

    return ret_value;
}

/*
 **********************************************************************
 * Ping-pong between two threads, using semaphores
 **********************************************************************
 */
static void
tts_semaphore_pingpong(void)
{
    H5TS_thread_t ping_thread, pong_thread;
    pingpong_t    test_info;
    herr_t        result;

    /* Test set up */
    result = H5TS_semaphore_init(&test_info.ping_sem, 0);
    CHECK_I(result, "H5TS_semaphore_init");
    result = H5TS_semaphore_init(&test_info.pong_sem, 0);
    CHECK_I(result, "H5TS_semaphore_init");
    test_info.ping_counter = 0;
    test_info.pong_counter = 0;

    /* Start ping & pong threads */
    result = H5TS_thread_create(&ping_thread, ping, &test_info);
    CHECK_I(result, "H5TS_thread_create");
    result = H5TS_thread_create(&pong_thread, pong, &test_info);
    CHECK_I(result, "H5TS_thread_create");

    /* Release ping thread */
    result = H5TS_semaphore_signal(&test_info.ping_sem);
    CHECK_I(result, "H5TS_semaphore_signal");

    /* Join ping & pong threads */
    result = H5TS_thread_join(ping_thread, NULL);
    CHECK_I(result, "H5TS_thread_join");
    result = H5TS_thread_join(pong_thread, NULL);
    CHECK_I(result, "H5TS_thread_join");

    VERIFY(test_info.ping_counter, NUM_PINGPONG, "ping counter");
    VERIFY(test_info.pong_counter, NUM_PINGPONG, "pong counter");

    /* Destroy semaphores */
    result = H5TS_semaphore_destroy(&test_info.ping_sem);
    CHECK_I(result, "H5TS_semaphore_destroy");
    result = H5TS_semaphore_destroy(&test_info.pong_sem);
    CHECK_I(result, "H5TS_semaphore_destroy");
} /* end tts_semaphore_pingpong() */

static H5TS_THREAD_RETURN_TYPE
client(void *_test_info)
{
    clientserver_t   *test_info = (clientserver_t *)_test_info;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    do {
        /* Tell server this client is ready */
        result = H5TS_semaphore_signal(&test_info->ready_sem);
        CHECK_I(result, "H5TS_semaphore_signal");

        /* Wait for work */
        result = H5TS_semaphore_wait(&test_info->work_avail_sem);
        CHECK_I(result, "H5TS_semaphore_wait");

        /* Check for shutdown */
        if (test_info->shutdown)
            break;

        /* "work" */
        test_info->counter--;
    } while (1);

    return ret_value;
}

/*
 **********************************************************************
 * Many clients, single server
 **********************************************************************
 */
static void
tts_semaphore_clientserver(void)
{
    H5TS_thread_t  client_threads[NUM_THREADS];
    clientserver_t test_info[NUM_THREADS];
    unsigned       u;
    herr_t         result;

    for (u = 0; u < NUM_THREADS; u++) {
        /* Test set up */
        result = H5TS_semaphore_init(&test_info[u].ready_sem, 0);
        CHECK_I(result, "H5TS_semaphore_init");
        result = H5TS_semaphore_init(&test_info[u].work_avail_sem, 0);
        CHECK_I(result, "H5TS_semaphore_init");
        test_info[u].counter  = 0;
        test_info[u].shutdown = false;

        /* Start client thread */
        result = H5TS_thread_create(&client_threads[u], client, &test_info[u]);
        CHECK_I(result, "H5TS_thread_create");
    }

    /* Issue "work" to clients */
    for (unsigned v = 0; v < NUM_CLIENTSERVER; v++)
        for (u = 0; u < NUM_THREADS; u++) {
            /* Wait for client to be ready */
            result = H5TS_semaphore_wait(&test_info[u].ready_sem);
            CHECK_I(result, "H5TS_semaphore_signal");

            /* Set up "work" */
            test_info[u].counter++;

            /* Signal client thread */
            result = H5TS_semaphore_signal(&test_info[u].work_avail_sem);
            CHECK_I(result, "H5TS_semaphore_signal");
        }

    /* Tell clients to shut down */
    for (u = 0; u < NUM_THREADS; u++) {
        /* Wait for client to be ready */
        result = H5TS_semaphore_wait(&test_info[u].ready_sem);
        CHECK_I(result, "H5TS_semaphore_signal");

        /* Set 'shutdown' flag */
        test_info[u].shutdown = true;

        /* Signal client thread */
        result = H5TS_semaphore_signal(&test_info[u].work_avail_sem);
        CHECK_I(result, "H5TS_semaphore_signal");
    }

    /* Wrap up */
    for (u = 0; u < NUM_THREADS; u++) {
        /* Join client thread */
        result = H5TS_thread_join(client_threads[u], NULL);
        CHECK_I(result, "H5TS_thread_join");

        /* Verify work counter */
        VERIFY(test_info[u].counter, 0, "client-server");

        /* Destroy semaphores */
        result = H5TS_semaphore_destroy(&test_info[u].ready_sem);
        CHECK_I(result, "H5TS_semaphore_destroy");
        result = H5TS_semaphore_destroy(&test_info[u].work_avail_sem);
        CHECK_I(result, "H5TS_semaphore_destroy");
    }
} /* end tts_semaphore_clientserver() */

/*
 **********************************************************************
 * tts_semaphore
 **********************************************************************
 */
void
tts_semaphore(void H5_ATTR_UNUSED *params)
{
    H5TS_semaphore_t sem;
    herr_t           result;

    /* Sanity checks on bad input */
    result = H5TS_semaphore_init(NULL, 0);
    VERIFY(result, FAIL, "H5TS_semaphore_init");
    result = H5TS_semaphore_signal(NULL);
    VERIFY(result, FAIL, "H5TS_semaphore_signal");
    result = H5TS_semaphore_wait(NULL);
    VERIFY(result, FAIL, "H5TS_semaphore_wait");
    result = H5TS_semaphore_destroy(NULL);
    VERIFY(result, FAIL, "H5TS_semaphore_destroy");

    /* Create & destroy semaphore */
    result = H5TS_semaphore_init(&sem, 0);
    CHECK_I(result, "H5TS_semaphore_init");

    result = H5TS_semaphore_destroy(&sem);
    CHECK_I(result, "H5TS_semaphore_destroy");

    /* Signal & wait w/same thread */
    result = H5TS_semaphore_init(&sem, 0);
    CHECK_I(result, "H5TS_semaphore_init");

    result = H5TS_semaphore_signal(&sem);
    CHECK_I(result, "H5TS_semaphore_signal");

    result = H5TS_semaphore_wait(&sem);
    CHECK_I(result, "H5TS_semaphore_wait");

    result = H5TS_semaphore_destroy(&sem);
    CHECK_I(result, "H5TS_semaphore_destroy");

    /* Ping-pong test */
    tts_semaphore_pingpong();

    /* Client-server test */
    tts_semaphore_clientserver();
} /* end tts_semaphore() */

#endif /* defined(H5_HAVE_THREADS) */
