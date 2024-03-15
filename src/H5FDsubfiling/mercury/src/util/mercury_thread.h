/**
 * Copyright (c) 2013-2022 UChicago Argonne, LLC and The HDF Group.
 * Copyright (c) 2022-2023 Intel Corporation.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_THREAD_H
#define MERCURY_THREAD_H

#if !defined(_WIN32) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif
#include "mercury_util_config.h"

#ifdef _WIN32
#define _WINSOCKAPI_
#include <windows.h>
typedef HANDLE                 hg_thread_t;
typedef LPTHREAD_START_ROUTINE hg_thread_func_t;
typedef DWORD                  hg_thread_ret_t;
#define HG_THREAD_RETURN_TYPE hg_thread_ret_t WINAPI
#else
#include <pthread.h>
typedef pthread_t hg_thread_t;
typedef void *(*hg_thread_func_t)(void *);
typedef void         *hg_thread_ret_t;
#define HG_THREAD_RETURN_TYPE hg_thread_ret_t
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize the thread.
 *
 * \param thread [IN/OUT]       pointer to thread object
 */
void hg_thread_init(hg_thread_t *thread);

/**
 * Create a new thread for the given function.
 *
 * \param thread [IN/OUT]       pointer to thread object
 * \param f [IN]                pointer to function
 * \param data [IN]             pointer to data than be passed to function f
 *
 * \return Non-negative on success or negative on failure
 */
int hg_thread_create(hg_thread_t *thread, hg_thread_func_t f, void *data);

/**
 * Ends the calling thread.
 *
 * \param ret [IN]              exit code for the thread
 *
 * \return Non-negative on success or negative on failure
 */
void hg_thread_exit(hg_thread_ret_t ret);

/**
 * Wait for thread completion.
 *
 * \param thread [IN]           thread object
 *
 * \return Non-negative on success or negative on failure
 */
int hg_thread_join(hg_thread_t thread);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_THREAD_H */
