/**
 * Copyright (c) 2013-2022 UChicago Argonne, LLC and The HDF Group.
 * Copyright (c) 2022-2023 Intel Corporation.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "mercury_thread.h"

#if !defined(_WIN32) && !defined(__APPLE__)
#include <sched.h>
#endif

/*---------------------------------------------------------------------------*/
void
hg_thread_init(hg_thread_t *thread)
{
#ifdef _WIN32
    *thread = NULL;
#else
    *thread = 0;
#endif
}

/*---------------------------------------------------------------------------*/
int
hg_thread_create(hg_thread_t *thread, hg_thread_func_t f, void *data)
{
#ifdef _WIN32
    *thread = CreateThread(NULL, 0, f, data, 0, NULL);
    if (*thread == NULL)
        return HG_UTIL_FAIL;
#else
    if (pthread_create(thread, NULL, f, data))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
void
hg_thread_exit(hg_thread_ret_t ret)
{
#ifdef _WIN32
    ExitThread(ret);
#else
    pthread_exit(ret);
#endif
}

/*---------------------------------------------------------------------------*/
int
hg_thread_join(hg_thread_t thread)
{
#ifdef _WIN32
    WaitForSingleObject(thread, INFINITE);
    CloseHandle(thread);
#else
    if (pthread_join(thread, NULL))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}
