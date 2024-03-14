/**
 * Copyright (c) 2013-2022 UChicago Argonne, LLC and The HDF Group.
 * Copyright (c) 2022-2023 Intel Corporation.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "mercury_thread_mutex.h"

#include "mercury_util_error.h"

#include <string.h>

/*---------------------------------------------------------------------------*/
int
hg_thread_mutex_init(hg_thread_mutex_t *mutex)
{
    int ret = HG_UTIL_SUCCESS;

#ifdef _WIN32
    InitializeCriticalSection(mutex);
#else
    int rc;

    rc = pthread_mutex_init(mutex, NULL);
    HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "pthread_mutex_init() failed (%s)",
                        strerror(rc));

done:
#endif
    return ret;
}

/*---------------------------------------------------------------------------*/
int
hg_thread_mutex_destroy(hg_thread_mutex_t *mutex)
{
    int ret = HG_UTIL_SUCCESS;

#ifdef _WIN32
    DeleteCriticalSection(mutex);
#else
    int rc;

    rc = pthread_mutex_destroy(mutex);
    HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "pthread_mutex_destroy() failed (%s)",
                        strerror(rc));

done:
#endif
    return ret;
}
