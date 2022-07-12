/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "mercury_thread_rwlock.h"

#include "mercury_util_error.h"

#include <string.h>

/*---------------------------------------------------------------------------*/
int
hg_thread_rwlock_init(hg_thread_rwlock_t *rwlock)
{
    int ret = HG_UTIL_SUCCESS;

#ifdef _WIN32
    InitializeSRWLock(rwlock);
#else
    int rc = pthread_rwlock_init(rwlock, NULL);
    HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "pthread_rwlock_init() failed (%s)", strerror(rc));

done:
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
int
hg_thread_rwlock_destroy(hg_thread_rwlock_t *rwlock)
{
    int ret = HG_UTIL_SUCCESS;

#ifdef _WIN32
    /* nothing to do */
#else
    int rc = pthread_rwlock_destroy(rwlock);
    HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "pthread_rwlock_destroy() failed (%s)",
                        strerror(rc));

done:
#endif

    return ret;
}
