/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "mercury_thread_spin.h"

#include "mercury_util_error.h"

#include <string.h>

/*---------------------------------------------------------------------------*/
int
hg_thread_spin_init(hg_thread_spin_t *lock)
{
    int ret = HG_UTIL_SUCCESS;

#if defined(_WIN32)
    *lock = 0;
#elif defined(HG_UTIL_HAS_PTHREAD_SPINLOCK_T)
    int rc = pthread_spin_init(lock, 0);
    HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "pthread_spin_init() failed (%s)", strerror(rc));

done:
#else
    ret = hg_thread_mutex_init_fast(lock);
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
int
hg_thread_spin_destroy(hg_thread_spin_t *lock)
{
    int ret = HG_UTIL_SUCCESS;

#if defined(_WIN32)
    (void)lock;
#elif defined(HG_UTIL_HAS_PTHREAD_SPINLOCK_T)
    int rc = pthread_spin_destroy(lock);
    HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "pthread_spin_destroy() failed (%s)", strerror(rc));

done:
#else
    ret = hg_thread_mutex_destroy(lock);
#endif

    return ret;
}
