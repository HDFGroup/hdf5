/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
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
