/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_thread_spin.h"

/*---------------------------------------------------------------------------*/
int
hg_thread_spin_init(hg_thread_spin_t *lock)
{
#if defined(_WIN32)
    *lock = 0;

    return HG_UTIL_SUCCESS;
#elif defined(HG_UTIL_HAS_PTHREAD_SPINLOCK_T)
    if (pthread_spin_init(lock, 0))
        return HG_UTIL_FAIL;

    return HG_UTIL_SUCCESS;
#else
    return hg_thread_mutex_init(lock);
#endif
}

/*---------------------------------------------------------------------------*/
int
hg_thread_spin_destroy(hg_thread_spin_t *lock)
{
#if defined(_WIN32)
    (void) lock;

    return HG_UTIL_SUCCESS;
#elif defined(HG_UTIL_HAS_PTHREAD_SPINLOCK_T)
    if (pthread_spin_destroy(lock))
        return HG_UTIL_FAIL;

    return HG_UTIL_SUCCESS;
#else
    return hg_thread_mutex_destroy(lock);
#endif
}
