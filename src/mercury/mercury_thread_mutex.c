/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_thread_mutex.h"

/*---------------------------------------------------------------------------*/
int
hg_thread_mutex_init(hg_thread_mutex_t *mutex)
{
#ifdef _WIN32
    InitializeCriticalSection(mutex);
#else
    pthread_mutexattr_t mutex_attr;

    pthread_mutexattr_init(&mutex_attr);
#    ifdef HG_UTIL_HAS_PTHREAD_MUTEX_ADAPTIVE_NP
    /* Set type to PTHREAD_MUTEX_ADAPTIVE_NP to improve performance */
    pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_ADAPTIVE_NP);
#    else
    pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_DEFAULT);
#    endif
    if (pthread_mutex_init(mutex, &mutex_attr))
        return HG_UTIL_FAIL;

    pthread_mutexattr_destroy(&mutex_attr);
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
int
hg_thread_mutex_destroy(hg_thread_mutex_t *mutex)
{
#ifdef _WIN32
    DeleteCriticalSection(mutex);
#else
    if (pthread_mutex_destroy(mutex))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}
