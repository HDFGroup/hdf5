/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_THREAD_MUTEX_H
#define MERCURY_THREAD_MUTEX_H

#include "mercury_util_config.h"

#ifdef _WIN32
#    include <windows.h>
#    define HG_THREAD_MUTEX_INITIALIZER NULL
typedef CRITICAL_SECTION hg_thread_mutex_t;
#else
#    include <pthread.h>
#    define HG_THREAD_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER
typedef pthread_mutex_t hg_thread_mutex_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize the mutex.
 *
 * \param mutex [IN/OUT]        pointer to mutex object
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_thread_mutex_init(hg_thread_mutex_t *mutex);

/**
 * Destroy the mutex.
 *
 * \param mutex [IN/OUT]        pointer to mutex object
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_thread_mutex_destroy(hg_thread_mutex_t *mutex);

/**
 * Lock the mutex.
 *
 * \param mutex [IN/OUT]        pointer to mutex object
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int
hg_thread_mutex_lock(hg_thread_mutex_t *mutex);

/**
 * Try locking the mutex.
 *
 * \param mutex [IN/OUT]        pointer to mutex object
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int
hg_thread_mutex_try_lock(hg_thread_mutex_t *mutex);

/**
 * Unlock the mutex.
 *
 * \param mutex [IN/OUT]        pointer to mutex object
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int
hg_thread_mutex_unlock(hg_thread_mutex_t *mutex);

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_thread_mutex_lock(hg_thread_mutex_t *mutex)
{
#ifdef _WIN32
    EnterCriticalSection(mutex);
#else
    if (pthread_mutex_lock(mutex))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_thread_mutex_try_lock(hg_thread_mutex_t *mutex)
{
#ifdef _WIN32
    if (!TryEnterCriticalSection(mutex))
        return HG_UTIL_FAIL;
#else
    if (pthread_mutex_trylock(mutex))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_thread_mutex_unlock(hg_thread_mutex_t *mutex)
{
#ifdef _WIN32
    LeaveCriticalSection(mutex);
#else
    if (pthread_mutex_unlock(mutex))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_THREAD_MUTEX_H */
