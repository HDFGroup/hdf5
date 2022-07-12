/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_THREAD_RWLOCK_H
#define MERCURY_THREAD_RWLOCK_H

#include "mercury_util_config.h"

#include "mercury_thread_annotation.h"

#ifdef _WIN32
#    define _WINSOCKAPI_
#    include <windows.h>
typedef SRWLOCK hg_thread_rwlock_t;
#else
#    include <pthread.h>
typedef pthread_rwlock_t HG_LOCK_CAPABILITY("rwlock") hg_thread_rwlock_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_thread_rwlock_init(hg_thread_rwlock_t *rwlock);

/**
 * Destroy the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_thread_rwlock_destroy(hg_thread_rwlock_t *rwlock);

/**
 * Take a read lock for the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 */
static HG_UTIL_INLINE void
hg_thread_rwlock_rdlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_ACQUIRE_SHARED(*rwlock);

/**
 * Try to take a read lock for the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int
hg_thread_rwlock_try_rdlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_TRY_ACQUIRE_SHARED(HG_UTIL_SUCCESS, *rwlock);

/**
 * Release the read lock of the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 */
static HG_UTIL_INLINE void
hg_thread_rwlock_release_rdlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_RELEASE_SHARED(*rwlock);

/**
 * Take a write lock for the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 */
static HG_UTIL_INLINE void
hg_thread_rwlock_wrlock(hg_thread_rwlock_t *rwlock) HG_LOCK_ACQUIRE(*rwlock);

/**
 * Try to take a write lock for the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int
hg_thread_rwlock_try_wrlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_TRY_ACQUIRE(HG_UTIL_SUCCESS, *rwlock);

/**
 * Release the write lock of the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 */
static HG_UTIL_INLINE void
hg_thread_rwlock_release_wrlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_RELEASE(*rwlock);

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_thread_rwlock_rdlock(
    hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    AcquireSRWLockShared(rwlock);
#else
    (void) pthread_rwlock_rdlock(rwlock);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_thread_rwlock_try_rdlock(
    hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    if (TryAcquireSRWLockShared(rwlock) == 0)
        return HG_UTIL_FAIL;
#else
    if (pthread_rwlock_tryrdlock(rwlock))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_thread_rwlock_release_rdlock(
    hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    ReleaseSRWLockShared(rwlock);
#else
    (void) pthread_rwlock_unlock(rwlock);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_thread_rwlock_wrlock(
    hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    ReleaseSRWLockExclusive(rwlock);
#else
    (void) pthread_rwlock_wrlock(rwlock);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_thread_rwlock_try_wrlock(
    hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    if (TryAcquireSRWLockExclusive(rwlock) == 0)
        return HG_UTIL_FAIL;
#else
    if (pthread_rwlock_trywrlock(rwlock))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_thread_rwlock_release_wrlock(
    hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    ReleaseSRWLockExclusive(rwlock);
#else
    (void) pthread_rwlock_unlock(rwlock);
#endif
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_THREAD_RWLOCK_H */
