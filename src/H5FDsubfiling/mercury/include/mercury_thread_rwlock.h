/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

/* Copyright (C) 2017 Intel Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted for any purpose (including commercial purposes)
 * provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions, and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions, and the following disclaimer in the
 *    documentation and/or materials provided with the distribution.
 *
 * 3. In addition, redistributions of modified forms of the source or binary
 *    code must carry prominent notices stating that the original code was
 *    changed and the date of the change.
 *
 *  4. All publications or advertising materials mentioning features or use of
 *     this software are asked, but not required, to acknowledge that it was
 *     developed by Intel Corporation and credit the contributors.
 *
 * 5. Neither the name of Intel Corporation, nor the name of any Contributor
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef MERCURY_THREAD_RWLOCK_H
#define MERCURY_THREAD_RWLOCK_H

#include "mercury_util_config.h"

#include "mercury_thread_annotation.h"

#ifdef _WIN32
#include <windows.h>
typedef PSRWLOCK hg_thread_rwlock_t;
#else
#include <pthread.h>
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
HG_UTIL_PUBLIC int hg_thread_rwlock_init(hg_thread_rwlock_t *rwlock);

/**
 * Destroy the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_thread_rwlock_destroy(hg_thread_rwlock_t *rwlock);

/**
 * Take a read lock for the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 */
static HG_UTIL_INLINE void hg_thread_rwlock_rdlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_ACQUIRE_SHARED(*rwlock);

/**
 * Try to take a read lock for the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int hg_thread_rwlock_try_rdlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_TRY_ACQUIRE_SHARED(HG_UTIL_SUCCESS, *rwlock);

/**
 * Release the read lock of the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 */
static HG_UTIL_INLINE void hg_thread_rwlock_release_rdlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_RELEASE_SHARED(*rwlock);

/**
 * Take a write lock for the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 */
static HG_UTIL_INLINE void hg_thread_rwlock_wrlock(hg_thread_rwlock_t *rwlock) HG_LOCK_ACQUIRE(*rwlock);

/**
 * Try to take a write lock for the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int hg_thread_rwlock_try_wrlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_TRY_ACQUIRE(HG_UTIL_SUCCESS, *rwlock);

/**
 * Release the write lock of the rwlock.
 *
 * \param rwlock [IN/OUT]        pointer to rwlock object
 */
static HG_UTIL_INLINE void hg_thread_rwlock_release_wrlock(hg_thread_rwlock_t *rwlock)
    HG_LOCK_RELEASE(*rwlock);

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_thread_rwlock_rdlock(hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    AcquireSRWLockShared(rwlock);
#else
    (void)pthread_rwlock_rdlock(rwlock);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_thread_rwlock_try_rdlock(hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
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
hg_thread_rwlock_release_rdlock(hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    ReleaseSRWLockShared(rwlock);
#else
    (void)pthread_rwlock_unlock(rwlock);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_thread_rwlock_wrlock(hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    ReleaseSRWLockExclusive(rwlock);
#else
    (void)pthread_rwlock_wrlock(rwlock);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_thread_rwlock_try_wrlock(hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
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
hg_thread_rwlock_release_wrlock(hg_thread_rwlock_t *rwlock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#ifdef _WIN32
    ReleaseSRWLockExclusive(rwlock);
#else
    (void)pthread_rwlock_unlock(rwlock);
#endif
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_THREAD_RWLOCK_H */
