/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_THREAD_SPIN_H
#define MERCURY_THREAD_SPIN_H

#include "mercury_util_config.h"

#include "mercury_thread_annotation.h"

#if defined(_WIN32)
#include <windows.h>
typedef volatile LONG hg_thread_spin_t;
#elif defined(HG_UTIL_HAS_PTHREAD_SPINLOCK_T)
#include <pthread.h>
typedef pthread_spinlock_t HG_LOCK_CAPABILITY("spin") hg_thread_spin_t;
#else
/* Default to hg_thread_mutex_t if pthread_spinlock_t is not supported */
#include "mercury_thread_mutex.h"
typedef hg_thread_mutex_t HG_LOCK_CAPABILITY("mutex") hg_thread_spin_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize the spin lock.
 *
 * \param lock [IN/OUT]         pointer to lock object
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_thread_spin_init(hg_thread_spin_t *lock);

/**
 * Destroy the spin lock.
 *
 * \param lock [IN/OUT]         pointer to lock object
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_thread_spin_destroy(hg_thread_spin_t *lock);

/**
 * Lock the spin lock.
 *
 * \param lock [IN/OUT]         pointer to lock object
 */
static HG_UTIL_INLINE void hg_thread_spin_lock(hg_thread_spin_t *lock) HG_LOCK_ACQUIRE(*lock);

/**
 * Try locking the spin lock.
 *
 * \param mutex [IN/OUT]        pointer to lock object
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int hg_thread_spin_try_lock(hg_thread_spin_t *lock)
    HG_LOCK_TRY_ACQUIRE(HG_UTIL_SUCCESS, *lock);

/**
 * Unlock the spin lock.
 *
 * \param mutex [IN/OUT]        pointer to lock object
 */
static HG_UTIL_INLINE void hg_thread_spin_unlock(hg_thread_spin_t *lock) HG_LOCK_RELEASE(*lock);

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_thread_spin_lock(hg_thread_spin_t *lock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#if defined(_WIN32)
    while (InterlockedExchange(lock, EBUSY)) {
        /* Don't lock while waiting */
        while (*lock) {
            YieldProcessor();

            /* Compiler barrier. Prevent caching of *lock */
            MemoryBarrier();
        }
    }
#elif defined(HG_UTIL_HAS_PTHREAD_SPINLOCK_T)
    (void)pthread_spin_lock(lock);
#else
    hg_thread_mutex_lock(lock);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_thread_spin_try_lock(hg_thread_spin_t *lock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#if defined(_WIN32)
    return InterlockedExchange(lock, EBUSY);
#elif defined(HG_UTIL_HAS_PTHREAD_SPINLOCK_T)
    if (pthread_spin_trylock(lock))
        return HG_UTIL_FAIL;

    return HG_UTIL_SUCCESS;
#else
    return hg_thread_mutex_try_lock(lock);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_thread_spin_unlock(hg_thread_spin_t *lock) HG_LOCK_NO_THREAD_SAFETY_ANALYSIS
{
#if defined(_WIN32)
    /* Compiler barrier. The store below acts with release semantics */
    MemoryBarrier();
    *lock = 0;
#elif defined(HG_UTIL_HAS_PTHREAD_SPINLOCK_T)
    (void)pthread_spin_unlock(lock);
#else
    hg_thread_mutex_unlock(lock);
#endif
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_THREAD_SPIN_H */
