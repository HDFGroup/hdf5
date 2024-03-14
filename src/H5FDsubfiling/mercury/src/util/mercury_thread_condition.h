/**
 * Copyright (c) 2013-2022 UChicago Argonne, LLC and The HDF Group.
 * Copyright (c) 2022-2023 Intel Corporation.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_THREAD_CONDITION_H
#define MERCURY_THREAD_CONDITION_H

#include "mercury_thread_mutex.h"

#ifdef _WIN32
typedef CONDITION_VARIABLE hg_thread_cond_t;
#else
typedef pthread_cond_t hg_thread_cond_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize the condition.
 *
 * \param cond [IN/OUT]         pointer to condition object
 *
 * \return Non-negative on success or negative on failure
 */
int hg_thread_cond_init(hg_thread_cond_t *cond);

/**
 * Destroy the condition.
 *
 * \param cond [IN/OUT]         pointer to condition object
 *
 * \return Non-negative on success or negative on failure
 */
int hg_thread_cond_destroy(hg_thread_cond_t *cond);

/**
 * Wake one thread waiting for the condition to change.
 *
 * \param cond [IN/OUT]         pointer to condition object
 *
 * \return Non-negative on success or negative on failure
 */
static inline int hg_thread_cond_signal(hg_thread_cond_t *cond);

/**
 * Wake all the threads waiting for the condition to change.
 *
 * \param cond [IN/OUT]         pointer to condition object
 *
 * \return Non-negative on success or negative on failure
 */
static inline int hg_thread_cond_broadcast(hg_thread_cond_t *cond);

/**
 * Wait for the condition to change.
 *
 * \param cond [IN/OUT]         pointer to condition object
 * \param mutex [IN/OUT]        pointer to mutex object
 *
 * \return Non-negative on success or negative on failure
 */
static inline int hg_thread_cond_wait(hg_thread_cond_t *cond, hg_thread_mutex_t *mutex);

/*---------------------------------------------------------------------------*/
static inline int
hg_thread_cond_signal(hg_thread_cond_t *cond)
{
#ifdef _WIN32
    WakeConditionVariable(cond);
#else
    if (pthread_cond_signal(cond))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static inline int
hg_thread_cond_broadcast(hg_thread_cond_t *cond)
{
#ifdef _WIN32
    WakeAllConditionVariable(cond);
#else
    if (pthread_cond_broadcast(cond))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static inline int
hg_thread_cond_wait(hg_thread_cond_t *cond, hg_thread_mutex_t *mutex)
{
#ifdef _WIN32
    if (!SleepConditionVariableCS(cond, mutex, INFINITE))
        return HG_UTIL_FAIL;
#else
    if (pthread_cond_wait(cond, mutex))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_THREAD_CONDITION_H */
