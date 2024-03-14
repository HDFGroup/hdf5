/**
 * Copyright (c) 2013-2022 UChicago Argonne, LLC and The HDF Group.
 * Copyright (c) 2022-2023 Intel Corporation.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "mercury_thread_condition.h"

/*---------------------------------------------------------------------------*/
int
hg_thread_cond_init(hg_thread_cond_t *cond)
{
#ifdef _WIN32
    InitializeConditionVariable(cond);
#else
    if (pthread_cond_init(cond, NULL))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
int
hg_thread_cond_destroy(hg_thread_cond_t *cond)
{
#ifndef _WIN32
    if (pthread_cond_destroy(cond))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}
