/**
 * Copyright (c) 2013-2022 UChicago Argonne, LLC and The HDF Group.
 * Copyright (c) 2022-2023 Intel Corporation.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_UTIL_ERROR_H
#define MERCURY_UTIL_ERROR_H

#include "mercury_util_config.h"

/* Error macros */

/* Check for cond, set ret to err_val and goto label */
#define HG_UTIL_CHECK_ERROR(cond, label, ret, err_val, ...)                                                  \
    do {                                                                                                     \
        if (H5_UNLIKELY(cond)) {                                                                             \
            ret = err_val;                                                                                   \
            goto label;                                                                                      \
        }                                                                                                    \
    } while (0)

#define HG_UTIL_CHECK_ERROR_NORET(cond, label, ...)                                                          \
    do {                                                                                                     \
        if (H5_UNLIKELY(cond)) {                                                                             \
            goto label;                                                                                      \
        }                                                                                                    \
    } while (0)

#endif /* MERCURY_UTIL_ERROR_H */
