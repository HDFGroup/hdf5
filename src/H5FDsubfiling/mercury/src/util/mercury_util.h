/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_UTIL_LOG_H
#define MERCURY_UTIL_LOG_H

#include "mercury_util_config.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/*****************/
/* Public Macros */
/*****************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Get HG util version number.
 *
 * \param major [OUT]           pointer to unsigned integer
 * \param minor [OUT]           pointer to unsigned integer
 * \param patch [OUT]           pointer to unsigned integer
 */
HG_UTIL_PUBLIC void HG_Util_version_get(unsigned int *major, unsigned int *minor, unsigned int *patch);

/**
 * Set the log level for HG util. That setting is valid for all HG classes.
 *
 * \param level [IN]            level string, valid values are:
 *                                "none", "error", "warning", "debug"
 */
HG_UTIL_PUBLIC void HG_Util_set_log_level(const char *level);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_UTIL_LOG_H */
