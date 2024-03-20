/**
 * Copyright (c) 2013-2022 UChicago Argonne, LLC and The HDF Group.
 * Copyright (c) 2022-2023 Intel Corporation.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_UTIL_CONFIG_H
#define MERCURY_UTIL_CONFIG_H

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

#include "H5private.h"

/*****************/
/* Public Macros */
/*****************/

/* Return codes */
#define HG_UTIL_SUCCESS 0
#define HG_UTIL_FAIL    -1

#ifdef _WIN32
#define _WINSOCKAPI_
#include <windows.h>
typedef DWORD hg_thread_ret_t;
#define HG_THREAD_RETURN_TYPE hg_thread_ret_t WINAPI
#else
#include <pthread.h>
typedef void *hg_thread_ret_t;
#define HG_THREAD_RETURN_TYPE hg_thread_ret_t
#endif

#endif /* MERCURY_UTIL_CONFIG_H */
