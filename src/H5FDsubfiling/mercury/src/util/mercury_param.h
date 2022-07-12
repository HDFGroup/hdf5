/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_PARAM_H
#define MERCURY_PARAM_H

#include "mercury_util_config.h"

#ifdef HG_UTIL_HAS_SYSPARAM_H
#include <sys/param.h>
#endif

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/*****************/
/* Public Macros */
/*****************/

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

#ifndef powerof2
#define powerof2(x) ((((x)-1) & (x)) == 0)
#endif

/*********************/
/* Public Prototypes */
/*********************/

#endif /* MERCURY_PARAM_H */
