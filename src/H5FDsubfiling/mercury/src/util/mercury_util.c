/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "mercury_util.h"

#include "mercury_util_error.h"

#include <stdlib.h>
#include <string.h>

/****************/
/* Local Macros */
/****************/

/* Name of this subsystem */
#define HG_UTIL_SUBSYS_NAME        hg_util
#define HG_UTIL_STRINGIFY1(x)      HG_UTIL_STRINGIFY(x)
#define HG_UTIL_SUBSYS_NAME_STRING HG_UTIL_STRINGIFY1(HG_UTIL_SUBSYS_NAME)

/*******************/
/* Local Variables */
/*******************/

/* Default error log mask */
HG_LOG_SUBSYS_DECL_REGISTER(HG_UTIL_SUBSYS_NAME, hg);

/*---------------------------------------------------------------------------*/
void
HG_Util_version_get(unsigned int *major, unsigned int *minor, unsigned int *patch)
{
    if (major)
        *major = HG_UTIL_VERSION_MAJOR;
    if (minor)
        *minor = HG_UTIL_VERSION_MINOR;
    if (patch)
        *patch = HG_UTIL_VERSION_PATCH;
}

/*---------------------------------------------------------------------------*/
void
HG_Util_set_log_level(const char *level)
{
    hg_log_set_subsys_level(HG_UTIL_SUBSYS_NAME_STRING, hg_log_name_to_level(level));
}
