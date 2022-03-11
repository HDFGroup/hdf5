/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
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
HG_Util_set_log_level(const char *level)
{
    hg_log_set_subsys_level(HG_UTIL_SUBSYS_NAME_STRING, hg_log_name_to_level(level));
}
