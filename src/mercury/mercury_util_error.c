/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_util_error.h"

/*******************/
/* Local Variables */
/*******************/

/* Default error log mask */
#ifdef HG_UTIL_HAS_VERBOSE_ERROR
unsigned int HG_UTIL_LOG_MASK = HG_LOG_TYPE_ERROR | HG_LOG_TYPE_WARNING;
#endif