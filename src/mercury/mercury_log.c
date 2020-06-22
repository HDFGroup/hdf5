/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_log.h"

#include <stdarg.h>

/****************/
/* Local Macros */
/****************/

#define HG_LOG_MAX_BUF 256

#ifdef HG_UTIL_HAS_LOG_COLOR
#    define HG_LOG_ESC     "\033"
#    define HG_LOG_RESET   HG_LOG_ESC "[0m"
#    define HG_LOG_REG     HG_LOG_ESC "[0;"
#    define HG_LOG_BOLD    HG_LOG_ESC "[1;"
#    define HG_LOG_RED     "31m"
#    define HG_LOG_GREEN   "32m"
#    define HG_LOG_YELLOW  "33m"
#    define HG_LOG_BLUE    "34m"
#    define HG_LOG_MAGENTA "35m"
#    define HG_LOG_CYAN    "36m"
#endif

/*******************/
/* Local Variables */
/*******************/

static int (*hg_log_func_g)(FILE *stream, const char *format, ...) = fprintf;
static FILE *hg_log_stream_debug_g = NULL;
static FILE *hg_log_stream_warning_g = NULL;
static FILE *hg_log_stream_error_g = NULL;

/*---------------------------------------------------------------------------*/
void
hg_log_set_func(int (*log_func)(FILE *stream, const char *format, ...))
{
    hg_log_func_g = log_func;
}

/*---------------------------------------------------------------------------*/
void
hg_log_set_stream_debug(FILE *stream)
{
    hg_log_stream_debug_g = stream;
}

/*---------------------------------------------------------------------------*/
void
hg_log_set_stream_warning(FILE *stream)
{
    hg_log_stream_warning_g = stream;
}

/*---------------------------------------------------------------------------*/
void
hg_log_set_stream_error(FILE *stream)
{
    hg_log_stream_error_g = stream;
}

/*---------------------------------------------------------------------------*/
void
hg_log_write(unsigned int log_type, const char *module, const char *file,
    unsigned int line, const char *func, const char *format, ...)
{
    char buf[HG_LOG_MAX_BUF];
    FILE *stream = NULL;
    const char *msg_type = NULL;
#ifdef HG_UTIL_HAS_LOG_COLOR
    const char *color = "";
#endif
    va_list ap;

    switch (log_type) {
        case HG_LOG_TYPE_DEBUG:
#ifdef HG_UTIL_HAS_LOG_COLOR
            color = HG_LOG_BLUE;
#endif
            stream = hg_log_stream_debug_g ? hg_log_stream_debug_g : stdout;
            msg_type = "Debug";
            break;
        case HG_LOG_TYPE_WARNING:
#ifdef HG_UTIL_HAS_LOG_COLOR
            color = HG_LOG_MAGENTA;
#endif
            stream = hg_log_stream_warning_g ? hg_log_stream_warning_g : stdout;
            msg_type = "Warning";
            break;
        case HG_LOG_TYPE_ERROR:
#ifdef HG_UTIL_HAS_LOG_COLOR
            color = HG_LOG_RED;
#endif
            stream = hg_log_stream_error_g ? hg_log_stream_error_g : stderr;
            msg_type = "Error";
            break;
        default:
            return;
    };

    va_start(ap, format);
    vsnprintf(buf, HG_LOG_MAX_BUF, format, ap);
    va_end(ap);

/* Print using logging function */
#ifdef HG_UTIL_HAS_LOG_COLOR
    hg_log_func_g(stream,
        "# %s%s[%s -- %s%s%s%s%s -- %s:%d]%s\n"
        "##    %s%s%s()%s: %s\n",
        HG_LOG_REG, color, module, HG_LOG_BOLD, color, msg_type, HG_LOG_REG,
        color, file, line, HG_LOG_RESET, HG_LOG_REG, HG_LOG_YELLOW, func,
        HG_LOG_RESET, buf);
#else
    hg_log_func_g(stream,
        "# %s -- %s -- %s:%d\n"
        " # %s(): %s\n",
        module, msg_type, file, line, func, buf);
#endif
}
