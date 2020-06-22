/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_LOG_H
#define MERCURY_LOG_H

#include "mercury_util_config.h"

#include <stdio.h>

#define HG_LOG_TYPE_NONE    0
#define HG_LOG_TYPE_DEBUG   0x01
#define HG_LOG_TYPE_WARNING 0x02
#define HG_LOG_TYPE_ERROR   0x04

/* For compatibility */
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ < 199901L)
#    if defined(__GNUC__) && (__GNUC__ >= 2)
#        define __func__ __FUNCTION__
#    else
#        define __func__ "<unknown>"
#    endif
#elif defined(_WIN32)
#    define __func__ __FUNCTION__
#endif

#define HG_LOG_WRITE_ERROR(HG_LOG_MODULE_NAME, ...)                            \
    do {                                                                       \
        hg_log_write(HG_LOG_TYPE_ERROR, HG_LOG_MODULE_NAME, __FILE__,          \
            __LINE__, __func__, __VA_ARGS__);                                  \
    } while (0)
#define HG_LOG_WRITE_DEBUG(HG_LOG_MODULE_NAME, ...)                            \
    do {                                                                       \
        hg_log_write(HG_LOG_TYPE_DEBUG, HG_LOG_MODULE_NAME, __FILE__,          \
            __LINE__, __func__, __VA_ARGS__);                                  \
    } while (0)
#define HG_LOG_WRITE_WARNING(HG_LOG_MODULE_NAME, ...)                          \
    do {                                                                       \
        hg_log_write(HG_LOG_TYPE_WARNING, HG_LOG_MODULE_NAME, __FILE__,        \
            __LINE__, __func__, __VA_ARGS__);                                  \
    } while (0)

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Set the logging function.
 *
 * \param log_func [IN]         pointer to function
 */
HG_UTIL_PUBLIC void
hg_log_set_func(int (*log_func)(FILE *stream, const char *format, ...));

/**
 * Set the stream for debug output.
 *
 * \param stream [IN/OUT]       pointer to stream
 */
HG_UTIL_PUBLIC void
hg_log_set_stream_debug(FILE *stream);

/**
 * Set the stream for warning output.
 *
 * \param stream [IN/OUT]       pointer to stream
 */
HG_UTIL_PUBLIC void
hg_log_set_stream_warning(FILE *stream);

/**
 * Set the stream for error output.
 *
 * \param stream [IN/OUT]       pointer to stream
 */
HG_UTIL_PUBLIC void
hg_log_set_stream_error(FILE *stream);

/**
 * Write log.
 *
 * \param log_type [IN]         log type (HG_LOG_TYPE_DEBUG, etc)
 * \param module [IN]           module name
 * \param file [IN]             file name
 * \param line [IN]             line number
 * \param func [IN]             function name
 * \param format [IN]           string format
 */
HG_UTIL_PUBLIC void
hg_log_write(unsigned int log_type, const char *module, const char *file,
    unsigned int line, const char *func, const char *format, ...);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_LOG_H */
