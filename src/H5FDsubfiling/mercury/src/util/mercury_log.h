/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_LOG_H
#define MERCURY_LOG_H

#include "mercury_util_config.h"

#include "mercury_dlog.h"
#include "mercury_queue.h"

#include <stdio.h>

/*****************/
/* Public Macros */
/*****************/

/* For compatibility */
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ < 199901L)
#if defined(__GNUC__) && (__GNUC__ >= 2)
#define __func__ __FUNCTION__
#else
#define __func__ "<unknown>"
#endif
#elif defined(_WIN32)
#define __func__ __FUNCTION__
#endif

/* Cat macro */
#define HG_UTIL_CAT(x, y) x##y

/* Stringify macro */
#define HG_UTIL_STRINGIFY(x) #x

/* Constructor (used to initialize log outlets) */
#define HG_UTIL_CONSTRUCTOR HG_ATTR_CONSTRUCTOR

/* Available log levels, additional log levels should be added to that list by
 * order of verbosity. Format is:
 * - enum type
 * - level name
 * - default output
 *
 * error: print error level logs
 * warning: print warning level logs
 * min_debug: store minimal debug information and defer printing until error
 * debug: print debug level logs
 */
#define HG_LOG_LEVELS                                                                                        \
    X(HG_LOG_LEVEL_NONE, "", NULL)                  /*!< no log */                                           \
    X(HG_LOG_LEVEL_ERROR, "error", &stderr)         /*!< error log type */                                   \
    X(HG_LOG_LEVEL_WARNING, "warning", &stdout)     /*!< warning log type */                                 \
    X(HG_LOG_LEVEL_MIN_DEBUG, "min_debug", &stdout) /*!< debug log type */                                   \
    X(HG_LOG_LEVEL_DEBUG, "debug", &stdout)         /*!< debug log type */                                   \
    X(HG_LOG_LEVEL_MAX, "", NULL)

/* HG_LOG_OUTLET: global variable name of log outlet. */
#define HG_LOG_OUTLET(name) HG_UTIL_CAT(name, _log_outlet_g)

/* HG_LOG_OUTLET_DECL: declare an outlet. */
#define HG_LOG_OUTLET_DECL(name) struct hg_log_outlet HG_LOG_OUTLET(name)

/*
 * HG_LOG_OUTLET_INITIALIZER: initializer for a log in a global variable.
 * (parent and debug_log are optional and can be set to NULL)
 */
#define HG_LOG_OUTLET_INITIALIZER(name, state, parent, debug_log)                                            \
    {                                                                                                        \
        HG_UTIL_STRINGIFY(name), state, HG_LOG_LEVEL_NONE, parent, debug_log,                                \
        {                                                                                                    \
            NULL                                                                                             \
        }                                                                                                    \
    }

/* HG_LOG_OUTLET_SUBSYS_INITIALIZER: initializer for a sub-system log. */
#define HG_LOG_OUTLET_SUBSYS_INITIALIZER(name, parent_name)                                                  \
    HG_LOG_OUTLET_INITIALIZER(name, HG_LOG_PASS, &HG_LOG_OUTLET(parent_name), NULL)

/* HG_LOG_OUTLET_SUBSYS_STATE_INITIALIZER: initializer for a sub-system log with
 * a defined state. */
#define HG_LOG_OUTLET_SUBSYS_STATE_INITIALIZER(name, parent_name, state)                                     \
    HG_LOG_OUTLET_INITIALIZER(name, state, &HG_LOG_OUTLET(parent_name), NULL)

/* HG_LOG_SUBSYS_REGISTER: register a name */
#define HG_LOG_SUBSYS_REGISTER(name)                                                                         \
    static void HG_UTIL_CAT(hg_log_outlet_, name)(void) HG_UTIL_CONSTRUCTOR;                                 \
    static void HG_UTIL_CAT(hg_log_outlet_, name)(void)                                                      \
    {                                                                                                        \
        hg_log_outlet_register(&HG_LOG_OUTLET(name));                                                        \
    }                                                                                                        \
    /* Keep unused prototype to use semicolon at end of macro */                                             \
    void hg_log_outlet_##name##_unused(void)

/* HG_LOG_SUBSYS_DECL_REGISTER: declare and register a log outlet. */
#define HG_LOG_SUBSYS_DECL_REGISTER(name, parent_name)                                                       \
    struct hg_log_outlet HG_LOG_OUTLET(name) = HG_LOG_OUTLET_SUBSYS_INITIALIZER(name, parent_name);          \
    HG_LOG_SUBSYS_REGISTER(name)

/* HG_LOG_SUBSYS_DECL_STATE_REGISTER: declare and register a log outlet and
 * enforce an init state. */
#define HG_LOG_SUBSYS_DECL_STATE_REGISTER(name, parent_name, state)                                          \
    struct hg_log_outlet HG_LOG_OUTLET(name) =                                                               \
        HG_LOG_OUTLET_SUBSYS_STATE_INITIALIZER(name, parent_name, state);                                    \
    HG_LOG_SUBSYS_REGISTER(name)

/* Log macro */
#define HG_LOG_WRITE(name, log_level, ...)                                                                   \
    do {                                                                                                     \
        if (log_level == HG_LOG_LEVEL_DEBUG && HG_LOG_OUTLET(name).level >= HG_LOG_LEVEL_MIN_DEBUG &&        \
            HG_LOG_OUTLET(name).debug_log)                                                                   \
            hg_dlog_addlog(HG_LOG_OUTLET(name).debug_log, __FILE__, __LINE__, __func__, NULL, NULL);         \
        if (HG_LOG_OUTLET(name).level >= log_level)                                                          \
            hg_log_write(&HG_LOG_OUTLET(name), log_level, __FILE__, __LINE__, __func__, __VA_ARGS__);        \
    } while (0)

#define HG_LOG_WRITE_DEBUG_EXT(name, header, ...)                                                            \
    do {                                                                                                     \
        if (HG_LOG_OUTLET(name).level == HG_LOG_LEVEL_DEBUG) {                                               \
            hg_log_func_t log_func = hg_log_get_func();                                                      \
            hg_log_write(&HG_LOG_OUTLET(name), HG_LOG_LEVEL_DEBUG, __FILE__, __LINE__, __func__, header);    \
            log_func(hg_log_get_stream_debug(), __VA_ARGS__);                                                \
            log_func(hg_log_get_stream_debug(), "---\n");                                                    \
        }                                                                                                    \
    } while (0)

/**
 * Additional macros for debug log support.
 */

/* HG_LOG_DEBUG_DLOG: global variable name of debug log. */
#define HG_LOG_DEBUG_DLOG(name) HG_UTIL_CAT(name, _dlog_g)

/* HG_LOG_DEBUG_LE: global variable name of debug log entries. */
#define HG_LOG_DEBUG_LE(name) HG_UTIL_CAT(name, _dlog_entries_g)

/* HG_LOG_DEBUG_DECL_DLOG: declare new debug log. */
#define HG_LOG_DEBUG_DECL_DLOG(name) struct hg_dlog HG_LOG_DEBUG_DLOG(name)

/* HG_LOG_DEBUG_DECL_LE: declare array of debug log entries. */
#define HG_LOG_DEBUG_DECL_LE(name, size) struct hg_dlog_entry HG_LOG_DEBUG_LE(name)[size]

/* HG_LOG_DLOG_INITIALIZER: initializer for a debug log */
#define HG_LOG_DLOG_INITIALIZER(name, size)                                                                  \
    HG_DLOG_INITIALIZER(HG_UTIL_STRINGIFY(name), HG_LOG_DEBUG_LE(name), size, 1)

/* HG_LOG_OUTLET_SUBSYS_DLOG_INITIALIZER: initializer for a sub-system with
 * debug log. */
#define HG_LOG_OUTLET_SUBSYS_DLOG_INITIALIZER(name, parent_name)                                             \
    HG_LOG_OUTLET_INITIALIZER(name, HG_LOG_PASS, &HG_LOG_OUTLET(parent_name), &HG_LOG_DEBUG_DLOG(name))

/* HG_LOG_SUBSYS_DLOG_DECL_REGISTER: declare and register a log outlet with
 * debug log. */
#define HG_LOG_SUBSYS_DLOG_DECL_REGISTER(name, parent_name)                                                  \
    struct hg_log_outlet HG_LOG_OUTLET(name) = HG_LOG_OUTLET_SUBSYS_DLOG_INITIALIZER(name, parent_name);     \
    HG_LOG_SUBSYS_REGISTER(name)

/* HG_LOG_ADD_COUNTER32: add 32-bit debug log counter */
#define HG_LOG_ADD_COUNTER32(name, counter_ptr, counter_name, counter_desc)                                  \
    hg_dlog_mkcount32(HG_LOG_OUTLET(name).debug_log, counter_ptr, counter_name, counter_desc)

/* HG_LOG_ADD_COUNTER64: add 64-bit debug log counter */
#define HG_LOG_ADD_COUNTER64(name, counter_ptr, counter_name, counter_desc)                                  \
    hg_dlog_mkcount64(HG_LOG_OUTLET(name).debug_log, counter_ptr, counter_name, counter_desc)

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

#define X(a, b, c) a,
/* Log levels */
enum hg_log_level { HG_LOG_LEVELS };
#undef X

/* Log states */
enum hg_log_state { HG_LOG_PASS, HG_LOG_OFF, HG_LOG_ON };

/* Log outlet */
struct hg_log_outlet {
    const char           *name;          /* Name of outlet */
    enum hg_log_state     state;         /* Init state of outlet */
    enum hg_log_level     level;         /* Level of outlet */
    struct hg_log_outlet *parent;        /* Parent of outlet */
    struct hg_dlog       *debug_log;     /* Debug log to use */
    HG_QUEUE_ENTRY(hg_log_outlet) entry; /* List entry */
};

/* Log function */
typedef int (*hg_log_func_t)(FILE *stream, const char *format, ...);

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Set the global log level.
 *
 * \param log_level [IN]        enum log level type
 */
HG_UTIL_PUBLIC void hg_log_set_level(enum hg_log_level log_level);

/**
 * Get the global log level.
 *
 * \return global log_level
 */
HG_UTIL_PUBLIC enum hg_log_level hg_log_get_level(void);

/**
 * Set the log subsystems from a string. Format is: subsys1,subsys2,...
 * Subsys can also be forced to be disabled with "~", e.g., ~subsys1
 *
 * \param log_level [IN]        null terminated string
 */
HG_UTIL_PUBLIC void hg_log_set_subsys(const char *log_subsys);

/**
 * Get the log subsystems as a string. Format is similar to hg_log_set_subsys().
 * Buffer returned is static.
 *
 * \return string of enabled log subsystems
 */
HG_UTIL_PUBLIC const char *hg_log_get_subsys(void);

/**
 * Set a specific subsystem's log level.
 */
HG_UTIL_PUBLIC void hg_log_set_subsys_level(const char *subsys, enum hg_log_level log_level);

/**
 * Get the log level from a string.
 *
 * \param log_level [IN]        null terminated string
 *
 * \return log type enum value
 */
HG_UTIL_PUBLIC enum hg_log_level hg_log_name_to_level(const char *log_level);

/**
 * Set the logging function.
 *
 * \param log_func [IN]         pointer to function
 */
HG_UTIL_PUBLIC void hg_log_set_func(hg_log_func_t log_func);

/**
 * Get the logging function.
 *
 * \return pointer pointer to function
 */
HG_UTIL_PUBLIC hg_log_func_t hg_log_get_func(void);

/**
 * Set the stream for error output.
 *
 * \param stream [IN/OUT]       pointer to stream
 */
HG_UTIL_PUBLIC void hg_log_set_stream_error(FILE *stream);

/**
 * Get the stream for error output.
 *
 * \return pointer to stream
 */
HG_UTIL_PUBLIC FILE *hg_log_get_stream_error(void);

/**
 * Set the stream for warning output.
 *
 * \param stream [IN/OUT]       pointer to stream
 */
HG_UTIL_PUBLIC void hg_log_set_stream_warning(FILE *stream);

/**
 * Get the stream for warning output.
 *
 * \return pointer to stream
 */
HG_UTIL_PUBLIC FILE *hg_log_get_stream_warning(void);

/**
 * Set the stream for debug output.
 *
 * \param stream [IN/OUT]       pointer to stream
 */
HG_UTIL_PUBLIC void hg_log_set_stream_debug(FILE *stream);

/**
 * Get the stream for debug output.
 *
 * \return pointer to stream
 */
HG_UTIL_PUBLIC FILE *hg_log_get_stream_debug(void);

/**
 * Register log outlet.
 *
 * \param outlet [IN]           log outlet
 */
HG_UTIL_PUBLIC void hg_log_outlet_register(struct hg_log_outlet *outlet);

/**
 * Write log.
 *
 * \param outlet [IN]           log outlet
 * \param log_level [IN]        log level
 * \param file [IN]             file name
 * \param line [IN]             line number
 * \param func [IN]             function name
 * \param format [IN]           string format
 */
HG_UTIL_PUBLIC void hg_log_write(struct hg_log_outlet *outlet, enum hg_log_level log_level, const char *file,
                                 unsigned int line, const char *func, const char *format, ...)
    HG_UTIL_PRINTF(6, 7);

/*********************/
/* Public Variables */
/*********************/

/* Top error outlet */
extern HG_UTIL_PUBLIC HG_LOG_OUTLET_DECL(hg);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_LOG_H */
