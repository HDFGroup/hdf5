/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_TIME_H
#define MERCURY_TIME_H

#include "mercury_util_config.h"

#if defined(_WIN32)
#    include <windows.h>
#elif defined(HG_UTIL_HAS_CLOCK_MONOTONIC)
#    if defined(HG_UTIL_HAS_TIME_H) && defined(HG_UTIL_HAS_CLOCK_GETTIME)
#        include <time.h>
#    elif defined(__APPLE__) && defined(HG_UTIL_HAS_SYSTIME_H)
#        include <mach/mach_time.h>
#        include <sys/time.h>
#    else
#        error "Not supported on this platform."
#    endif
#else
#    include <stdio.h>
#    include <unistd.h>
#    if defined(HG_UTIL_HAS_SYSTIME_H)
#        include <sys/time.h>
#    else
#        error "Not supported on this platform."
#    endif
#endif

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

typedef struct hg_time hg_time_t;

struct hg_time {
    long tv_sec;
    long tv_usec;
};

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
 * Get an elapsed time on the calling processor.
 *
 * \param tv [OUT]              pointer to returned time structure
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int
hg_time_get_current(hg_time_t *tv);

/**
 * Convert hg_time_t to double.
 *
 * \param tv [IN]               time structure
 *
 * \return Converted time in seconds
 */
static HG_UTIL_INLINE double
hg_time_to_double(hg_time_t tv);

/**
 * Convert double to hg_time_t.
 *
 * \param d [IN]                time in seconds
 *
 * \return Converted time structure
 */
static HG_UTIL_INLINE hg_time_t
hg_time_from_double(double d);

/**
 * Compare time values.
 *
 * \param in1 [IN]              time structure
 * \param in2 [IN]              time structure
 *
 * \return 1 if in1 < in2, 0 otherwise
 */
static HG_UTIL_INLINE int
hg_time_less(hg_time_t in1, hg_time_t in2);

/**
 * Add time values.
 *
 * \param in1 [IN]              time structure
 * \param in2 [IN]              time structure
 *
 * \return Summed time structure
 */
static HG_UTIL_INLINE hg_time_t
hg_time_add(hg_time_t in1, hg_time_t in2);

/**
 * Subtract time values.
 *
 * \param in1 [IN]              time structure
 * \param in2 [IN]              time structure
 *
 * \return Subtracted time structure
 */
static HG_UTIL_INLINE hg_time_t
hg_time_subtract(hg_time_t in1, hg_time_t in2);

/**
 * Sleep until the time specified in rqt has elapsed.
 *
 * \param reqt [IN]             time structure
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int
hg_time_sleep(const hg_time_t rqt);

/**
 * Get a string containing current time/date stamp.
 *
 * \return Valid string or NULL on failure
 */
static HG_UTIL_INLINE char *
hg_time_stamp(void);

/*---------------------------------------------------------------------------*/
#ifdef _WIN32
static HG_UTIL_INLINE LARGE_INTEGER
get_FILETIME_offset(void)
{
    SYSTEMTIME s;
    FILETIME f;
    LARGE_INTEGER t;

    s.wYear = 1970;
    s.wMonth = 1;
    s.wDay = 1;
    s.wHour = 0;
    s.wMinute = 0;
    s.wSecond = 0;
    s.wMilliseconds = 0;
    SystemTimeToFileTime(&s, &f);
    t.QuadPart = f.dwHighDateTime;
    t.QuadPart <<= 32;
    t.QuadPart |= f.dwLowDateTime;

    return t;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_time_get_current(hg_time_t *tv)
{
    LARGE_INTEGER t;
    FILETIME f;
    double t_usec;
    static LARGE_INTEGER offset;
    static double freq_to_usec;
    static int initialized = 0;
    static BOOL use_perf_counter = 0;

    if (!tv)
        return HG_UTIL_FAIL;

    if (!initialized) {
        LARGE_INTEGER perf_freq;
        initialized = 1;
        use_perf_counter = QueryPerformanceFrequency(&perf_freq);
        if (use_perf_counter) {
            QueryPerformanceCounter(&offset);
            freq_to_usec = (double) perf_freq.QuadPart / 1000000.;
        } else {
            offset = get_FILETIME_offset();
            freq_to_usec = 10.;
        }
    }
    if (use_perf_counter) {
        QueryPerformanceCounter(&t);
    } else {
        GetSystemTimeAsFileTime(&f);
        t.QuadPart = f.dwHighDateTime;
        t.QuadPart <<= 32;
        t.QuadPart |= f.dwLowDateTime;
    }

    t.QuadPart -= offset.QuadPart;
    t_usec = (double) t.QuadPart / freq_to_usec;
    t.QuadPart = t_usec;
    tv->tv_sec = t.QuadPart / 1000000;
    tv->tv_usec = t.QuadPart % 1000000;

    return HG_UTIL_SUCCESS;
}

#elif defined(HG_UTIL_HAS_CLOCK_MONOTONIC)
/*---------------------------------------------------------------------------*/
#    if defined(HG_UTIL_HAS_TIME_H) && defined(HG_UTIL_HAS_CLOCK_GETTIME)
static HG_UTIL_INLINE int
hg_time_get_current(hg_time_t *tv)
{
    struct timespec tp = {0, 0};
    /* NB. CLOCK_MONOTONIC_RAW is not explicitly supported in the vdso */
    clockid_t clock_id = CLOCK_MONOTONIC;

    if (!tv)
        return HG_UTIL_FAIL;

    clock_gettime(clock_id, &tp);
    tv->tv_sec = tp.tv_sec;
    tv->tv_usec = tp.tv_nsec / 1000;

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
#    elif defined(__APPLE__) && defined(HG_UTIL_HAS_SYSTIME_H)
static HG_UTIL_INLINE int
hg_time_get_current(hg_time_t *tv)
{
    static uint64_t monotonic_timebase_factor = 0;
    uint64_t monotonic_nsec;

    if (!tv)
        return HG_UTIL_FAIL;

    if (monotonic_timebase_factor == 0) {
        mach_timebase_info_data_t timebase_info;

        (void) mach_timebase_info(&timebase_info);
        monotonic_timebase_factor = timebase_info.numer / timebase_info.denom;
    }
    monotonic_nsec = (mach_absolute_time() * monotonic_timebase_factor);
    tv->tv_sec = (long) (monotonic_nsec / 1000000000);
    tv->tv_usec = (long) ((monotonic_nsec - (uint64_t) tv->tv_sec) / 1000);

    return HG_UTIL_SUCCESS;
}

#    endif
#else
/*---------------------------------------------------------------------------*/
#    if defined(HG_UTIL_HAS_SYSTIME_H)
static HG_UTIL_INLINE int
hg_time_get_current(hg_time_t *tv)
{
    if (!tv)
        return HG_UTIL_FAIL;

    gettimeofday((struct timeval *) tv, NULL);

    return HG_UTIL_SUCCESS;
}

#    endif
#endif
/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE double
hg_time_to_double(hg_time_t tv)
{
    return (double) tv.tv_sec + (double) (tv.tv_usec) * 0.000001;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_time_t
hg_time_from_double(double d)
{
    hg_time_t tv;

    tv.tv_sec = (long) d;
    tv.tv_usec = (long) ((d - (double) (tv.tv_sec)) * 1000000);

    return tv;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_time_less(hg_time_t in1, hg_time_t in2)
{
    return ((in1.tv_sec < in2.tv_sec) ||
            ((in1.tv_sec == in2.tv_sec) && (in1.tv_usec < in2.tv_usec)));
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_time_t
hg_time_add(hg_time_t in1, hg_time_t in2)
{
    hg_time_t out;

    out.tv_sec = in1.tv_sec + in2.tv_sec;
    out.tv_usec = in1.tv_usec + in2.tv_usec;
    if (out.tv_usec > 1000000) {
        out.tv_usec -= 1000000;
        out.tv_sec += 1;
    }

    return out;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_time_t
hg_time_subtract(hg_time_t in1, hg_time_t in2)
{
    hg_time_t out;

    out.tv_sec = in1.tv_sec - in2.tv_sec;
    out.tv_usec = in1.tv_usec - in2.tv_usec;
    if (out.tv_usec < 0) {
        out.tv_usec += 1000000;
        out.tv_sec -= 1;
    }

    return out;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE int
hg_time_sleep(const hg_time_t rqt)
{
#ifdef _WIN32
    DWORD dwMilliseconds = (DWORD)(hg_time_to_double(rqt) / 1000);

    Sleep(dwMilliseconds);
#elif defined(HG_UTIL_HAS_CLOCK_MONOTONIC)
    struct timespec rqtp;

    rqtp.tv_sec = rqt.tv_sec;
    rqtp.tv_nsec = rqt.tv_usec * 1000;

    if (nanosleep(&rqtp, NULL))
        return HG_UTIL_FAIL;
#else
    useconds_t usec =
        (useconds_t) rqt.tv_sec * 1000000 + (useconds_t) rqt.tv_usec;

    if (usleep(usec))
        return HG_UTIL_FAIL;
#endif

    return HG_UTIL_SUCCESS;
}

/*---------------------------------------------------------------------------*/
#define HG_UTIL_STAMP_MAX 128
static HG_UTIL_INLINE char *
hg_time_stamp(void)
{
    static char buf[HG_UTIL_STAMP_MAX] = {'\0'};

#if defined(_WIN32)
    /* TODO not implemented */
#elif defined(HG_UTIL_HAS_CLOCK_MONOTONIC)
    struct tm *local_time;
    time_t t;

    t = time(NULL);
    local_time = localtime(&t);
    if (local_time == NULL)
        return NULL;

    if (strftime(buf, HG_UTIL_STAMP_MAX, "%a, %d %b %Y %T %Z", local_time) == 0)
        return NULL;
#else
    struct timeval tv;
    struct timezone tz;
    unsigned long days, hours, minutes, seconds;

    gettimeofday(&tv, &tz);
    days = (unsigned long) tv.tv_sec / (3600 * 24);
    hours = ((unsigned long) tv.tv_sec - days * 24 * 3600) / 3600;
    minutes =
        ((unsigned long) tv.tv_sec - days * 24 * 3600 - hours * 3600) / 60;
    seconds = (unsigned long) tv.tv_sec - days * 24 * 3600 - hours * 3600 -
              minutes * 60;
    hours -= (unsigned long) tz.tz_minuteswest / 60;

    snprintf(buf, HG_UTIL_STAMP_MAX, "%02lu:%02lu:%02lu (GMT-%d)", hours,
        minutes, seconds, tz.tz_minuteswest / 60);
#endif

    return buf;
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_TIME_H */
